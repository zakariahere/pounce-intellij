package pounce.core.catalog

import com.intellij.openapi.diagnostic.Logger
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigSyntax}
import pounce.core.{Lib, Typeclasses}
import pounce.core.types.Ty

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** How an operation is used, which decides where it shows up. */
enum OpKind {

  /** Extension method from a syntax import: `xs.traverse(f)`. Needs `import cats.syntax.all._`. */
  case Syntax

  /** Extension method on *any* value: `a.some`, `a.asRight[E]`, `a.pure[F]`. Needs a syntax import. */
  case Lift

  /** Extension method on a tuple of effects: `(fa, fb).mapN(f)`. */
  case Tuple

  /** A method defined on the type itself (`IO#attempt`), shown in docs, explorer and finder only. */
  case Method

  /** A method on a companion / typeclass summoner: `IO.blocking`, `Ref.of`, `Sync[F].delay`. */
  case Companion
}

/**
 * One catalog entry: everything Pounce knows about a Cats / Cats Effect / fs2 operation.
 *
 * @param receiver shape of the receiver (`F[A]`) for Syntax/Method; the companion name for Companion; `A` for Lift
 * @param requires typeclass constraints on shape variables: `F -> Traverse`, `G -> Applicative`
 * @param syntax   syntax module the extension comes from: `traverse` (→ `cats.syntax.traverse._`) or
 *                 `effect.spawn` (→ `cats.effect.syntax.spawn._`); empty for methods / companions
 * @param insert   text to insert after the dot; `|` marks the caret (default: `name(|)` or `name` when no params)
 */
final case class Op(
  id: String,
  name: String,
  owner: String,
  kind: OpKind,
  receiver: Option[Ty],
  params: List[Ty],
  result: Option[Ty],
  requires: List[(String, String)],
  syntax: Option[String],
  library: Lib,
  rank: Int,
  doc: String,
  example: String,
  seeAlso: List[String],
  insert: Option[String],
  tags: Set[String],
  since: Option[String]
) {

  /** Shown in completion tail text and quick doc. */
  def signature: String = {
    val recv   = receiver.map(_.show).getOrElse("")
    val ps     = if (params.isEmpty) "" else params.map(_.show).mkString("(", ", ", ")")
    val res    = result.map(r => s": ${r.show}").getOrElse("")
    kind match {
      case OpKind.Companion => s"$owner.$name$ps$res"
      case _                => s"$recv.$name$ps$res"
    }
  }

  def constraintsText: String = requires.map { case (v, tc) => s"$tc[$v]" }.mkString(", ")

  /**
   * Text inserted by completion and caret offset inside it.
   *
   * Only an explicit `insert` template uses a `|` caret marker (the last `|`, so operator names such as `|+|` in the
   * template survive). Default templates compute the caret directly: an operator name like `|+|` must never be
   * mistaken for a marker.
   */
  def insertTextAndCaret: (String, Int) = insert match {
    case Some(raw) =>
      val i = raw.lastIndexOf('|')
      if (i < 0) (raw, raw.length) else (raw.substring(0, i) + raw.substring(i + 1), i)
    case None if params.isEmpty => (name, name.length)
    case None                   => (s"$name()", name.length + 1)
  }

  /** Function-type view used by the Combinator Finder: inputs (receiver first) and output. */
  def asFunction: Option[(List[Ty], Ty)] =
    result.map { r =>
      val inputs = kind match {
        case OpKind.Companion => params
        case _                => receiver.toList ++ params
      }
      (inputs, r)
    }

  def isExtension: Boolean = kind == OpKind.Syntax || kind == OpKind.Lift || kind == OpKind.Tuple
}

final case class Catalog(ops: Vector[Op]) {

  lazy val byName: Map[String, Vector[Op]] = ops.groupBy(_.name)

  lazy val byId: Map[String, Op] = ops.map(o => o.id -> o).toMap

  lazy val extensions: Vector[Op] = ops.filter(_.isExtension)

  lazy val byOwner: Map[String, Vector[Op]] = ops.groupBy(_.owner)

  def find(name: String, owner: String): Option[Op] = byName.getOrElse(name, Vector.empty).find(_.owner == owner)

  def ++(other: Catalog): Catalog = Catalog(ops ++ other.ops)
}

object Catalog {

  private val Log = Logger.getInstance(classOf[Catalog])

  val IndexResource = "pounce/catalog/index.conf"

  /** The bundled catalog, loaded once. */
  lazy val bundled: Catalog = loadBundled(getClass.getClassLoader)

  def loadBundled(cl: ClassLoader): Catalog = {
    val index = ConfigFactory.parseResources(cl, IndexResource, ConfigParseOptions.defaults().setAllowMissing(false))
    val files = index.getStringList("files").asScala.toList
    val parts = files.map { f =>
      try fromConfig(ConfigFactory.parseResources(cl, s"pounce/catalog/$f", ConfigParseOptions.defaults().setAllowMissing(false)), f)
      catch {
        case NonFatal(e) =>
          Log.warn(s"Pounce: failed to load catalog file $f", e)
          Catalog(Vector.empty)
      }
    }
    parts.foldLeft(Catalog(Vector.empty))(_ ++ _)
  }

  def fromString(hocon: String, origin: String = "inline"): Catalog =
    fromConfig(ConfigFactory.parseString(hocon, ConfigParseOptions.defaults().setSyntax(ConfigSyntax.CONF)), origin)

  def fromConfig(config: Config, origin: String): Catalog = {
    val defaultLib  = if (config.hasPath("library")) parseLib(config.getString("library")) else Lib.CatsCore
    val defaultKind = if (config.hasPath("kind")) parseKind(config.getString("kind")) else OpKind.Syntax
    val defaultOwner = if (config.hasPath("owner")) Some(config.getString("owner")) else None
    val defaultSyntax = if (config.hasPath("syntax")) Some(config.getString("syntax")) else None
    val ops = config.getConfigList("ops").asScala.toVector.zipWithIndex.flatMap { case (c, i) =>
      try Some(parseOp(c, defaultLib, defaultKind, defaultOwner, defaultSyntax))
      catch {
        case NonFatal(e) =>
          Log.warn(s"Pounce: skipping catalog entry #$i in $origin: ${e.getMessage}")
          None
      }
    }
    Catalog(ops)
  }

  private def str(c: Config, path: String): Option[String] = if (c.hasPath(path)) Some(c.getString(path)) else None

  private def strs(c: Config, path: String): List[String] =
    if (!c.hasPath(path)) Nil
    else
      c.getValue(path).unwrapped() match {
        case s: String              => List(s)
        case l: java.util.List[?]   => l.asScala.map(_.toString).toList
        case other                  => List(other.toString)
      }

  private def ty(s: String): Ty = Ty.parse(s).fold(e => throw new IllegalArgumentException(s"bad type '$s': ${e.msg}"), identity)

  private def parseOp(c: Config, defaultLib: Lib, defaultKind: OpKind, defaultOwner: Option[String], defaultSyntax: Option[String]): Op = {
    val name  = c.getString("name")
    val owner = str(c, "owner").orElse(defaultOwner).getOrElse(throw new IllegalArgumentException(s"$name: missing owner"))
    val kind  = str(c, "kind").map(parseKind).getOrElse(defaultKind)
    val variant  = str(c, "variant")
    val receiver = str(c, "receiver").map(ty)
    val params   = strs(c, "params").map(ty)
    val result   = str(c, "result").map(ty)
    val explicitRequires =
      if (!c.hasPath("requires")) Nil
      else
        c.getConfig("requires").entrySet().asScala.toList.map(e => e.getKey -> e.getValue.unwrapped().toString)
    // A typeclass summoner (`Spawn[F].start(fa)`) only exists when F has the owner's instance. Entries don't spell
    // that out, so add `F = owner` here; without it the finder would offer `Spawn.start(xs)` for a List.
    val shapeVars = (receiver.toList ++ params ++ result.toList).flatMap(_.vars).toSet
    val implied =
      Option.when(
        variant.contains("summoner") && shapeVars("F") && !explicitRequires.exists(_._1 == "F") &&
          Typeclasses.canonical(owner).isDefined
      )("F" -> owner)
    val requires = (explicitRequires ++ implied).sortBy(_._1)
    val syntax =
      if (kind == OpKind.Method || kind == OpKind.Companion) str(c, "syntax")
      else str(c, "syntax").orElse(defaultSyntax)
    Op(
      id = str(c, "id").getOrElse(s"$owner.$name${str(c, "variant").map("#" + _).getOrElse("")}"),
      name = name,
      owner = owner,
      kind = kind,
      receiver = receiver,
      params = params,
      result = result,
      requires = requires,
      syntax = syntax.filter(_.nonEmpty),
      library = str(c, "library").map(parseLib).getOrElse(defaultLib),
      rank = if (c.hasPath("rank")) c.getInt("rank") else 50,
      doc = str(c, "doc").getOrElse(""),
      example = str(c, "example").map(_.stripTrailing()).getOrElse(""),
      seeAlso = strs(c, "see-also"),
      insert = str(c, "insert"),
      tags = strs(c, "tags").toSet,
      since = str(c, "since")
    )
  }

  def parseKind(s: String): OpKind = s.toLowerCase match {
    case "syntax"    => OpKind.Syntax
    case "lift"      => OpKind.Lift
    case "tuple"     => OpKind.Tuple
    case "method"    => OpKind.Method
    case "companion" => OpKind.Companion
    case other       => throw new IllegalArgumentException(s"unknown kind '$other'")
  }

  def parseLib(s: String): Lib = s.toLowerCase match {
    case "cats" | "cats-core"     => Lib.CatsCore
    case "cats-effect" | "ce"     => Lib.CatsEffect
    case "fs2"                    => Lib.Fs2
    case "http4s"                 => Lib.Http4s
    case "doobie"                 => Lib.Doobie
    case "circe"                  => Lib.Circe
    case "play"                   => Lib.Play
    case other                    => Lib.values.find(_.displayName == other).getOrElse(throw new IllegalArgumentException(s"unknown library '$other'"))
  }
}
