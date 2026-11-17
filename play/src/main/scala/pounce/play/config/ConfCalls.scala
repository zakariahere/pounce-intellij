package pounce.play.config

import com.intellij.psi.{PsiClass, PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import pounce.core.settings.PounceSettings
import pounce.core.{Lib, Libraries}
import pounce.play.util.PlayPsi
import pounce.play.util.PlayPsi.LiteralCall

/** Where a config path is relative to. */
enum ConfPrefix {

  /** The root of the application config (`ConfigFactory.load()`, an injected `Configuration`). */
  case Root

  /** Relative to a known path (`config.getConfig("app.db").getString("url")`). */
  case Known(path: String)

  /** Relative to something we can't see (a method parameter `cfg: Config`): no "unknown key" warnings. */
  case Unknown

  def resolve(key: String): Option[String] = this match {
    case Root        => Some(key)
    case Known(p)    => Some(s"$p.$key")
    case Unknown     => None
  }

  def child(key: String): ConfPrefix = this match {
    case Root     => Known(key)
    case Known(p) => Known(s"$p.$key")
    case Unknown  => Unknown
  }
}

/**
 * A string literal used as a config path.
 *
 * @param optional the read tolerates a missing key (`getOptional`, `has`, `hasPath`, `get[Option[T]]`, `getDeprecated`)
 */
final case class ConfKeyUse(literal: ScStringLiteral, call: LiteralCall, prefix: ConfPrefix, optional: Boolean) {
  def key: String                 = literal.getValue
  def absolutePath: Option[String] = prefix.resolve(key)
}

/** Recognised config-reading call patterns. */
object ConfCalls {

  val PlayConfiguration = "play.api.Configuration"
  val TypesafeConfig    = "com.typesafe.config.Config"
  val Pureconfig        = Set("pureconfig.ConfigSource", "pureconfig.ConfigObjectSource")

  /** `play.api.Configuration` methods whose first argument is a path. */
  val PlayMethods: Set[String] =
    Set("get", "getOptional", "has", "getDeprecated", "getMillis", "getNanos", "getAndValidate", "getPrototypedSeq",
      "getPrototypedMap")
  val PlayOptional: Set[String] = Set("getOptional", "has", "getDeprecated")

  /** `com.typesafe.config.Config` methods whose first argument is a path. */
  val ConfigMethods: Set[String] = Set(
    "hasPath", "hasPathOrNull", "getIsNull", "getBoolean", "getNumber", "getInt", "getLong", "getDouble", "getString",
    "getObject", "getConfig", "getAnyRef", "getValue", "getBytes", "getMemorySize", "getMilliseconds",
    "getNanoseconds", "getDuration", "getPeriod", "getTemporal", "getList", "getBooleanList", "getNumberList",
    "getIntList", "getLongList", "getDoubleList", "getStringList", "getObjectList", "getConfigList", "getAnyRefList",
    "getBytesList", "getMemorySizeList", "getMillisecondsList", "getNanosecondsList", "getDurationList", "withOnlyPath",
    "withoutPath"
  )

  /** Methods taking the path as their second argument (`getEnum(classOf[E], "path")`). */
  val ConfigMethodsSecondArg: Set[String] = Set("getEnum", "getEnumList")
  val ConfigOptional: Set[String]         = Set("hasPath", "hasPathOrNull", "withOnlyPath", "withoutPath")

  val PureconfigMethods: Set[String] = Set("at")

  def enabled(e: PsiElement): Boolean =
    PounceSettings().playConfigKeys && {
      val libs = Libraries.of(e)
      libs.contains(Lib.Play) || libs.contains(Lib.TypesafeConfig) || libs.contains(Lib.Pureconfig)
    }

  /** The literal at a recognised call position, or None. */
  def use(lit: ScStringLiteral): Option[ConfKeyUse] =
    PlayPsi.literalCall(lit).flatMap { c =>
      kind(c).map(optional => ConfKeyUse(lit, c, prefixOfReceiver(c.receiver, depth = 0), optional))
    }

  /** Some(optional) when the call is a config read with the literal in path position. */
  private def kind(c: LiteralCall): Option[Boolean] = {
    val name  = c.name
    val owner = PlayPsi.fqn(c.owner)
    if (PlayMethods.contains(name) && c.argIndex == 0 && isPlayConfiguration(c.owner, owner))
      Some(PlayOptional.contains(name) || readsOption(c.typeArgs))
    else if (ConfigMethods.contains(name) && c.argIndex == 0 && isTypesafeConfig(c.owner, owner))
      Some(ConfigOptional.contains(name))
    else if (ConfigMethodsSecondArg.contains(name) && c.argIndex == 1 && isTypesafeConfig(c.owner, owner))
      Some(false)
    else if (PureconfigMethods.contains(name) && c.argIndex == 0 && PlayPsi.isOwner(c.owner, Pureconfig))
      Some(false)
    else None
  }

  private def isPlayConfiguration(cls: PsiClass, fqn: String): Boolean = fqn == PlayConfiguration
  private def isTypesafeConfig(cls: PsiClass, fqn: String): Boolean =
    fqn == TypesafeConfig || PlayPsi.isOwner(cls, Set(TypesafeConfig))

  /** `config.get[Option[String]]("k")`: Play's Option loader returns None for a missing key. */
  private def readsOption(typeArgs: Seq[ScTypeElement]): Boolean =
    typeArgs.headOption.exists(t => t.getText.trim.startsWith("Option[") || t.getText.trim.startsWith("scala.Option["))

  // ---- receiver → path prefix -------------------------------------------------------------------------------------

  private val RootFactories = Set("load", "defaultApplication", "defaultReference", "defaultOverrides")

  def prefixOfReceiver(receiver: Option[ScExpression], depth: Int): ConfPrefix =
    receiver match {
      case None       => ConfPrefix.Unknown
      case Some(expr) => prefixOf(expr, depth)
    }

  private def prefixOf(expr: ScExpression, depth: Int): ConfPrefix =
    if (depth > 4) ConfPrefix.Unknown
    else
      expr match {
        case p: ScParenthesisedExpr => p.innerElement.map(prefixOf(_, depth + 1)).getOrElse(ConfPrefix.Unknown)

        // config.getConfig("a"), config.get[Configuration]("a"), ConfigSource.default.at("a")
        case call: ScMethodCall =>
          val nested = call.args.exprs match {
            case Seq(lit: ScStringLiteral) => PlayPsi.plainLiteral(lit).flatMap(use)
            case _                         => None
          }
          nested match {
            case Some(u) if isSubConfig(u) => u.prefix.child(u.key)
            case Some(_)                   => ConfPrefix.Unknown
            case None                      => factoryPrefix(call)
          }

        case ref: ScReferenceExpression =>
          ref.refName match {
            // configuration.underlying, ConfigSource.default
            case "underlying" => prefixOfReceiver(ref.qualifier, depth + 1)
            case "default" if ref.qualifier.exists(_.getText.endsWith("ConfigSource")) => ConfPrefix.Root
            case _ =>
              PlayPsi.safe(ref.resolve()).map(prefixOfDefinition(_, depth)).getOrElse(ConfPrefix.Unknown)
          }

        case _ => ConfPrefix.Unknown
      }

  private def isSubConfig(u: ConfKeyUse): Boolean = u.call.name match {
    case "getConfig" | "at" => true
    case "get" =>
      u.call.typeArgs.headOption.exists { t =>
        val text = t.getText.trim
        text == "Configuration" || text == "Config" || text.endsWith(".Configuration") || text.endsWith(".Config")
      }
    case _ => false
  }

  /** `ConfigFactory.load()`, `Configuration.load(env)`, `Configuration(ConfigFactory.load())`. */
  private def factoryPrefix(call: ScMethodCall): ConfPrefix =
    PlayPsi.resolveMethod(call) match {
      case Some(m) if RootFactories.contains(m.getName) && Option(m.getContainingClass).exists { c =>
            val fqn = PlayPsi.fqn(c)
            fqn == "com.typesafe.config.ConfigFactory" || fqn == PlayConfiguration
          } =>
        ConfPrefix.Root
      case Some(m) if m.getName == "apply" && Option(m.getContainingClass).exists(c => PlayPsi.fqn(c) == PlayConfiguration) =>
        call.args.exprs.headOption.map(prefixOf(_, 1)).getOrElse(ConfPrefix.Unknown)
      case Some(m) if m.getName == "fromConfig" && Option(m.getContainingClass).exists(c => PlayPsi.fqn(c).startsWith("pureconfig.")) =>
        call.args.exprs.headOption.map(prefixOf(_, 1)).getOrElse(ConfPrefix.Unknown)
      case _ => ConfPrefix.Unknown
    }

  private def prefixOfDefinition(target: PsiElement, depth: Int): ConfPrefix = target match {
    // class UserController @Inject() (config: Configuration): Guice injects the root configuration
    case p: ScClassParameter =>
      p.containingClass match {
        case cls: ScClass if hasInjectConstructor(cls) => ConfPrefix.Root
        case _                                         => ConfPrefix.Unknown
      }
    // val dbConfig = config.getConfig("db")
    case pattern: ScReferencePattern =>
      pattern.getParent.getParent match {
        case definition: ScPatternDefinition if definition.pList.patterns.size == 1 =>
          definition.expr.map(prefixOf(_, depth + 1)).getOrElse(ConfPrefix.Unknown)
        case _ => ConfPrefix.Unknown
      }
    // def config = ConfigFactory.load()
    case f: ScFunctionDefinition if f.parameters.isEmpty =>
      f.body.map(prefixOf(_, depth + 1)).getOrElse(ConfPrefix.Unknown)
    // app.configuration, components.configuration (play.api.Application, BuiltInComponents): the root config
    case m: PsiMethod if m.getName == "configuration" && Option(m.getContainingClass).exists(c => PlayPsi.fqn(c).startsWith("play.api.")) =>
      ConfPrefix.Root
    case _ => ConfPrefix.Unknown
  }

  def hasInjectConstructor(cls: ScClass): Boolean =
    cls.constructor.exists(c => c.annotations.exists(a => isInjectAnnotation(a.typeElement.getText)))

  def isInjectAnnotation(text: String): Boolean = {
    val t = text.trim
    t == "Inject" || t.endsWith(".Inject")
  }
}
