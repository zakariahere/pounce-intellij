package pounce.completion

import com.intellij.openapi.util.UserDataHolder
import com.intellij.psi.PsiElement
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.impl.{ScalaPsiElementFactory, ScalaPsiManager}
import pounce.core.StaticInstances.Answer
import pounce.core.types.Ty
import pounce.core.{Bounds, StaticInstances, Typeclasses}

import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NonFatal

/**
 * The instance oracle used by completion. Same evaluation order as [[pounce.core.InstanceOracle]] (bounds in scope →
 * static table → the Scala plugin's implicit search), but tuned for the "runs on every `.`" budget:
 *
 *  - implicit-search answers are cached per file until a *top-level* change (Scala plugin's
 *    `TopLevelModificationTracker`), so typing inside method bodies doesn't throw them away. Instances live in
 *    companions, imports and class members: all top-level changes;
 *  - rigid proper types (`A: Monoid`) are answered from bounds too;
 *  - partially applied constructors are rendered as type lambdas, so `Functor[Result[E, *]]` can be searched
 *    without kind-projector.
 */
final class LensOracle(place: PsiElement) {

  private lazy val bounds: Map[String, Set[String]] = Bounds.at(place)

  def has(typeclass: String, t: Ty): Answer = {
    val tc = Typeclasses.canonical(typeclass).getOrElse(typeclass)
    Typeclasses.byName.get(tc).map(_.kind) match {
      case Some(Typeclasses.Kind.Proper) => proper(tc, t)
      case Some(Typeclasses.Kind.Binary) => binary(tc, t)
      case _                             => higherKinded(tc, t)
    }
  }

  /**
   * `F[_, _]` typeclasses (Bifunctor, Bifoldable, ...). The shape binds `F := Either` (no fixed arguments), which the
   * static table only consults for constructors applied to two arguments, so look the binary table up directly.
   */
  private def binary(tc: String, ctor: Ty): Answer = ctor match {
    case Ty.Lambda(name, _, _, true) => rigid(tc, name)
    case Ty.Lambda(name, Nil, fqn, false) =>
      StaticInstances.binary.get(StaticInstances.fqnOf(name, fqn)) match {
        case Some(strongest) => if (Typeclasses.satisfies(strongest, tc)) Answer.Yes else Answer.No
        case None            => search(tc, ctor)
      }
    case _ => higherKinded(tc, ctor)
  }

  /** The bounds of a rigid type parameter, for "why is this applicable" texts. */
  def boundsOf(typeParam: String): Set[String] = bounds.getOrElse(typeParam, Set.empty)

  private def higherKinded(tc: String, ctor: Ty): Answer = ctor match {
    case Ty.Lambda(name, _, _, true) => rigid(tc, name)
    case Ty.Con(name, _, _, true)    => rigid(tc, name)
    case _ =>
      StaticInstances.higherKinded(tc, ctor) match {
        case Answer.Unknown => search(tc, ctor)
        case known          => known
      }
  }

  private def proper(tc: String, t: Ty): Answer = t match {
    case Ty.Con(name, Nil, _, true) => rigid(tc, name)
    case _ =>
      StaticInstances.proper(tc, t) match {
        case Answer.Unknown => search(tc, t)
        case known          => known
      }
  }

  private def rigid(tc: String, typeParam: String): Answer = {
    val bs = bounds.getOrElse(typeParam, Set.empty)
    if (bs.isEmpty) Answer.Unknown
    else if (Typeclasses.satisfies(bs, tc)) Answer.Yes
    else Answer.No
  }

  private def search(tc: String, t: Ty): Answer =
    (Typeclasses.byName.get(tc).map(_.fqn), LensOracle.render(t, place.isInScala3File)) match {
      case (Some(fqnTc), Some(typeText)) => LensOracle.cached(place, s"$fqnTc[$typeText]")(implicitSearch(s"$fqnTc[$typeText]"))
      case _                             => Answer.Unknown
    }

  private def implicitSearch(key: String): Answer =
    try {
      val expr = ScalaPsiElementFactory.createExpressionWithContextFromText(s"_root_.scala.Predef.implicitly[$key]", place, null)
      val ok   = expr.findImplicitArguments.flatMap(_.args).exists(!_.isImplicitParameterProblem)
      if (ok) Answer.Yes else Answer.No
    } catch {
      case NonFatal(_) => Answer.Unknown
    }
}

object LensOracle {

  /** Renders a type (constructor) as Scala source with `_root_` FQNs; `None` when something has no FQN. */
  def render(t: Ty, scala3: Boolean): Option[String] = t match {
    case Ty.Con(_, args, fqn, false) if fqn.nonEmpty =>
      val as = args.map(render(_, scala3))
      if (as.forall(_.isDefined)) Some(if (args.isEmpty) s"_root_.$fqn" else s"_root_.$fqn[${as.flatten.mkString(", ")}]")
      else None
    case Ty.Lambda(_, Nil, fqn, false) if fqn.nonEmpty => Some(s"_root_.$fqn")
    case Ty.Lambda(_, fixed, fqn, false) if fqn.nonEmpty =>
      val fs = fixed.map(render(_, scala3))
      if (!fs.forall(_.isDefined)) None
      else {
        val applied = s"_root_.$fqn[${(fs.flatten :+ "PounceX").mkString(", ")}]"
        Some(if (scala3) s"([PounceX] =>> $applied)" else s"({ type PounceL[PounceX] = $applied })#PounceL")
      }
    case Ty.Tuple(es) =>
      val rs = es.map(render(_, scala3))
      if (rs.forall(_.isDefined)) Some(rs.flatten.mkString("(", ", ", ")")) else None
    case _ => None
  }

  /**
   * Per-file cache of implicit-search answers, dropped on top-level changes only. The provider captures nothing but
   * the project (capturing PSI would leak it).
   */
  private def cached(place: PsiElement, key: String)(compute: => Answer): Answer = {
    val project = place.getProject
    val file    = place.getContainingFile
    val holder: UserDataHolder = if (file != null) file.getOriginalFile else project
    val map = CachedValuesManager.getManager(project).getCachedValue(
      holder,
      () =>
        CachedValueProvider.Result.create(
          new ConcurrentHashMap[String, Answer](),
          ScalaPsiManager.instance(project).TopLevelModificationTracker
        )
    )
    val existing = map.get(key)
    if (existing != null) existing
    else {
      val v = compute
      map.put(key, v)
      v
    }
  }
}
