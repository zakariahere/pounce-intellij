package pounce.core

import com.intellij.psi.{JavaPsiFacade, PsiElement}
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, PsiModificationTracker}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import pounce.core.StaticInstances.Answer
import pounce.core.types.Ty

import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NonFatal

/**
 * Answers "does typeclass TC have an instance for this type (constructor) here?".
 *
 * Order of evaluation, cheapest first:
 *  1. rigid type parameters (`F` in tagless code): bounds in scope + hierarchy ([[Bounds]], [[Typeclasses]]);
 *  2. the static table for well-known types ([[StaticInstances]]);
 *  3. the Scala plugin's implicit search on a synthetic `implicitly[TC[X]]`, cached per file until PSI changes.
 */
final class InstanceOracle(place: PsiElement) {

  private lazy val bounds: Map[String, Set[String]] = Bounds.at(place)

  def has(typeclass: String, t: Ty): Answer = {
    val tc   = Typeclasses.canonical(typeclass).getOrElse(typeclass)
    val kind = Typeclasses.byName.get(tc).map(_.kind).getOrElse(Typeclasses.Kind.HigherKinded)
    kind match {
      case Typeclasses.Kind.Proper => proper(tc, t)
      case _                       => higherKinded(tc, t)
    }
  }

  private def higherKinded(tc: String, ctor: Ty): Answer = ctor match {
    case Ty.Lambda(name, _, _, true) => rigid(tc, name)
    case Ty.Con(name, _, _, true)    => rigid(tc, name)
    case _ =>
      StaticInstances.higherKinded(tc, ctor) match {
        case Answer.Unknown => implicitSearch(tc, ctor)
        case known          => known
      }
  }

  private def rigid(tc: String, typeParam: String): Answer = {
    val bs = bounds.getOrElse(typeParam, Set.empty)
    if (bs.isEmpty) Answer.Unknown
    else if (Typeclasses.satisfies(bs, tc)) Answer.Yes
    else Answer.No
  }

  private def proper(tc: String, t: Ty): Answer =
    StaticInstances.proper(tc, t) match {
      case Answer.Unknown => implicitSearch(tc, t)
      case known          => known
    }

  /** Runs `implicitly[TC[X]]` through the Scala plugin. Only for types we can render back as Scala source. */
  private def implicitSearch(tc: String, t: Ty): Answer =
    (Typeclasses.byName.get(tc).map(_.fqn), renderable(t)) match {
      // An unresolvable typeclass (e.g. cats-kernel missing from the classpath) makes `implicitly[...]` meaningless:
      // the Scala plugin then reports no implicit problem at all, which would read as a false "Yes".
      case (Some(fqnTc), Some(typeText)) if classExists(fqnTc) => implicitSearch(s"$fqnTc[$typeText]")
      case _                                                    => Answer.Unknown
    }

  private def classExists(fqn: String): Boolean =
    try JavaPsiFacade.getInstance(place.getProject).findClass(fqn, place.getResolveScope) != null
    catch { case NonFatal(_) => false }

  private def implicitSearch(key: String): Answer =
    InstanceOracle.cached(place, key) {
      try {
        val expr = ScalaPsiElementFactory.createExpressionWithContextFromText(s"_root_.scala.Predef.implicitly[$key]", place, null)
        val ok   = expr.findImplicitArguments.flatMap(_.args).exists(!_.isImplicitParameterProblem)
        if (ok) Answer.Yes else Answer.No
      } catch {
        case NonFatal(_) => Answer.Unknown
      }
    }

  /** Renders a type (constructor) back to Scala source using FQNs, `({ type L[x] = ... })#L` avoided via `*`. */
  private def renderable(t: Ty): Option[String] = t match {
    case Ty.Con(_, args, fqn, false) if fqn.nonEmpty =>
      val as = args.map(renderable)
      if (as.forall(_.isDefined)) Some(if (args.isEmpty) s"_root_.$fqn" else s"_root_.$fqn[${as.flatten.mkString(", ")}]") else None
    case Ty.Lambda(_, fixed, fqn, false) if fqn.nonEmpty && fixed.isEmpty => Some(s"_root_.$fqn")
    case _ => None // partially applied constructors need kind-projector; don't guess
  }
}

object InstanceOracle {

  def apply(place: PsiElement): InstanceOracle = new InstanceOracle(place)

  /**
   * Per-file cache (implicit scope depends on the file's imports), dropped on any PSI change.
   *
   * The provider lambda must capture nothing but the project: capturing `place` retains PSI past its lifetime,
   * which the platform reports as a leak (caught by OracleFixtureTest).
   */
  private def cached(place: PsiElement, key: String)(compute: => Answer): Answer = {
    val project = place.getProject
    val file    = place.getContainingFile
    val holder: com.intellij.openapi.util.UserDataHolder = if (file != null) file.getOriginalFile else project
    val map = CachedValuesManager.getManager(project).getCachedValue(
      holder,
      () => CachedValueProvider.Result.create(new ConcurrentHashMap[String, Answer](), PsiModificationTracker.getInstance(project))
    )
    val existing = map.get(key)
    if (existing != null) existing
    else {
      val v = compute
      map.put(key, v)
      v
    }
  }

  /** Convenience for expressions. */
  def forExpr(expr: ScExpression): InstanceOracle = new InstanceOracle(expr)
}
