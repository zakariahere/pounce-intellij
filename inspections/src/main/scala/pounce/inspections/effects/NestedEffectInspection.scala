package pounce.inspections.effects

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{rightRangeInParent, stripped}
import org.jetbrains.plugins.scala.codeInspection.expressionResultIsNotUsed
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType
import pounce.inspections.util.Patterns.*
import pounce.inspections.util.Typing
import pounce.inspections.{PounceInspection, ReplaceExpressionFix}

/**
 * B2. Effects nested by `map`, so the inner effect never runs:
 *  - `io.map(x => IO.println(x))`: `IO[IO[Unit]]` → `flatMap`
 *  - `opt.map(save)` whose value is thrown away / expected as an effect: `Option[IO[Unit]]` → `traverse`
 *  - `xs.foreach(x => IO.println(x))`: `foreach` discards the effects → `traverse_`
 */
final class NestedEffectInspection extends PounceInspection {

  import NestedEffectInspection.*

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case expr @ qual `.map` f if !isFlattenedRightAway(expr) =>
      Typing.typeOf(expr).foreach { t =>
        if (isNestedSameEffect(t, expr) && !expectsNested(expr)) {
          val text = invocation(qual, "flatMap", f)
          Typing.checkReplacement(expr, text, Some("flatMap"), sameType = false).foreach { imp =>
            report(holder, expr, nestedMessage(Typing.show(t, expr)), Seq(new ReplaceExpressionFix(flatMapHint, expr, text, imp)), rightRangeInParent(qual, expr))
          }
        } else if (isTraversableOfEffect(qual, t, expr) && (expressionResultIsNotUsed(expr) || expectsEffect(expr))) {
          val text = invocation(qual, "traverse", f)
          val fix  = Typing.checkReplacement(expr, text, Some("traverse"), sameType = false).map(imp => new ReplaceExpressionFix(traverseHint, expr, text, imp))
          report(holder, expr, traverseMessage(Typing.show(t, expr)), fix.toSeq, rightRangeInParent(qual, expr))
        }
      }
    case expr @ qual `.foreach` f if returnsEffect(f) =>
      val text = invocation(qual, "traverse_", f)
      val fix  = Typing.checkReplacement(expr, text, Some("foldable"), sameType = false).map(imp => new ReplaceExpressionFix(traverseUnderscoreHint, expr, text, imp))
      report(holder, expr, foreachMessage, fix.toSeq, rightRangeInParent(qual, expr))
    case _ =>
  }

  private def invocation(qual: ScExpression, name: String, f: ScExpression): String =
    org.jetbrains.plugins.scala.codeInspection.collections.invocationText(qual, name, f)

  /** `IO[IO[A]]` / `F[F[A]]`: outer and inner have the same effect constructor. */
  private def isNestedSameEffect(t: ScType, place: PsiElement): Boolean =
    Typing.isEffect(t, place) && Typing.lastTypeArg(t, place).exists { inner =>
      Typing.isEffect(inner, place) && Typing.headKey(inner, place) == Typing.headKey(t, place)
    }

  /** `.map(f).flatten` is the simplification inspection's business. */
  private def isFlattenedRightAway(expr: ScExpression): Boolean = expr.getParent match {
    case r: ScReferenceExpression => r.refName == "flatten" && r.qualifier.contains(expr)
    case _                        => false
  }

  private def expectsNested(expr: ScExpression): Boolean =
    expr.expectedType().exists(et => isNestedSameEffect(et, expr))

  private def expectsEffect(expr: ScExpression): Boolean =
    expr.expectedType().exists(et => Typing.isEffect(et, expr))

  private def isTraversableOfEffect(qual: ScExpression, t: ScType, place: PsiElement): Boolean =
    Typing.headFqn(t, place).exists(traversables.contains) &&
      Typing.typeOf(qual).flatMap(Typing.headFqn(_, place)).exists(traversables.contains) &&
      Typing.lastTypeArg(t, place).exists(Typing.isEffect(_, place))

  private def returnsEffect(f: ScExpression): Boolean = stripped(f) match {
    case Lambda1(_, body) => Typing.isEffect(body)
    case other =>
      Typing.typeOf(other).exists {
        case FunctionType(ret, _) => Typing.isEffect(ret, other)
        case _                    => false
      }
  }
}

object NestedEffectInspection {

  /** Std / cats structures with a `Traverse` instance whose `map` result shouldn't hold effects. */
  val traversables: Set[String] = Set(
    "scala.Option",
    "scala.Some",
    "scala.collection.immutable.List",
    "scala.collection.immutable.Vector",
    "scala.collection.immutable.Seq",
    "scala.collection.Seq",
    "scala.util.Either",
    "cats.data.NonEmptyList",
    "cats.data.NonEmptyVector",
    "cats.data.Chain",
    "cats.data.NonEmptyChainImpl",
    "cats.data.Validated"
  )

  def nestedMessage(tpe: String): String =
    s"Nested effect: map produces $tpe, so the inner effect never runs"

  def traverseMessage(tpe: String): String =
    s"Effects inside $tpe are never run; did you mean traverse?"

  val foreachMessage = "foreach discards the effects returned by the function, so they never run"

  val flatMapHint            = "Replace map with flatMap"
  val traverseHint           = "Replace map with traverse"
  val traverseUnderscoreHint = "Replace foreach with traverse_"
}
