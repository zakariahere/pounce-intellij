package pounce.inspections.effects

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.psi.PsiElement
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.jetbrains.plugins.scala.codeInspection.collections.{isIndependentOf, stripped}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.core.Lib
import pounce.inspections.util.Patterns.*
import pounce.inspections.util.Typing
import pounce.inspections.{PounceInspection, ReplaceExpressionFix}

/**
 * B6. `.start` whose fiber is thrown away: nobody joins or cancels it, so it leaks when the rest of the program
 * fails or is cancelled. Flagged: `fa.start >> fb`, `fa.start *> fb`, `fa.start.void`, `fa.start.as(x)`,
 * `fa.start.flatMap(_ => fb)` and for-comprehension generators `_ <- fa.start` / unused `fib <- fa.start`.
 * A discarded `fa.start` statement is left to the discarded-effect inspection.
 */
final class FiberLeakInspection extends PounceInspection {

  import FiberLeakInspection.*

  override protected def requiredLibraries: Set[Lib] = Set(Lib.CatsEffect)

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case start @ `.start`(fa) if isSpawnStart(start) =>
      leak(start, fa).foreach { case (fix) => report(holder, start, message, fix.toSeq) }
    case _ =>
  }

  private def isSpawnStart(e: ScExpression): Boolean = e match {
    case r: ScReferenceExpression => Typing.resolvedOwner(r).exists(_.startsWith("cats.effect"))
    case _                        => false
  }

  /** Some(optional fix) when the fiber produced by `start` is discarded. */
  private def leak(start: ScExpression, fa: ScExpression): Option[Option[LocalQuickFix]] = start.getParent match {
    // fa.start >> fb / fa.start *> fb
    case infix @ ScInfixExpr(left, op, right) if (left eq start) && sequencing.contains(op.refName) =>
      Some(backgroundFix(infix, fa, right))
    // fa.start.void / fa.start.as(x)
    case r: ScReferenceExpression if r.qualifier.contains(start) && r.refName == "void" => Some(None)
    case r: ScReferenceExpression if r.qualifier.contains(start) && (r.refName == "as" || sequencing.contains(r.refName)) =>
      r.getParent match {
        case call: ScMethodCall if sequencing.contains(r.refName) =>
          call.argumentExpressions match {
            case Seq(fb) => Some(backgroundFix(call, fa, fb))
            case _       => Some(None)
          }
        case _ => Some(None)
      }
    // fa.start.flatMap(_ => fb)
    case r: ScReferenceExpression if r.qualifier.contains(start) && r.refName == "flatMap" =>
      r.getParent match {
        case call: ScMethodCall =>
          call.argumentExpressions match {
            case Seq(ConstFn(fb)) => Some(backgroundFix(call, fa, fb))
            case _                => None
          }
        case _ => None
      }
    // _ <- fa.start / fib <- fa.start (fib unused)
    case g: ScGenerator if g.expr.contains(start) =>
      g.pattern match {
        case _: ScWildcardPattern => Some(None)
        case p: ScReferencePattern if ReferencesSearch.search(p, new LocalSearchScope(g.getContainingFile)).findFirst() == null => Some(None)
        case _ => None
      }
    case _ => None
  }

  /** `fa.start >> fb` → `fa.background.surround(fb)`: the fiber is cancelled when `fb` completes. */
  private def backgroundFix(whole: ScExpression, fa: ScExpression, fb: ScExpression): Option[LocalQuickFix] = {
    val text = s"${Typing.operand(fa)}.background.surround(${fb.getText})"
    Typing.checkReplacement(whole, text, Some("effect.spawn"), sameType = false).map(imp => new ReplaceExpressionFix(backgroundHint, whole, text, imp))
  }
}

object FiberLeakInspection {
  val sequencing: Set[String] = Set(">>", "*>", "productR")

  val message = "Fiber started with .start is never joined or cancelled; it leaks if the rest fails or is cancelled (prefer .background or a Supervisor)"

  val backgroundHint = "Use .background (cancel the fiber when the rest completes)"
}
