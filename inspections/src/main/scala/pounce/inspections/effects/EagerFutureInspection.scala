package pounce.inspections.effects

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.jetbrains.plugins.scala.codeInspection.collections.stripped
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import pounce.core.Lib
import pounce.inspections.util.Patterns.*
import pounce.inspections.util.Typing
import pounce.inspections.{EditFix, PounceInspection, ReplaceExpressionFix, Rewriter}

import scala.util.control.NonFatal

/**
 * B5. A `Future` that already runs before the effect is built:
 *  - `IO.fromFuture(IO.pure(fut))`: `pure` evaluates its argument eagerly → `IO.fromFuture(IO(fut))`;
 *  - `IO.fromFuture(IO(fut))` where `fut` is a val / parameter created outside: already running, wrapping it in IO
 *    can't make it lazy → move the construction inside (local vals) or pass a `=> Future`;
 *  - `IO(Future(...))`: an `IO[Future[A]]` whose result and errors are ignored → `IO.fromFuture(IO(Future(...)))`.
 */
final class EagerFutureInspection extends PounceInspection {

  import EagerFutureInspection.*

  override protected def requiredLibraries: Set[Lib] = Set(Lib.CatsEffect)

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case call @ (q `.fromFuture` arg) if isFromFuture(call) =>
      stripped(arg) match {
        case Pure(r: ScReferenceExpression, _) if isAlreadyRunning(r) =>
          report(holder, r, alreadyRunningMessage(r.refName))
        case p @ Pure(fut, pureQual) =>
          val fix = suspendedText(fut, pureQual).flatMap { text =>
            Typing.checkReplacement(p, text, None, sameType = false).map(_ => new ReplaceExpressionFix(suspendHint, p, text))
          }
          report(holder, p, pureMessage, fix.toSeq)
        case d @ Delay(_, body) =>
          stripped(body) match {
            case r: ScReferenceExpression if isAlreadyRunning(r) =>
              report(holder, r, alreadyRunningMessage(r.refName), moveInsideFix(r).toSeq)
            case _ =>
          }
        case _ =>
      }
    case d @ Delay(call, body) if returnsFuture(call) && !isFromFutureArgument(call) && !expectsFutureInEffect(call) =>
      val text = s"IO.fromFuture(${call.getText})"
      val fix  = if (isIoCall(call)) Typing.checkReplacement(call, text, None, sameType = false).map(_ => new ReplaceExpressionFix(wrapHint, call, text)) else None
      report(holder, call, ignoredFutureMessage, fix.toSeq)
    case _ =>
  }

  private def isFromFuture(call: ScExpression): Boolean = call match {
    case c: ScMethodCall => invokedRef(c).exists(r => Typing.resolvedOwner(r).exists(o => o == "cats.effect.IO" || o.startsWith("cats.effect.kernel.Async")))
    case _               => false
  }

  private def isFromFutureArgument(call: ScExpression): Boolean = call.getParent match {
    case args: ScArgumentExprList =>
      args.getParent match {
        case outer: ScMethodCall => isFromFuture(outer)
        case _                   => false
      }
    case _ => false
  }

  private def isIoCall(call: ScMethodCall): Boolean = invokedRef(call).exists { r =>
    r.refName == "IO" || r.qualifier.exists(q => Typing.objectFqn(q).contains("cats.effect.IO"))
  }

  private def returnsFuture(call: ScMethodCall): Boolean =
    Typing.typeOf(call).flatMap(Typing.lastTypeArg(_, call)).exists(Typing.isFuture(_, call))

  private def expectsFutureInEffect(call: ScMethodCall): Boolean =
    call.expectedType().exists(et => Typing.lastTypeArg(et, call).exists(Typing.isFuture(_, call)))

  /** `IO(fut)` for `IO.pure(fut)`, `X.delay(fut)` for `X.pure(fut)`. */
  private def suspendedText(fut: ScExpression, pureQual: Option[ScExpression]): Option[String] = pureQual match {
    case Some(q) if Typing.objectFqn(q).contains("cats.effect.IO") => Some(s"IO(${fut.getText})")
    case Some(q)                                                   => Some(s"${q.getText}.delay(${fut.getText})")
    case None                                                      => None
  }

  /** A by-value parameter or a (non-lazy) val holding a Future: it started when it was created. */
  private def isAlreadyRunning(r: ScReferenceExpression): Boolean =
    Typing.typeOf(r).exists(Typing.isFuture(_, r)) && (try {
      r.resolve() match {
        case p: ScParameter        => !p.isCallByNameParameter
        case p: ScReferencePattern => localValOf(r).isDefined || isNonLazyVal(p)
        case _                     => false
      }
    } catch { case NonFatal(_) => false })

  private def isNonLazyVal(p: ScReferencePattern): Boolean =
    org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.inNameContext.unapply(p).exists {
      case d: ScPatternDefinition => !d.hasModifierProperty("lazy")
      case _                      => false
    }

  /** For a local `val fut = Future(...)` used only here: inline the construction into the IO. */
  private def moveInsideFix(r: ScReferenceExpression): Option[LocalQuickFix] =
    localValOf(r).filter(d => d.expr.isDefined && usages(d) == 1).map { _ =>
      new EditFix[ScReferenceExpression](moveInsideHint, r)((ref, _) => moveInside(ref))
    }
}

object EagerFutureInspection {

  val pureMessage = "IO.pure evaluates the Future eagerly: it is already running when fromFuture receives it"

  def alreadyRunningMessage(name: String): String =
    s"'$name' is a Future created outside the effect: it is already running and fromFuture can't make it lazy"

  val ignoredFutureMessage = "This effect returns a Future whose result and errors are ignored; use IO.fromFuture"

  val suspendHint    = "Suspend the Future construction (IO(...) instead of IO.pure)"
  val moveInsideHint = "Move the Future construction inside the effect"
  val wrapHint       = "Wrap in IO.fromFuture"

  private def usages(d: ScPatternDefinition): Int =
    d.bindings.headOption.map(b => ReferencesSearch.search(b, new LocalSearchScope(d.getContainingFile)).findAll().size()).getOrElse(0)

  private def moveInside(ref: ScReferenceExpression): Unit =
    localValOf(ref).foreach { d =>
      d.expr.foreach { init =>
        Rewriter.replace(ref, init.getText, None)
        Rewriter.deleteStatement(d)
      }
    }
}
