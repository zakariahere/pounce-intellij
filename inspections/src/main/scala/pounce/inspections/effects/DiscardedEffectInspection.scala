package pounce.inspections.effects

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.expressionResultIsNotUsed
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.util.Typing
import pounce.inspections.{EditFix, PounceInspection, ReplaceExpressionFix, Rewriter}

/**
 * B1. An effect value (IO, Resource, EitherT, Stream, tagless `F[A]`, ...) in statement position is never sequenced,
 * so it never runs: `{ IO.println("a"); IO.println("b") }` only prints "b". Value discarding too:
 * `def log(): Unit = IO.println("a")`.
 *
 * Not flagged: Unit-returning side effects, `Future` (eager: it runs anyway), the result expression of a block that
 * flows out, assignments, and anything we can't classify as a lazy effect.
 */
final class DiscardedEffectInspection extends PounceInspection {

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case ifStmt: ScIf if ifStmt.elseExpression.isEmpty =>
      for {
        cond  <- ifStmt.condition
        thenE <- ifStmt.thenExpression
        if Typing.isEffect(thenE)
      } {
        val fixes = whenAFix(ifStmt, cond, thenE).toSeq
        report(holder, thenE, DiscardedEffectInspection.ifMessage(display(thenE)), fixes)
      }
    case e: ScExpression if isLeaf(e) && !isThenOfElselessIf(e) =>
      discardedType(e).foreach(tpe => report(holder, e, DiscardedEffectInspection.message(tpe), chainFix(e).toSeq))
    case _ =>
  }

  /** The displayed type of a discarded effect: a statement whose value is unused, or a value-discarded expression. */
  private def discardedType(e: ScExpression): Option[String] =
    if (expressionResultIsNotUsed(e) && Typing.isEffect(e)) Some(display(e))
    else valueDiscarded(e)

  /**
   * Value discarding: `def log(): Unit = IO.println(...)`, or an effect passed where `=> Unit` is expected. The Scala
   * plugin types such an expression as `Unit` (it applies the discard against the real expected type), so we type a
   * copy as the right-hand side of a local `val` in the same scope, where nothing is expected, to see the effect.
   */
  private def valueDiscarded(e: ScExpression): Option[String] =
    try
      if (!e.expectedType().exists(Typing.isUnit) || !mayBeEffectCall(e)) None
      else
        Typing.synthetic(e, s"{ val pounceDiscarded = ${e.getText}; () }") match {
          case Some(b: ScBlock) =>
            b.statements.headOption
              .collect { case d: org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition => d }
              .flatMap(_.expr)
              .flatMap(Typing.typeOf)
              .filter(t => !Typing.isUnit(t) && Typing.isEffect(t, e))
              .map(Typing.show(_, e))
          case _ => None
        }
    catch {
      case pce: com.intellij.openapi.progress.ProcessCanceledException => throw pce
      case scala.util.control.NonFatal(_)                             => None
    }

  /** Only calls and references can produce an effect value; skip literals, lambdas, etc. before re-typing. */
  private def mayBeEffectCall(e: ScExpression): Boolean = e match {
    case _: ScMethodCall | _: ScReferenceExpression | _: ScGenericCall | _: ScInfixExpr | _: ScPostfixExpr => true
    case _                                                                                                  => false
  }

  private def display(e: ScExpression): String = Typing.typeOf(e).map(Typing.show(_, e)).getOrElse("effect")

  /** Containers (blocks, ifs, matches, ...) are not reported themselves; their result expressions are. */
  private def isLeaf(e: ScExpression): Boolean = e match {
    case _: ScBlock | _: ScParenthesisedExpr | _: ScIf | _: ScMatch | _: ScTry | _: ScAssignment | _: ScReturn | _: ScThrow | _: ScWhile |
        _: ScDo | _: ScUnitExpr => false
    case _ => true
  }

  private def isThenOfElselessIf(e: ScExpression): Boolean = e.getParent match {
    case i: ScIf => i.elseExpression.isEmpty && i.thenExpression.contains(e)
    case _       => false
  }

  /** `{ a; b }` where `a` is discarded: `{ a >> b }`, when that typechecks. */
  private def chainFix(e: ScExpression): Option[LocalQuickFix] = e.getParent match {
    case block: ScBlock if !block.isInstanceOf[ScFor] =>
      val stmts = block.statements
      val idx   = stmts.indexOf(e)
      stmts.lift(idx + 1) match {
        case Some(next: ScExpression) if Typing.isEffect(next) =>
          val text = s"${Typing.operand(e)} >> ${Typing.operand(next)}"
          Typing.checkReplacement(next, text, Some("flatMap"), sameType = false).map { importRoot =>
            new EditFix[ScExpression](DiscardedEffectInspection.chainHint, e)((stmt, project) =>
              DiscardedEffectInspection.chainWithNext(stmt, importRoot, project)
            )
          }
        case _ => None
      }
    case _ => None
  }

  private def whenAFix(ifStmt: ScIf, cond: ScExpression, thenE: ScExpression): Option[LocalQuickFix] = {
    val text = s"${Typing.operand(thenE)}.whenA(${cond.getText})"
    Typing.checkReplacement(ifStmt, text, Some("applicative"), sameType = false).map { importRoot =>
      new ReplaceExpressionFix(DiscardedEffectInspection.whenAHint, ifStmt, text, importRoot)
    }
  }
}

object DiscardedEffectInspection {
  def message(tpe: String): String =
    s"Discarded effect: this $tpe is never sequenced, so it never runs"

  def ifMessage(tpe: String): String =
    s"Discarded effect: an 'if' without 'else' throws this $tpe away, so it never runs"

  val chainHint = "Chain with the next statement using >>"
  val whenAHint = "Replace with .whenA(condition)"

  private def chainWithNext(stmt: ScExpression, importRoot: Option[String], project: com.intellij.openapi.project.Project): Unit =
    stmt.getParent match {
      case block: ScBlock =>
        val stmts = block.statements
        stmts.lift(stmts.indexOf(stmt) + 1) match {
          case Some(next: ScExpression) =>
            val text = s"${Typing.operand(stmt)} >> ${Typing.operand(next)}"
            Rewriter.replace(next, text, importRoot)
            Rewriter.deleteStatement(stmt)
          case _ =>
        }
      case _ =>
    }
}
