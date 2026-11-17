package pounce.inspections.tagless

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.psi.PsiElement
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScContextBound
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScTypeParam}
import pounce.core.Typeclasses
import pounce.inspections.util.BoundEditor
import pounce.inspections.util.BoundEditor.{ContextBound, ImplicitParam}
import pounce.inspections.{EditFix, PounceInspection}

/**
 * B8 (a). A typeclass bound implied by another bound of the same type parameter: `F[_]: Monad: Sync` (Sync is a
 * Monad). Besides being noise, two evidences for `Monad[F]` make implicit resolution ambiguous in Scala 2, which
 * breaks syntax like `.flatMap`. Fix: remove the redundant bound.
 */
final class RedundantBoundInspection extends PounceInspection {

  import RedundantBoundInspection.*

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case tp: ScTypeParam =>
      val declared = BoundEditor.declared(tp)
      if (declared.size >= 2) {
        val pairs = Typeclasses.redundant(declared.map(_.typeclass))
        pairs.groupBy(_._1).foreach { case (redundantName, byPairs) =>
          val by = byPairs.head._2
          declared.filter(_.typeclass == redundantName).foreach {
            case ContextBound(bound, name) =>
              report(holder, bound, message(name, by), Seq(removeBoundFix(bound, name)))
            case ImplicitParam(param, name) =>
              val fix = if (isUnused(param)) Seq(removeParamFix(param, name)) else Nil
              report(holder, param, message(name, by), fix)
          }
        }
      }
    case _ =>
  }

  private def isUnused(p: ScParameter): Boolean =
    ReferencesSearch.search(p, new LocalSearchScope(p.getContainingFile)).findFirst() == null

  private def removeBoundFix(bound: ScContextBound, name: String): LocalQuickFix =
    new EditFix[ScContextBound](removeHint(name), bound)((b, _) => BoundEditor.removeContextBound(b))

  private def removeParamFix(param: ScParameter, name: String): LocalQuickFix =
    new EditFix[ScParameter](removeHint(name), param)((p, _) => removeImplicitParam(p))
}

object RedundantBoundInspection {

  def message(redundant: String, by: String): String =
    s"Redundant bound $redundant: it is implied by $by (and makes implicit resolution ambiguous)"

  def removeHint(name: String): String = s"Remove redundant $name"

  private def removeImplicitParam(p: ScParameter): Unit =
    p.getParent match {
      case clause: org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameterClause if clause.parameters.size == 1 => clause.delete()
      case _ =>
        // `a, b` → delete the parameter and one neighbouring comma
        val next = p.getNextSibling
        val prev = p.getPrevSibling
        def isComma(e: PsiElement) = e != null && e.getText == ","
        def skipWs(e: PsiElement, forward: Boolean): PsiElement = {
          var x = e
          while (x != null && x.isInstanceOf[com.intellij.psi.PsiWhiteSpace]) x = if (forward) x.getNextSibling else x.getPrevSibling
          x
        }
        val nextComma = skipWs(next, forward = true)
        val prevComma = skipWs(prev, forward = false)
        if (isComma(nextComma)) {
          val after = nextComma.getNextSibling
          p.getParent.deleteChildRange(p, if (after != null && after.isInstanceOf[com.intellij.psi.PsiWhiteSpace]) after else nextComma)
        }
        else if (isComma(prevComma)) p.getParent.deleteChildRange(prevComma, p)
        else p.delete()
    }
}
