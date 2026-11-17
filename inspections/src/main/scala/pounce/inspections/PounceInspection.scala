package pounce.inspections

import com.intellij.codeInspection.{LocalInspectionTool, LocalQuickFix, ProblemHighlightType, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.codeInspection.AbstractFixOnPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression, ScParenthesisedExpr}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.{ProjectContext, ScalaFeatures}
import pounce.core.{Imports, Lib, Libraries}

/**
 * Base for every Pounce inspection: the visitor is empty in modules that don't have the libraries the inspection is
 * about, so plain Scala projects pay nothing.
 */
abstract class PounceInspection extends LocalInspectionTool {

  protected def requiredLibraries: Set[Lib] = Set(Lib.CatsCore)

  final override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = {
    val file = holder.getFile
    if (file == null || !requiredLibraries.forall(Libraries.has(file, _))) PsiElementVisitor.EMPTY_VISITOR
    else
      new PsiElementVisitor {
        override def visitElement(element: PsiElement): Unit = element match {
          // The collection extractors (`Qualified`, `MethodRepr`) look through `{ ... }` / `( ... )`; visiting the
          // wrapper too would report the inner problem twice, with a stretched range.
          case _: ScBlock | _: ScParenthesisedExpr =>
          case _                                   => visit(element, holder, isOnTheFly)
        }
      }
  }

  protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit

  /** Registers a problem; `range` is relative to `element` (null = whole element). */
  protected final def report(
    holder: ProblemsHolder,
    element: PsiElement,
    message: String,
    fixes: Seq[LocalQuickFix] = Nil,
    range: TextRange = null,
    highlight: ProblemHighlightType = ProblemHighlightType.GENERIC_ERROR_OR_WARNING
  ): Unit =
    holder.registerProblem(element, message, highlight, range, fixes*)
}

/**
 * Replaces an expression with new source text and, when the replacement needs a cats syntax import that isn't in
 * scope, adds it in the user's import style.
 */
final class ReplaceExpressionFix(name: String, expr: ScExpression, replacement: String, importRoot: Option[String] = None)
    extends AbstractFixOnPsiElement[ScExpression](name, expr) {

  override protected def doApplyFix(element: ScExpression)(implicit project: Project): Unit =
    Rewriter.replace(element, replacement, importRoot)
}

object Rewriter {

  /** Replaces `element` with `replacement` (source text) and adds the syntax import if one is needed. */
  def replace(element: ScExpression, replacement: String, importRoot: Option[String]): ScExpression = {
    val newExpr = ScalaPsiElementFactory.createExpressionFromText(replacement, ScalaFeatures.forPsiOrDefault(element))(
      using ProjectContext.fromProject(element.getProject)
    )
    val replaced = element.replaceExpression(newExpr, removeParenthesis = true)
    importRoot.foreach(root => Imports.ensureWildcardImport(root, replaced))
    replaced
  }

  /** Deletes a block statement together with the separator / whitespace that follows it. */
  def deleteStatement(stmt: PsiElement): Unit = {
    var next = stmt.getNextSibling
    while (next != null && (next.isInstanceOf[com.intellij.psi.PsiWhiteSpace] || next.getText == ";")) {
      val n = next.getNextSibling
      next.delete()
      next = n
    }
    stmt.delete()
  }
}

/** Runs an arbitrary edit on one element (for fixes that aren't a plain expression replacement). */
final class EditFix[T <: PsiElement](name: String, element: T)(edit: (T, Project) => Unit) extends AbstractFixOnPsiElement[T](name, element) {
  override protected def doApplyFix(e: T)(implicit project: Project): Unit = edit(e, project)
}
