package pounce.visual.finder

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.WriteCommandAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import pounce.core.{Imports, Libraries}
import pounce.visual.finder.CombinatorFinder.Match

import scala.jdk.CollectionConverters.*

/**
 * D6: "Fill ??? with a combinator" on a `???` whose expected type is known. Proposes one-step catalog expressions
 * built from the values in scope; picking one replaces the hole and adds the syntax import if needed.
 */
final class FillHoleIntention extends PsiElementBaseIntentionAction {

  override def getFamilyName: String = "Pounce: fill ??? with a combinator"

  override def startInWriteAction(): Boolean = false

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    HoleFiller.holeAt(element).filter(Libraries.hasCats).exists { hole =>
      val found = HoleFiller.candidates(hole)
      found.headOption.foreach { m =>
        setText(if (found.size == 1) s"Fill ??? with ${m.expression}" else s"Fill ??? with a combinator (${m.expression}, …)")
      }
      found.nonEmpty
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    HoleFiller.holeAt(element).foreach { hole =>
      val found = HoleFiller.candidates(hole)
      if (found.size == 1 || (found.nonEmpty && ApplicationManager.getApplication.isUnitTestMode)) apply(project, hole, found.head)
      else if (found.nonEmpty)
        JBPopupFactory
          .getInstance()
          .createPopupChooserBuilder(found.asJava)
          .setTitle("Fill ??? with")
          .setRenderer(new com.intellij.ui.SimpleListCellRenderer[Match] {
            override def customize(list: javax.swing.JList[? <: Match], value: Match, index: Int, selected: Boolean, hasFocus: Boolean): Unit =
              setText(s"${value.expression}    ${value.signature}")
          })
          .setItemChosenCallback(m => apply(project, hole, m))
          .createPopup()
          .showInBestPositionFor(editor)
    }

  private def apply(project: Project, hole: ScExpression, m: Match): Unit =
    WriteCommandAction.runWriteCommandAction(project, "Fill ???", null, (() => {
      if (hole.isValid) {
        val replacement = ScalaPsiElementFactory.createExpressionWithContextFromText(m.expression, hole.getContext, hole)
        val inserted    = hole.replace(replacement)
        m.ops.foreach(op => if (!Imports.isProvided(op, inserted)) Imports.ensureImportFor(op, inserted))
        Option(PsiDocumentManager.getInstance(project).getDocument(inserted.getContainingFile))
          .foreach(PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument)
      }
    }): Runnable, hole.getContainingFile)

  override def generatePreview(project: Project, editor: Editor, file: PsiFile): IntentionPreviewInfo = IntentionPreviewInfo.EMPTY
}
