package pounce.visual.toolwindow

import com.intellij.openapi.command.WriteCommandAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiFile, SmartPsiElementPointer}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import pounce.core.Imports
import pounce.core.catalog.Op

/** Inserting catalog ops / finder results into the current editor, with the cats syntax import when needed. */
object EditorInsert {

  def currentEditor(project: Project): Option[Editor] = Option(FileEditorManager.getInstance(project).getSelectedTextEditor)

  def fileOf(project: Project, editor: Editor): Option[PsiFile] =
    Option(PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument))

  /** `xs` + `.traverse(|)` → `xs.traverse(|)`, caret inside the parentheses. */
  def appendOp(project: Project, editor: Editor, target: SmartPsiElementPointer[ScExpression], op: Op): Unit =
    Option(target.getElement).filter(_.isValid).foreach { expr =>
      val (text, caret) = op.insertTextAndCaret
      run(project, expr.getContainingFile) {
        val doc = editor.getDocument
        val end = expr.getTextRange.getEndOffset
        doc.insertString(end, "." + text)
        editor.getCaretModel.moveToOffset(end + 1 + caret)
        addImports(project, editor, end, Seq(op))
      }
    }

  /** Inserts (or replaces the selection with) `text` at the caret. */
  def insertAtCaret(project: Project, editor: Editor, text: String, ops: Seq[Op]): Unit =
    fileOf(project, editor).foreach { file =>
      run(project, file) {
        val doc   = editor.getDocument
        val sel   = editor.getSelectionModel
        val start = if (sel.hasSelection) sel.getSelectionStart else editor.getCaretModel.getOffset
        val end   = if (sel.hasSelection) sel.getSelectionEnd else start
        doc.replaceString(start, end, text)
        sel.removeSelection()
        editor.getCaretModel.moveToOffset(start + text.length)
        addImports(project, editor, start, ops)
      }
    }

  private def addImports(project: Project, editor: Editor, offset: Int, ops: Seq[Op]): Unit = {
    val pdm = PsiDocumentManager.getInstance(project)
    pdm.commitDocument(editor.getDocument)
    fileOf(project, editor).flatMap(f => Option(f.findElementAt(offset))).foreach { place =>
      ops.foreach(op => if (!Imports.isProvided(op, place)) Imports.ensureImportFor(op, place))
      pdm.doPostponedOperationsAndUnblockDocument(editor.getDocument)
    }
  }

  private def run(project: Project, file: PsiFile)(body: => Unit): Unit =
    WriteCommandAction.runWriteCommandAction(project, "Pounce: insert", null, (() => body): Runnable, file)
}
