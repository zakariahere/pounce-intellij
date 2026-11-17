package pounce.visual.finder

import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.types.ScTypeBridge
import pounce.visual.util.VisualPsi

/**
 * D6: derives the finder's query from the editor: `have` = the type of the selected expression (or the expression
 * at the caret), `want` = the expected type at the caret (a `???` hole, or the selected expression's slot).
 */
object FinderPrefill {

  final case class Prefill(haveName: String, have: String, want: String)

  def at(file: PsiFile, editor: Editor): Prefill = {
    val sel = editor.getSelectionModel
    val expr: Option[ScExpression] =
      if (sel.hasSelection) Option(PsiTreeUtil.findElementOfClassAtRange(file, sel.getSelectionStart, sel.getSelectionEnd, classOf[ScExpression]))
      else
        Option(file.findElementAt(editor.getCaretModel.getOffset)).flatMap(_.parentOfType[ScExpression])
    expr match {
      case Some(hole: ScReferenceExpression) if HoleFiller.isHole(hole) =>
        Prefill("", "", present(VisualPsi.safe(hole.expectedType()).flatten, hole))
      case Some(e) =>
        val name = e.getText match {
          case t if t.matches("[a-z_][\\w]*") => t
          case _                               => "x"
        }
        Prefill(name, present(VisualPsi.typeOf(e), e), present(VisualPsi.safe(e.expectedType()).flatten, e))
      case None => Prefill("", "", "")
    }
  }

  private def present(t: Option[ScType], place: ScExpression): String =
    t.flatMap(tt => VisualPsi.safe(ScTypeBridge.present(tt, place))).getOrElse("")
}
