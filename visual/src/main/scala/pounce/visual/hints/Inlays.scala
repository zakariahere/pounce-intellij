package pounce.visual.hints

import com.intellij.codeInsight.hints.declarative.*
import com.intellij.psi.{PsiElement, SmartPointerManager}

/** Writing hints to the declarative inlay sink (Kotlin lambdas spelled out for Scala). */
object Inlays {

  final case class Part(text: String, target: Option[PsiElement] = None)

  def add(sink: InlayTreeSink, offset: Int, tooltip: String, parts: Seq[Part]): Unit =
    if (parts.nonEmpty)
      sink.addPresentation(
        new InlineInlayPosition(offset, true, 0),
        null,
        tooltip,
        HintFormat.Companion.getDefault,
        (b: PresentationTreeBuilder) => {
          parts.foreach { p =>
            val action = p.target.filter(_.isValid).map { t =>
              val pointer = SmartPointerManager.getInstance(t.getProject).createSmartPsiElementPointer(t)
              new InlayActionData(new PsiPointerInlayActionPayload(pointer), PsiPointerInlayActionNavigationHandler.HANDLER_ID)
            }
            b.text(p.text, action.orNull)
          }
          kotlin.Unit.INSTANCE
        }
      )

  def add(sink: InlayTreeSink, offset: Int, text: String, tooltip: String): Unit =
    add(sink, offset, tooltip, Seq(Part(text)))
}
