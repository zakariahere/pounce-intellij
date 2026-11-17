package pounce.visual.hints

import com.intellij.codeInsight.hints.declarative.{InlayHintsCollector, InlayHintsProvider, InlayTreeSink, SharedBypassCollector}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScFor
import pounce.core.Libraries
import pounce.core.settings.PounceSettings

/**
 * D1 inlays (declarative inlay API, `codeInsight.declarativeInlayProvider`). Gated by
 * Settings | Languages & Frameworks | Pounce | "For-comprehension hints" and by the platform's inlay settings.
 */
final class ForComprehensionInlayProvider extends InlayHintsProvider {

  override def createCollector(file: PsiFile, editor: Editor): InlayHintsCollector =
    file match {
      case sf: ScalaFile if PounceSettings().forComprehensionHints && Libraries.hasCats(sf) => ForComprehensionInlayProvider.Collector
      case _                                                                                => null
    }
}

object ForComprehensionInlayProvider {

  object Collector extends SharedBypassCollector {
    override def collectFromElement(element: PsiElement, sink: InlayTreeSink): Unit = element match {
      case f: ScFor => ForComprehensionLens.hintsFor(f).foreach(h => Inlays.add(sink, h.offset, h.text, h.tooltip))
      case _        =>
    }
  }
}
