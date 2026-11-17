package pounce.visual.hints

import com.intellij.codeInsight.hints.declarative.{InlayHintsCollector, InlayHintsProvider, InlayTreeSink, SharedBypassCollector}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.codeInsight.implicits.ImplicitHints
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import pounce.core.Libraries
import pounce.core.settings.PounceSettings
import pounce.visual.util.VisualPsi

/**
 * D3 inlays. Off by default (Settings | Languages & Frameworks | Pounce | Instance hints). Stays silent while the
 * Scala plugin's own implicit hints are switched on (Ctrl+Alt+Shift+=), which already show every implicit argument.
 */
final class InstanceInlayProvider extends InlayHintsProvider {

  override def createCollector(file: PsiFile, editor: Editor): InlayHintsCollector =
    file match {
      case sf: ScalaFile if PounceSettings().instanceHints && !scalaImplicitHintsOn && Libraries.hasCats(sf) =>
        InstanceInlayProvider.Collector
      case _ => null
    }

  private def scalaImplicitHintsOn: Boolean = VisualPsi.safe(ImplicitHints.enabled).contains(true)
}

object InstanceInlayProvider {

  object Collector extends SharedBypassCollector {
    override def collectFromElement(element: PsiElement, sink: InlayTreeSink): Unit =
      InstanceHints.hintFor(element).foreach { h =>
        Inlays.add(sink, h.offset, h.tooltip, h.instances.flatMap(i => Seq(Inlays.Part(" · "), Inlays.Part(i.text, i.target))).drop(1))
      }
  }
}
