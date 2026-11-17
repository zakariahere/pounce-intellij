package pounce.visual

import com.intellij.codeInsight.hints.declarative.*
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.scala.extensions.*

import scala.collection.mutable

/**
 * Runs a declarative inlay provider's collector over every element of a file, recording what it would show — the
 * platform's own DeclarativeInlayHintsProviderTestCase is tied to BasePlatformTestCase, not our Scala fixture.
 */
object InlayTestSupport {

  final case class Recorded(offset: Int, text: String, tooltip: String, targets: Int)

  final class RecordingSink extends InlayTreeSink {
    val hints: mutable.Buffer[Recorded] = mutable.ArrayBuffer.empty

    override def addPresentation(
      position: InlayPosition,
      payloads: java.util.List[InlayPayload],
      tooltip: String,
      hintFormat: HintFormat,
      builder: kotlin.jvm.functions.Function1[? >: PresentationTreeBuilder, kotlin.Unit]
    ): Unit = {
      val b = new RecordingBuilder
      builder.invoke(b)
      val offset = position match {
        case p: InlineInlayPosition => p.getOffset
        case _                      => -1
      }
      hints += Recorded(offset, b.text.toString, tooltip, b.targets)
    }

    override def whenOptionEnabled(optionId: String, block: kotlin.jvm.functions.Function0[kotlin.Unit]): Unit = block.invoke()
  }

  final class RecordingBuilder extends CollapsiblePresentationTreeBuilder {
    val text    = new StringBuilder
    var targets = 0

    override def list(builder: kotlin.jvm.functions.Function1[? >: PresentationTreeBuilder, kotlin.Unit]): Unit =
      builder.invoke(this)

    override def collapsibleList(
      state: CollapseState,
      expandedState: kotlin.jvm.functions.Function1[? >: CollapsiblePresentationTreeBuilder, kotlin.Unit],
      collapsedState: kotlin.jvm.functions.Function1[? >: CollapsiblePresentationTreeBuilder, kotlin.Unit]
    ): Unit = expandedState.invoke(this)

    override def text(text: String, actionData: InlayActionData): Unit = {
      this.text ++= text
      if (actionData != null) targets += 1
    }

    override def clickHandlerScope(actionData: InlayActionData, builder: kotlin.jvm.functions.Function1[? >: PresentationTreeBuilder, kotlin.Unit]): Unit =
      builder.invoke(this)

    override def toggleButton(builder: kotlin.jvm.functions.Function1[? >: PresentationTreeBuilder, kotlin.Unit]): Unit =
      builder.invoke(this)
  }

  def collect(provider: InlayHintsProvider, file: PsiFile, editor: com.intellij.openapi.editor.Editor): Seq[Recorded] =
    provider.createCollector(file, editor) match {
      case null => Seq.empty
      case c: SharedBypassCollector =>
        val sink = new RecordingSink
        file.depthFirst().foreach(e => c.collectFromElement(e, sink))
        sink.hints.toSeq.sortBy(_.offset)
      case c: OwnBypassCollector =>
        val sink = new RecordingSink
        c.collectHintsForFile(file, sink)
        sink.hints.toSeq.sortBy(_.offset)
    }

  /** Renders hints into the text at their offsets, `‹like this›`, for readable assertions. */
  def render(text: String, hints: Seq[Recorded]): String = {
    val sb = new StringBuilder(text)
    hints.sortBy(-_.offset).foreach(h => sb.insert(h.offset, s"‹${h.text}›"))
    sb.toString
  }
}
