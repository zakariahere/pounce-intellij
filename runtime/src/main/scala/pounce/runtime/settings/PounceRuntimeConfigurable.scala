package pounce.runtime.settings

import com.intellij.openapi.options.Configurable
import com.intellij.ui.components.{JBCheckBox, JBLabel}
import com.intellij.util.ui.{FormBuilder, JBUI}
import pounce.runtime.debugger.CatsSteppingFilterProvider

import javax.swing.{JComponent, JPanel}

/** Settings → Languages & Frameworks → Pounce → Runtime & Debugger. */
final class PounceRuntimeConfigurable extends Configurable {

  private val steppingFilters = new JBCheckBox("Step Into skips Cats / Cats Effect / fs2 internals")
  private val renderers       = new JBCheckBox("Debugger renderers for NonEmptyList, Chain, Validated, EitherT, OptionT, Ref")
  private val weaverGutters   = new JBCheckBox("Run gutter icons on weaver tests (runs through sbt testOnly)")

  override def getDisplayName: String = "Runtime & Debugger"

  override def createComponent(): JComponent = {
    val patterns = CatsSteppingFilterProvider.Patterns.map(_.pattern).mkString(", ")
    val panel: JPanel = FormBuilder
      .createFormBuilder()
      .addComponent(steppingFilters)
      .addComponent(new JBLabel(s"<html><small>Filtered classes: $patterns</small></html>"))
      .addComponent(renderers)
      .addComponent(new JBLabel("<html><small>Renderers changes apply to new debug sessions.</small></html>"))
      .addComponent(weaverGutters)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel
    panel.setBorder(JBUI.Borders.empty(8))
    reset()
    panel
  }

  private def s = PounceRuntimeSettings()

  override def isModified: Boolean =
    steppingFilters.isSelected != s.steppingFilters ||
      renderers.isSelected != s.catsRenderers ||
      weaverGutters.isSelected != s.weaverGutters

  override def apply(): Unit = {
    val st = s
    st.steppingFilters = steppingFilters.isSelected
    st.catsRenderers = renderers.isSelected
    st.weaverGutters = weaverGutters.isSelected
  }

  override def reset(): Unit = {
    val st = s
    steppingFilters.setSelected(st.steppingFilters)
    renderers.setSelected(st.catsRenderers)
    weaverGutters.setSelected(st.weaverGutters)
  }
}
