package pounce.core.settings

import com.intellij.openapi.options.Configurable
import com.intellij.openapi.ui.ComboBox
import com.intellij.ui.components.{JBCheckBox, JBLabel, JBTextField}
import com.intellij.util.ui.{FormBuilder, JBUI}

import javax.swing.{JComponent, JPanel}

/** Settings → Languages & Frameworks → Pounce. */
final class PounceConfigurable extends Configurable {

  private val importStyle = new ComboBox[ImportStyle](ImportStyle.values)
  importStyle.setRenderer(
    new com.intellij.ui.SimpleListCellRenderer[ImportStyle] {
      override def customize(list: javax.swing.JList[? <: ImportStyle], value: ImportStyle, index: Int, selected: Boolean, hasFocus: Boolean): Unit =
        if (value != null) setText(value.label)
    }
  )

  private val syntaxCompletion    = new JBCheckBox("Cats syntax methods without imports (traverse, void, attempt, ...)")
  private val liftCompletion      = new JBCheckBox("Lifting methods on any value (.some, .asRight, .pure[F], ...)")
  private val tupleCompletion     = new JBCheckBox("Tuple methods (mapN, tupled, parMapN, ...)")
  private val companionCompletion = new JBCheckBox("Curated companion completion (IO., Sync[F]., Resource., ...)")
  private val smartCompletion     = new JBCheckBox("Smart completion by expected type (Ctrl+Shift+Space)")

  private val forHints       = new JBCheckBox("For-comprehension lens (effect and bound types)")
  private val instanceHints  = new JBCheckBox("Typeclass instance hints after Cats calls")
  private val forkGutters    = new JBCheckBox("Fork gutter icons (start, background, parTraverse, race)")
  private val resourceGutters = new JBCheckBox("Resource acquire/release gutter icons")
  private val blockingGutters = new JBCheckBox("Blocking gutter icons (IO.blocking, Sync[F].blocking)")
  private val wiringGutters  = new JBCheckBox("Tagless algebra wiring gutters")

  private val playConfig   = new JBCheckBox("Play: application.conf key completion & navigation")
  private val playMessages = new JBCheckBox("Play: i18n messages key completion & navigation")
  private val playGuice    = new JBCheckBox("Play: Guice binding navigation")

  private val allowList = new JBTextField()
  private val compilerBacked = new JBCheckBox("Experimental: compiler-backed completion (Metals presentation compiler)")

  override def getDisplayName: String = "Pounce"

  override def createComponent(): JComponent = {
    val panel: JPanel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("Import style for Cats syntax:"), importStyle)
      .addSeparator()
      .addComponent(new JBLabel("Completion"))
      .addComponent(syntaxCompletion)
      .addComponent(liftCompletion)
      .addComponent(tupleCompletion)
      .addComponent(companionCompletion)
      .addComponent(smartCompletion)
      .addSeparator()
      .addComponent(new JBLabel("Visual"))
      .addComponent(forHints)
      .addComponent(instanceHints)
      .addComponent(forkGutters)
      .addComponent(resourceGutters)
      .addComponent(blockingGutters)
      .addComponent(wiringGutters)
      .addSeparator()
      .addComponent(new JBLabel("Play 3"))
      .addComponent(playConfig)
      .addComponent(playMessages)
      .addComponent(playGuice)
      .addSeparator()
      .addLabeledComponent(new JBLabel("unsafeRun* allowed in (classes, comma-separated):"), allowList)
      .addComponent(compilerBacked)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel
    panel.setBorder(JBUI.Borders.empty(8))
    reset()
    panel
  }

  private def s = PounceSettings()

  override def isModified: Boolean =
    importStyle.getSelectedItem != s.style ||
      syntaxCompletion.isSelected != s.syntaxCompletion ||
      liftCompletion.isSelected != s.liftCompletion ||
      tupleCompletion.isSelected != s.tupleCompletion ||
      companionCompletion.isSelected != s.companionCompletion ||
      smartCompletion.isSelected != s.smartCompletion ||
      forHints.isSelected != s.forComprehensionHints ||
      instanceHints.isSelected != s.instanceHints ||
      forkGutters.isSelected != s.forkGutters ||
      resourceGutters.isSelected != s.resourceGutters ||
      blockingGutters.isSelected != s.blockingGutters ||
      wiringGutters.isSelected != s.wiringGutters ||
      playConfig.isSelected != s.playConfigKeys ||
      playMessages.isSelected != s.playMessages ||
      playGuice.isSelected != s.playGuice ||
      allowList.getText != s.unsafeRunAllowList ||
      compilerBacked.isSelected != s.compilerBackedCompletion

  override def apply(): Unit = {
    val st = s
    st.importStyle = importStyle.getSelectedItem.asInstanceOf[ImportStyle].toString
    st.syntaxCompletion = syntaxCompletion.isSelected
    st.liftCompletion = liftCompletion.isSelected
    st.tupleCompletion = tupleCompletion.isSelected
    st.companionCompletion = companionCompletion.isSelected
    st.smartCompletion = smartCompletion.isSelected
    st.forComprehensionHints = forHints.isSelected
    st.instanceHints = instanceHints.isSelected
    st.forkGutters = forkGutters.isSelected
    st.resourceGutters = resourceGutters.isSelected
    st.blockingGutters = blockingGutters.isSelected
    st.wiringGutters = wiringGutters.isSelected
    st.playConfigKeys = playConfig.isSelected
    st.playMessages = playMessages.isSelected
    st.playGuice = playGuice.isSelected
    st.unsafeRunAllowList = allowList.getText
    st.compilerBackedCompletion = compilerBacked.isSelected
  }

  override def reset(): Unit = {
    val st = s
    importStyle.setSelectedItem(st.style)
    syntaxCompletion.setSelected(st.syntaxCompletion)
    liftCompletion.setSelected(st.liftCompletion)
    tupleCompletion.setSelected(st.tupleCompletion)
    companionCompletion.setSelected(st.companionCompletion)
    smartCompletion.setSelected(st.smartCompletion)
    forHints.setSelected(st.forComprehensionHints)
    instanceHints.setSelected(st.instanceHints)
    forkGutters.setSelected(st.forkGutters)
    resourceGutters.setSelected(st.resourceGutters)
    blockingGutters.setSelected(st.blockingGutters)
    wiringGutters.setSelected(st.wiringGutters)
    playConfig.setSelected(st.playConfigKeys)
    playMessages.setSelected(st.playMessages)
    playGuice.setSelected(st.playGuice)
    allowList.setText(st.unsafeRunAllowList)
    compilerBacked.setSelected(st.compilerBackedCompletion)
  }
}
