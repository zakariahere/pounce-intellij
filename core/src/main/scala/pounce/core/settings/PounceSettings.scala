package pounce.core.settings

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.{PersistentStateComponent, State, Storage}
import com.intellij.util.xmlb.XmlSerializerUtil

import scala.beans.BeanProperty

/** How Pounce adds the import that brings Cats syntax into scope. */
enum ImportStyle(val label: String) {
  /** Count what the project already uses; default to `All` when undecided. */
  case Auto     extends ImportStyle("Auto-detect from project")
  case All      extends ImportStyle("import cats.syntax.all._")
  case Implicits extends ImportStyle("import cats.implicits._")
  case Granular extends ImportStyle("import cats.syntax.<module>._ (e.g. cats.syntax.traverse._)")
}

/** Application-wide settings (Settings → Languages & Frameworks → Pounce). */
@State(name = "PounceSettings", storages = Array(new Storage("pounce.xml")))
final class PounceSettings extends PersistentStateComponent[PounceSettings] {

  @BeanProperty var importStyle: String = ImportStyle.Auto.toString

  // completion
  @BeanProperty var syntaxCompletion: Boolean    = true
  @BeanProperty var liftCompletion: Boolean      = true
  @BeanProperty var tupleCompletion: Boolean     = true
  @BeanProperty var companionCompletion: Boolean = true
  @BeanProperty var smartCompletion: Boolean     = true

  // visual
  @BeanProperty var forComprehensionHints: Boolean = true
  @BeanProperty var instanceHints: Boolean         = false
  @BeanProperty var forkGutters: Boolean           = true
  @BeanProperty var resourceGutters: Boolean       = true
  @BeanProperty var blockingGutters: Boolean       = true
  @BeanProperty var wiringGutters: Boolean         = true

  // play
  @BeanProperty var playConfigKeys: Boolean = true
  @BeanProperty var playMessages: Boolean   = true
  @BeanProperty var playGuice: Boolean      = true

  // inspections
  /** Fully qualified class names (or FQN prefixes ending in `._`) where `unsafeRun*` is allowed. */
  @BeanProperty var unsafeRunAllowList: String =
    "cats.effect.IOApp,cats.effect.IOApp.Simple,play.api.mvc.BaseController,play.api.mvc.AbstractController,play.api.mvc.InjectedController," +
      "com.google.inject.AbstractModule,play.api.inject.Module" // DI modules run once at startup: wiring, not a smell

  // experimental
  @BeanProperty var compilerBackedCompletion: Boolean = false

  override def getState: PounceSettings = this

  override def loadState(state: PounceSettings): Unit = XmlSerializerUtil.copyBean(state, this)

  def style: ImportStyle = ImportStyle.values.find(_.toString == importStyle).getOrElse(ImportStyle.Auto)
}

object PounceSettings {
  def apply(): PounceSettings = ApplicationManager.getApplication.getService(classOf[PounceSettings])
}
