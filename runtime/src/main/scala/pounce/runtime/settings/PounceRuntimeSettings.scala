package pounce.runtime.settings

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.{PersistentStateComponent, State, Storage}
import com.intellij.util.xmlb.XmlSerializerUtil

import scala.beans.BeanProperty

/**
 * Settings of the runtime pillar (debugger, test gutters). Kept in the pillar so it does not touch core's
 * [[pounce.core.settings.PounceSettings]]; shown as Settings → Languages & Frameworks → Pounce → Runtime & Debugger.
 */
@State(name = "PounceRuntimeSettings", storages = Array(new Storage("pounce-runtime.xml")))
final class PounceRuntimeSettings extends PersistentStateComponent[PounceRuntimeSettings] {

  /** Step Into skips cats / cats-effect / fs2 internals (see [[pounce.runtime.debugger.CatsSteppingFilterProvider]]). */
  @BeanProperty var steppingFilters: Boolean = true

  /** Debugger renderers for NonEmptyList, Chain, Validated, EitherT, OptionT, Ref, ... */
  @BeanProperty var catsRenderers: Boolean = true

  /** Run gutters on weaver `test("...")` / `pureTest` / `loggedTest` calls. */
  @BeanProperty var weaverGutters: Boolean = true

  override def getState: PounceRuntimeSettings = this

  override def loadState(state: PounceRuntimeSettings): Unit = XmlSerializerUtil.copyBean(state, this)
}

object PounceRuntimeSettings {
  def apply(): PounceRuntimeSettings = ApplicationManager.getApplication.getService(classOf[PounceRuntimeSettings])
}
