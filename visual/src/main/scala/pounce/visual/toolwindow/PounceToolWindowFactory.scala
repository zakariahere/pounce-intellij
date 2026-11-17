package pounce.visual.toolwindow

import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.wm.{ToolWindow, ToolWindowManager}
import com.intellij.ui.content.{Content, ContentFactory}

/** The "Pounce" tool window: Typeclass Explorer (D5), Combinator Finder (D6), Endpoints (D7). */
final class PounceToolWindowFactory extends com.intellij.openapi.wm.ToolWindowFactory with DumbAware {

  override def createToolWindowContent(project: Project, toolWindow: ToolWindow): Unit = {
    val factory  = ContentFactory.getInstance()
    val explorer = new ExplorerPanel(project, toolWindow)
    val finder   = new FinderPanel(project)
    val endpoints = new EndpointsPanel(project)
    Seq(
      PounceToolWindowFactory.ExplorerTab  -> explorer,
      PounceToolWindowFactory.FinderTab    -> finder,
      PounceToolWindowFactory.EndpointsTab -> endpoints
    ).foreach { case (name, panel) =>
      val content = factory.createContent(panel, name, false)
      content.setDisposer(panel)
      toolWindow.getContentManager.addContent(content)
    }
  }
}

object PounceToolWindowFactory {
  val Id           = "Pounce"
  val ExplorerTab  = "Explorer"
  val FinderTab    = "Combinator Finder"
  val EndpointsTab = "Endpoints"

  /** Shows the tool window on `tab` and hands its panel to `use` once the content exists. */
  def showTab[T: scala.reflect.ClassTag](project: Project, tab: String)(use: T => Unit): Unit =
    Option(ToolWindowManager.getInstance(project).getToolWindow(Id)).foreach { tw =>
      tw.activate(() => {
        val cm = tw.getContentManager
        Option[Content](cm.findContent(tab)).foreach { c =>
          cm.setSelectedContent(c)
          c.getComponent match {
            case p: T => use(p)
            case _    =>
          }
        }
      })
    }
}
