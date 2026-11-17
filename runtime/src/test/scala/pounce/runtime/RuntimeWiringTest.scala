package pounce.runtime

import com.intellij.execution.filters.ExceptionFilters
import com.intellij.openapi.actionSystem.{ActionManager, DefaultActionGroup}
import com.intellij.psi.search.GlobalSearchScope
import org.junit.Assert.*
import pounce.runtime.fibers.{DumpCatsEffectFibersAction, FiberDumpView, FiberSnapshot}
import pounce.runtime.settings.{PounceRuntimeConfigurable, PounceRuntimeSettings}
import pounce.testing.PounceFixtureTestBase

import scala.jdk.CollectionConverters.*

/** Settings page, Tools-menu action, and the fiber dump view's clickable frames. */
class RuntimeWiringTest extends PounceFixtureTestBase {

  def testSettingsDefaultsAndConfigurableRoundTrip(): Unit = {
    val s = PounceRuntimeSettings()
    assertTrue(s.steppingFilters && s.catsRenderers && s.weaverGutters)
    val page = new PounceRuntimeConfigurable
    assertNotNull(page.createComponent())
    assertFalse(page.isModified)
    s.catsRenderers = false
    try {
      assertTrue("the page still shows the old value", page.isModified)
      page.reset()
      assertFalse(page.isModified)
      s.catsRenderers = true
      page.apply() // writes the page's (unchecked) box back
      assertFalse(s.catsRenderers)
    } finally s.catsRenderers = true
  }

  def testSettingsPageIsRegisteredUnderPounce(): Unit = {
    val eps = com.intellij.openapi.options.Configurable.APPLICATION_CONFIGURABLE.getExtensionList.asScala
    val ours = eps.find(_.id == "pounce.settings.runtime").getOrElse(fail("pounce.settings.runtime not registered").asInstanceOf[Nothing])
    assertEquals("pounce.settings", ours.parentId)
    assertEquals(classOf[PounceRuntimeConfigurable].getName, ours.instanceClass)
  }

  def testDumpFibersActionIsInTheToolsMenu(): Unit = {
    val am     = ActionManager.getInstance()
    val action = am.getAction("Pounce.DumpCatsEffectFibers")
    assertTrue(String.valueOf(action), action.isInstanceOf[DumpCatsEffectFibersAction])
    assertEquals("Pounce: Dump Cats Effect Fibers", action.getTemplateText)
    val tools = am.getAction("ToolsMenu").asInstanceOf[DefaultActionGroup]
    assertTrue(tools.getChildActionsOrStubs.exists(a => am.getId(a) == "Pounce.DumpCatsEffectFibers"))
  }

  private val Snapshot =
    """cats.effect.IOFiber@1a WAITING
      | ├ flatMap @ com.acme.Main$.$anonfun$run$1(Main.scala:6)
      | ╰ delay @ com.acme.Main$.run(Main.scala:5)
      |
      |cats.effect.IOFiber@2b RUNNING
      | ╰ map @ com.acme.Main$.run(Main.scala:6)
      |""".stripMargin

  def testFiberViewEntriesPerFiber(): Unit = {
    val states = FiberDumpView.threadStates(FiberSnapshot.parseText(Snapshot))
    // grouped by state: RUNNING before WAITING
    assertEquals(List("IOFiber@2b  (map @ Main.scala:6)", "IOFiber@1a  (flatMap @ Main.scala:6)"), states.map(_.getName))
    assertEquals(List("running", "waiting"), states.map(_.getState))
    assertTrue(states.last.getStackTrace.contains("\tat com.acme.Main$.run(Main.scala:5) [delay]"))
  }

  def testRenderedFramesAreSourceLinks(): Unit = {
    myFixture.addFileToProject(
      "com/acme/Main.scala",
      """package com.acme
        |
        |import cats.effect._
        |
        |object Main extends IOApp.Simple {
        |  def run: IO[Unit] = IO(1).flatMap(n =>
        |    IO.println(n))
        |}
        |""".stripMargin
    )
    val text    = FiberSnapshot.render(FiberSnapshot.parseText(Snapshot), Nil)
    val filters = ExceptionFilters.getFilters(getProject, GlobalSearchScope.allScope(getProject)).asScala.toList
    assertFalse(filters.isEmpty)
    val frameLines = text.linesIterator.filter(_.startsWith("\tat ")).toList
    assertEquals(3, frameLines.size)
    frameLines.foreach { line =>
      val withNl = line + "\n"
      val links = filters.flatMap(f => Option(f.applyFilter(withNl, withNl.length))).flatMap(r => r.getResultItems.asScala).filter(_.getHyperlinkInfo != null)
      assertTrue(s"no hyperlink for: $line", links.nonEmpty)
      val item = links.head
      assertTrue(withNl.substring(item.getHighlightStartOffset, item.getHighlightEndOffset).contains("Main.scala"))
    }
  }
}
