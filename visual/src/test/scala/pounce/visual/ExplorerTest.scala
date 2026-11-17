package pounce.visual

import com.intellij.openapi.util.Disposer
import com.intellij.toolWindow.ToolWindowHeadlessManagerImpl
import org.junit.Assert.*
import pounce.core.StaticInstances.Answer
import pounce.visual.explorer.TypeclassExplorer
import pounce.visual.toolwindow.ExplorerPanel

class ExplorerTest extends VisualTestBase {

  private def reportAtCaret(text: String): TypeclassExplorer.Report = {
    configureFromFileText(text)
    val s = TypeclassExplorer.subjectAt(getFile, getEditor.getCaretModel.getOffset).getOrElse(fail("no subject").asInstanceOf[Nothing])
    TypeclassExplorer.explore(s.scType, s.place)
  }

  def testListUnderCaret(): Unit = {
    val r = reportAtCaret(
      """object T {
        |  val ids: List[Int] = List(1)
        |  def use = i<caret>ds.headOption
        |}
        |""".stripMargin
    )
    assertEquals("List[Int]", r.typeText)
    assertEquals("F[A]  (F = List, A = Int)", r.shape)
    assertEquals(Some(Answer.Yes), r.answerFor("Traverse"))
    assertEquals(Some(Answer.Yes), r.answerFor("Monad"))
    assertEquals(Some(Answer.Yes), r.answerFor("Monoid"))
    assertEquals(Some(Answer.No), r.answerFor("Async"))
    assertEquals(Some(Answer.Yes), r.answerFor("TraverseFilter"))
    // TraverseFilter extends Traverse, so for List it is the strongest instance and Traverse is implied by it.
    assertTrue(r.strongest.toString, r.strongest.contains("TraverseFilter"))
    assertFalse("Traverse is implied by TraverseFilter", r.strongest.contains("Traverse"))
    assertFalse("Functor is implied, not strongest", r.strongest.contains("Functor"))
    val ops = r.ops.map(_.op.name)
    assertTrue(ops.toString, ops.contains("traverse"))
    assertTrue(ops.toString, ops.contains("foldMap"))
    assertFalse(ops.toString, ops.contains("attempt"))
    assertFalse(ops.toString, ops.contains("start"))
  }

  def testIoAndCallReceiver(): Unit = {
    val r = reportAtCaret(
      """import cats.effect.IO
        |object T {
        |  val io: IO[Int] = IO.pure(1)
        |  def use = io.ma<caret>p(_ + 1)
        |}
        |""".stripMargin
    )
    // the caret is on `map`: the receiver `io` is explored
    assertEquals("IO[Int]", r.typeText)
    assertEquals(Some(Answer.Yes), r.answerFor("Async"))
    assertEquals(Some(Answer.No), r.answerFor("Traverse"))
    assertTrue(r.ops.map(_.op.name).contains("attempt"))
  }

  def testTypedTypeAndUserInstances(): Unit = {
    configureFromFileText(
      """import cats.Show
        |final case class User(name: String)
        |object User { implicit val show: Show[User] = Show.show(_.name) }
        |object T { def use = 1<caret> }
        |""".stripMargin
    )
    val place = getFile.findElementAt(getEditor.getCaretModel.getOffset)
    val either = TypeclassExplorer.explore(TypeclassExplorer.parseType("Either[String, Int]", place).get, place)
    assertEquals("F[A]  (F = Either[String, *], A = Int)", either.shape)
    assertEquals(Some(Answer.Yes), either.answerFor("MonadError"))
    val user = TypeclassExplorer.explore(TypeclassExplorer.parseType("User", place).get, place)
    assertEquals(Some(Answer.Yes), user.answerFor("Show"))
    assertEquals(Some(Answer.No), user.answerFor("Monoid"))
    assertEquals("no higher-kinded rows for a proper type", None, user.answerFor("Functor"))
  }

  def testPanelRendersReport(): Unit = {
    val r = reportAtCaret(
      """object T {
        |  val ids: List[Int] = List(1)
        |  def use = i<caret>ds
        |}
        |""".stripMargin
    )
    val panel = new ExplorerPanel(getProject, new ToolWindowHeadlessManagerImpl.MockToolWindow(getProject))
    try panel.show(r)
    finally Disposer.dispose(panel)
  }
}
