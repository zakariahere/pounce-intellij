package pounce.visual

import org.junit.Assert.*
import pounce.visual.finder.{CombinatorFinder, FinderPrefill}
import pounce.visual.toolwindow.{EndpointsPanel, FinderPanel}

import scala.jdk.CollectionConverters.*

class CombinatorFinderTest extends VisualTestBase {

  private def find(have: String, want: String): Seq[String] = {
    val (hs, w) = FinderPanel.parse(have, want).fold(e => fail(e).asInstanceOf[Nothing], identity)
    CombinatorFinder.search(hs, w).map(_.expression)
  }

  def testTraverseFromListToEffectOfList(): Unit = {
    val found = find("xs: List[A]", "IO[List[B]]")
    assertTrue(found.toString, found.take(3).contains("xs.traverse(???)"))
  }

  def testBothValuesUsedFirst(): Unit = {
    val found = find("xs: List[A]; f: A => IO[B]", "IO[List[B]]")
    assertEquals(found.toString, "xs.traverse(f)", found.head)
  }

  def testSequenceAndVoid(): Unit = {
    assertTrue(find("xs: List[IO[A]]", "IO[List[A]]").take(3).contains("xs.sequence"))
    val void = find("fa: F[A]", "F[Unit]")
    assertTrue(void.toString, void.take(3).contains("fa.void"))
  }

  def testOneStepComposition(): Unit = {
    val found = find("xs: List[IO[A]]", "IO[Option[A]]")
    assertTrue(found.toString, found.exists(_.startsWith("xs.sequence.map(")))
  }

  def testNoIllTypedSuggestions(): Unit = {
    val list = find("xs: List[A]", "List[Unit]")
    assertTrue(list.toString, list.contains("xs.void"))
    assertFalse(list.toString, list.exists(_.contains(".start")))
    // a polymorphic A cannot become a String by itself (only through a function still to write: x.tailRecM(???))
    val fromA = find("x: A", "IO[String]")
    assertFalse(fromA.toString, fromA.exists(e => e.startsWith("x.") && !e.contains("???")))
    assertFalse(fromA.toString, fromA.exists(_.contains("raiseError")))
    // chains start from a value, not from a hole
    assertFalse(fromA.toString, fromA.exists(_.startsWith("???.")))
  }

  def testParseErrors(): Unit = {
    assertTrue(FinderPanel.parse("xs: List[", "IO[A]").isLeft)
    assertTrue(FinderPanel.parse("xs: List[A]", "").isLeft)
  }

  // ---------------------------------------------------------------------------------------------------- prefill

  def testPrefillFromSelection(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |object T {
        |  def load(ids: List[Int]): List[Int] = <selection>ids</selection>
        |}
        |""".stripMargin
    )
    assertEquals(FinderPrefill.Prefill("ids", "List[Int]", "List[Int]"), FinderPrefill.at(getFile, getEditor))
  }

  def testPrefillFromHole(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |object T {
        |  def run(n: Int): IO[Unit] = <caret>???
        |}
        |""".stripMargin
    )
    assertEquals(FinderPrefill.Prefill("", "", "IO[Unit]"), FinderPrefill.at(getFile, getEditor))
  }

  def testPanelsConstructHeadless(): Unit = {
    configureFromFileText("object T")
    val finder = new FinderPanel(getProject)
    val endpoints = new EndpointsPanel(getProject)
    try {
      finder.prefill(FinderPrefill.Prefill("xs", "List[A]", "IO[List[B]]"))
    } finally {
      com.intellij.openapi.util.Disposer.dispose(finder)
      com.intellij.openapi.util.Disposer.dispose(endpoints)
    }
  }

  // ---------------------------------------------------------------------------------------------------- fill ???

  def testFillHoleWithTraverse(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |object T {
        |  final case class User(id: Int)
        |  def load(ids: List[Int], find: Int => IO[User]): IO[List[User]] = <caret>???
        |}
        |""".stripMargin
    )
    val action = myFixture.getAvailableIntentions.asScala.find(_.getText.startsWith("Fill ??? with")).getOrElse(fail("no intention").asInstanceOf[Nothing])
    assertTrue(action.getText, action.getText.contains("ids.traverse(find)"))
    myFixture.launchAction(action)
    myFixture.checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object T {
        |  final case class User(id: Int)
        |  def load(ids: List[Int], find: Int => IO[User]): IO[List[User]] = ids.traverse(find)
        |}
        |""".stripMargin
    )
  }

  def testNoFillWithoutMatchingValues(): Unit = {
    configureFromFileText(
      """object T {
        |  def f(n: Int): java.io.File = <caret>???
        |}
        |""".stripMargin
    )
    assertFalse(myFixture.getAvailableIntentions.asScala.exists(_.getText.startsWith("Fill ???")))
  }
}
