package pounce.visual

import com.intellij.openapi.application.ReadAction
import org.junit.Assert.*
import pounce.visual.finder.{CombinatorFinder, FinderPrefill, HoleFiller}
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

  /**
   * `F[_]: Sync` fixes the error type to `Throwable`, so a repository is no error to raise. Seen by hand in the
   * sample: `repo.raiseError` and `ApplicativeError[F].raiseError(repo)` were ranked above `ids.traverse(???)`.
   */
  def testFillHoleRaisesOnlyThrowables(): Unit = {
    configureFromFileText(
      """import cats.effect.Sync
        |trait Repo[F[_]] { def find(id: Long): F[Option[String]] }
        |final class Svc[F[_]: Sync](repo: Repo[F]) {
        |  def all(ids: List[Long]): F[List[Option[String]]] = <caret>???
        |}
        |""".stripMargin
    )
    val found = fillCandidates()
    assertFalse(found.toString, found.exists(_.contains("raiseError")))
    assertFalse(found.toString, found.exists(_.contains("liftTo")))
    assertEquals(found.toString, "ids.traverse(???)", found.head)
  }

  /** The class check must not reject a user exception that the static name rules cannot recognise. */
  def testFillHoleStillRaisesAUserException(): Unit = {
    configureFromFileText(
      """import cats.effect.Sync
        |final class AppFailure(msg: String) extends RuntimeException(msg)
        |final class Svc[F[_]: Sync] {
        |  def fail(err: AppFailure): F[Int] = <caret>???
        |}
        |""".stripMargin
    )
    val found = fillCandidates()
    assertTrue(found.toString, found.exists(_.startsWith("err.raiseError")))
  }

  private def fillCandidates(): Seq[String] =
    ReadAction.compute[Seq[String], RuntimeException] { () =>
      val element = getFile.findElementAt(getEditor.getCaretModel.getOffset)
      val hole    = HoleFiller.holeAt(element).getOrElse(fail("no ??? at the caret").asInstanceOf[Nothing])
      HoleFiller.candidates(hole).map(_.expression)
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
