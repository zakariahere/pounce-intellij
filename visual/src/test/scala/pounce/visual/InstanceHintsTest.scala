package pounce.visual

import org.junit.Assert.*
import pounce.core.settings.PounceSettings
import pounce.visual.InlayTestSupport.*
import pounce.visual.hints.{InstanceHints, InstanceInlayProvider}

class InstanceHintsTest extends VisualTestBase {

  private def withInstanceHints[T](on: Boolean)(body: => T): T = {
    val s   = PounceSettings()
    val old = s.instanceHints
    s.instanceHints = on
    try body
    finally s.instanceHints = old
  }

  private val Code =
    """import cats._
      |import cats.syntax.all._
      |import cats.effect._
      |object T {
      |  final case class User(id: Int)
      |  def findUser(id: Int): IO[User] = IO.pure(User(id))
      |  val ids: List[Int] = List(1, 2)
      |  val a = ids.traverse(findUser)
      |  val b = ids.map(_ + 1)
      |  val c = ids.combineAll
      |  val d = Option(1).filter(_ > 0)
      |}
      |""".stripMargin

  def testInstancesOfCatsCalls(): Unit = instancesOfCatsCalls()

  // bodies live outside test* methods: a by-name Unit thunk inside testX compiles to a non-public testX$$anonfun$1
  // that JUnit 3 reports as "Test method isn't public"
  private def instancesOfCatsCalls(): Unit = withInstanceHints(on = true) {
    configureFromFileText(Code)
    val hints    = collect(new InstanceInlayProvider, getFile, getEditor)
    val rendered = render(getFile.getText, hints)
    val traverse = rendered.linesIterator.find(_.contains("val a")).get
    assertTrue(rendered, traverse.contains("ids.traverse(findUser)‹"))
    assertTrue(traverse, traverse.contains("Traverse[List]"))
    assertTrue(traverse, traverse.contains("Applicative[IO]"))
    val combine = rendered.linesIterator.find(_.contains("val c")).get
    assertTrue(combine, combine.contains("Monoid[Int]"))
    // the resolved instances are clickable
    assertTrue(hints.toString, hints.exists(_.targets > 0))
    // standard library calls get nothing
    assertFalse(rendered, rendered.linesIterator.find(_.contains("val b")).get.contains("‹"))
    assertFalse(rendered, rendered.linesIterator.find(_.contains("val d")).get.contains("‹"))
  }

  def testOffByDefaultSetting(): Unit = offByDefaultSetting()

  private def offByDefaultSetting(): Unit = withInstanceHints(on = false) {
    configureFromFileText(Code)
    assertEquals(Nil, collect(new InstanceInlayProvider, getFile, getEditor).toList)
  }

  def testTaglessInstanceComesFromBound(): Unit = taglessInstanceComesFromBound()

  private def taglessInstanceComesFromBound(): Unit = withInstanceHints(on = true) {
    configureFromFileText(
      """import cats._
        |import cats.syntax.all._
        |object T {
        |  def run[F[_]: Applicative](ids: List[Int], f: Int => F[String]) = ids.traverse(f)
        |}
        |""".stripMargin
    )
    val rendered = render(getFile.getText, collect(new InstanceInlayProvider, getFile, getEditor))
    assertTrue(rendered, rendered.contains("Applicative[F]"))
  }

  def testTypeLambdaCleanup(): Unit =
    assertEquals("Monad[Either[String, *]]", InstanceHints.clean("Monad[[β$0$]Either[String, β$0$]]"))
}
