package pounce.visual

import com.intellij.codeInsight.daemon.{LineMarkerInfo, RelatedItemLineMarkerInfo}
import com.intellij.codeInsight.daemon.GutterMark
import org.junit.Assert.*
import pounce.core.settings.PounceSettings

import scala.jdk.CollectionConverters.*

class GutterTest extends VisualTestBase {

  private def gutters: Seq[GutterMark] = myFixture.findAllGutters().asScala.toSeq

  private def tooltips: Seq[String] = gutters.flatMap(g => Option(g.getTooltipText))

  /** Runs the (lazy) navigation search of a gutter and returns the target texts. */
  private def targetsOf(tooltipPrefix: String): Seq[String] = {
    val mark = gutters.find(g => Option(g.getTooltipText).exists(_.startsWith(tooltipPrefix)))
      .getOrElse(fail(s"no gutter '$tooltipPrefix' in $tooltips").asInstanceOf[Nothing])
    val info = mark.asInstanceOf[LineMarkerInfo.LineMarkerGutterIconRenderer[?]].getLineMarkerInfo
    info match {
      case r: RelatedItemLineMarkerInfo[?] => r.createGotoRelatedItems().asScala.toSeq.flatMap(i => Option(i.getElement)).map(_.getText)
      case other                           => fail(s"not navigable: $other").asInstanceOf[Nothing]
    }
  }

  def testForkBlockingAndResourceMarkers(): Unit = {
    configureFromFileText(
      """import cats.effect._
        |import cats.syntax.all._
        |object T {
        |  val io: IO[Int] = IO.pure(1)
        |  def fork = io.start
        |  def blk = IO.blocking(Thread.sleep(10))
        |  def par = List(1, 2).parTraverse(i => IO.pure(i))
        |  val res = Resource.make(IO.pure(1))(_ => IO.unit)
        |  def used = res.use(i => IO.pure(i))
        |}
        |""".stripMargin
    )
    val ts = tooltips
    assertTrue(ts.toString, ts.contains("Runs on a new fiber (.start)"))
    assertTrue(ts.toString, ts.contains("Runs on a new fiber (.parTraverse)"))
    assertTrue(ts.toString, ts.contains("Runs on the blocking thread pool (.blocking)"))
    assertTrue(ts.toString, ts.contains("Resource: navigate between acquire and release"))
    assertTrue(ts.toString, ts.contains("Resource is acquired here and released when use completes"))
    assertEquals(Seq("IO.pure(1)", "_ => IO.unit"), targetsOf("Resource: navigate"))
    assertEquals(Seq("IO.pure(1)", "_ => IO.unit"), targetsOf("Resource is acquired here"))
  }

  def testNoMarkersForLookalikes(): Unit = {
    configureFromFileText(
      """import scala.concurrent.blocking
        |object Engine {
        |  def start(): Unit = ()
        |  def make(a: Int)(b: Int): Int = a + b
        |  def use(f: Int => Int): Int = f(1)
        |}
        |object T {
        |  def a = Engine.start()
        |  def b = blocking { Thread.sleep(1) }
        |  def c = Engine.make(1)(2)
        |  def d = Engine.use(_ + 1)
        |  def e = List(1, 2).map(_ + 1)
        |}
        |""".stripMargin
    )
    val ours = tooltips.filter(t => t.startsWith("Runs on") || t.startsWith("Resource"))
    assertEquals(Nil, ours.toList)
  }

  def testSettingsDisableMarkers(): Unit = {
    val s = PounceSettings()
    s.forkGutters = false
    try {
      configureFromFileText(
        """import cats.effect._
          |object T { def fork = IO.pure(1).start }
          |""".stripMargin
      )
      assertFalse(tooltips.toString, tooltips.exists(_.startsWith("Runs on a new fiber")))
    } finally s.forkGutters = true
  }

  def testTaglessWiring(): Unit = {
    configureFromFileText(
      """import cats.effect._
        |trait UserRepo[F[_]] {
        |  def find(id: Int): F[Option[String]]
        |}
        |class DoobieUserRepo[F[_]: Sync](url: String) extends UserRepo[F] {
        |  def find(id: Int): F[Option[String]] = Sync[F].delay(None)
        |}
        |object InMemoryUserRepo extends UserRepo[IO] {
        |  def find(id: Int): IO[Option[String]] = IO.pure(None)
        |}
        |trait Plain { def x: Int }
        |class PlainImpl extends Plain { def x = 1 }
        |object Main {
        |  val repo: UserRepo[IO] = new DoobieUserRepo[IO]("jdbc:x")
        |  val other: UserRepo[IO] = InMemoryUserRepo
        |}
        |""".stripMargin
    )
    val ts = tooltips
    assertTrue(ts.toString, ts.contains("Tagless algebra UserRepo: implementations and where they are constructed"))
    assertTrue(ts.toString, ts.exists(_.startsWith("DoobieUserRepo implements UserRepo")))
    assertTrue(ts.toString, ts.exists(_.startsWith("InMemoryUserRepo implements UserRepo")))
    // not tagless: no wiring gutters
    // (the Scala plugin's own "Is mixed into PlainImpl" markers are not ours)
    assertFalse(ts.toString, ts.exists(t => t.startsWith("Tagless algebra Plain") || t.startsWith("PlainImpl implements")))

    val fromAlgebra = targetsOf("Tagless algebra UserRepo")
    assertTrue(fromAlgebra.toString, fromAlgebra.exists(_.startsWith("class DoobieUserRepo")))
    assertTrue(fromAlgebra.toString, fromAlgebra.contains("new DoobieUserRepo[IO](\"jdbc:x\")"))
    assertTrue(fromAlgebra.toString, fromAlgebra.contains("InMemoryUserRepo"))

    val fromImpl = targetsOf("DoobieUserRepo implements")
    assertTrue(fromImpl.toString, fromImpl.exists(_.startsWith("trait UserRepo")))
    assertTrue(fromImpl.toString, fromImpl.contains("new DoobieUserRepo[IO](\"jdbc:x\")"))
  }
}
