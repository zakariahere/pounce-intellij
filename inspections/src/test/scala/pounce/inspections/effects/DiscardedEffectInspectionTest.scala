package pounce.inspections.effects

import org.jetbrains.plugins.scala.ScalaVersion
import pounce.inspections.InspectionTestBase

class DiscardedEffectInspectionTest extends InspectionTestBase[DiscardedEffectInspection] {

  override protected val description = DiscardedEffectInspection.message("IO[Unit]")

  def testStatementInBlock(): Unit = checkTextHasError(
    ce(s"""${START}IO.println("a")$END
          |IO.println("b")""".stripMargin)
  )

  def testSemicolonSequence(): Unit = checkTextHasError(
    ce(s"""${START}IO.println("a")$END; IO.println("b")""")
  )

  def testUnitReturningMethod(): Unit = checkTextHasError(
    s"""import cats.effect._
       |object A {
       |  def log(): Unit = ${START}IO.println("a")$END
       |}""".stripMargin
  )

  def testChainFix(): Unit = testQuickFix(
    ce(s"""IO.println("a")
          |IO.println("b")""".stripMargin),
    ce("""IO.println("a") >> IO.println("b")"""),
    DiscardedEffectInspection.chainHint
  )

  // ---- negatives -------------------------------------------------------------------------------------------

  def testLastExpressionIsFine(): Unit = checkTextHasNoErrors(
    ce("""val a = 1
         |IO.println("b")""".stripMargin)
  )

  def testUnitSideEffectIsFine(): Unit = checkTextHasNoErrors(
    ce("""println("a")
         |IO.println("b")""".stripMargin)
  )

  def testFutureIsNotFlagged(): Unit = checkTextHasNoErrors(
    ce("""import scala.concurrent.{ExecutionContext, Future}
         |implicit val ec: ExecutionContext = ExecutionContext.global
         |Future(1)
         |IO.println("b")""".stripMargin)
  )

  def testSequencedIsFine(): Unit = checkTextHasNoErrors(
    ce("""val greet = IO.println("a") >> IO.println("b")
         |for {
         |  _ <- IO.println("a")
         |  _ <- IO.println("b")
         |} yield ()""".stripMargin)
  )

  def testAssignmentIsFine(): Unit = checkTextHasNoErrors(
    ce("""var x: IO[Unit] = IO.unit
         |x = IO.println("a")
         |x""".stripMargin)
  )

  def testUnboundedTypeParamIsFine(): Unit = checkTextHasNoErrors(
    """object A {
      |  def run[F[_]](fu: F[Unit]): Int = {
      |    fu
      |    1
      |  }
      |}""".stripMargin
  )
}

class DiscardedEffectIfInspectionTest extends InspectionTestBase[DiscardedEffectInspection] {

  override protected val description = DiscardedEffectInspection.ifMessage("IO[Unit]")

  def testIfWithoutElse(): Unit = checkTextHasError(
    ce(s"""val debug = true
          |if (debug) ${START}IO.println("a")$END
          |IO.unit""".stripMargin)
  )

  def testWhenAFix(): Unit = testQuickFix(
    ce(s"""val debug = true
          |if (debug) IO.println("a")
          |IO.unit""".stripMargin),
    ce(s"""val debug = true
          |IO.println("a").whenA(debug)
          |IO.unit""".stripMargin),
    DiscardedEffectInspection.whenAHint
  )

  def testIfWithElseIsFine(): Unit = checkTextHasNoErrors(
    ce("""val debug = true
         |if (debug) IO.println("a") else IO.unit""".stripMargin)
  )
}

class DiscardedEffectScala3Test extends InspectionTestBase[DiscardedEffectInspection] {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_3)

  override protected val description = DiscardedEffectInspection.message("IO[Unit]")

  def testScala3Statement(): Unit = checkTextHasError(
    s"""import cats.effect.*
       |
       |object A:
       |  def greet: IO[Unit] =
       |    ${START}IO.println("a")$END
       |    IO.println("b")
       |""".stripMargin
  )

  def testScala3LastIsFine(): Unit = checkTextHasNoErrors(
    s"""import cats.effect.*
       |
       |object A:
       |  def greet: IO[Unit] =
       |    val x = 1
       |    IO.println("b")
       |""".stripMargin
  )
}

/** Tagless code: the message quotes the abstract effect type. */
class DiscardedEffectTaglessTest extends InspectionTestBase[DiscardedEffectInspection] {

  override protected val description = DiscardedEffectInspection.message("F[Unit]")

  def testTaglessStatement(): Unit = checkTextHasError(
    s"""import cats._
       |import cats.syntax.all._
       |import cats.effect._
       |object A {
       |  def run[F[_]: Sync](fu: F[Unit]): F[Unit] = {
       |    ${START}fu$END
       |    Sync[F].unit
       |  }
       |}""".stripMargin
  )
}
