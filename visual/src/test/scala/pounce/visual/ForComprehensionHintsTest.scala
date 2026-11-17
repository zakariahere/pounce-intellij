package pounce.visual

import org.junit.Assert.*
import pounce.core.settings.PounceSettings
import pounce.visual.InlayTestSupport.*
import pounce.visual.hints.ForComprehensionInlayProvider

class ForComprehensionHintsTest extends VisualTestBase {

  private def hintsIn(text: String): (String, Seq[Recorded]) = {
    configureFromFileText(text)
    val hints = collect(new ForComprehensionInlayProvider, getFile, getEditor)
    (render(getFile.getText, hints), hints)
  }

  private val Header =
    """import cats._
      |import cats.syntax.all._
      |import cats.effect._
      |import cats.data.EitherT
      |final case class User(id: Int, name: String)
      |final case class Order(id: Int)
      |sealed trait AppError
      |""".stripMargin

  def testIoGenerators(): Unit = {
    val (rendered, _) = hintsIn(
      Header +
        """object T {
          |  def findUser(id: Int): IO[User] = IO.pure(User(id, "a"))
          |  def findOrder(u: User): IO[Order] = IO.pure(Order(u.id))
          |  def run(id: Int) = for {
          |    user  <- findUser(id)
          |    order <- findOrder(user)
          |    _     <- IO.println(order)
          |  } yield order
          |}
          |""".stripMargin
    )
    assertTrue(rendered, rendered.contains("for‹IO› {"))
    assertTrue(rendered, rendered.contains("user  <- findUser(id)‹IO · User›"))
    assertTrue(rendered, rendered.contains("order <- findOrder(user)‹IO · Order›"))
    // wildcard steps repeat nothing useful
    assertTrue(rendered, rendered.contains("_     <- IO.println(order)\n"))
  }

  def testTaglessShowsBounds(): Unit = {
    val (rendered, _) = hintsIn(
      Header +
        """trait Repo[F[_]] { def find(id: Int): F[User] }
          |class Service[F[_]: Sync](repo: Repo[F]) {
          |  def run(id: Int): F[String] = for {
          |    user <- repo.find(id)
          |    name <- Sync[F].delay(user.name)
          |  } yield name
          |}
          |""".stripMargin
    )
    assertTrue(rendered, rendered.contains("for‹F: Sync› {"))
    assertTrue(rendered, rendered.contains("user <- repo.find(id)‹F · User›"))
    assertTrue(rendered, rendered.contains("name <- Sync[F].delay(user.name)‹F · String›"))
  }

  def testTransformerAndAlias(): Unit = {
    val (rendered, _) = hintsIn(
      Header +
        """object T {
          |  type AppIO[A] = EitherT[IO, AppError, A]
          |  def findUser(id: Int): EitherT[IO, AppError, User] = EitherT.rightT[IO, AppError](User(id, "a"))
          |  def other(id: Int): EitherT[IO, String, Int] = EitherT.rightT[IO, String](id)
          |  def run(id: Int) = for {
          |    user <- findUser(id)
          |  } yield user
          |  def run2(id: Int) = for {
          |    n <- other(id)
          |  } yield n
          |}
          |""".stripMargin
    )
    // the alias in scope is used for the matching stack...
    assertTrue(rendered, rendered.contains("for‹AppIO› {"))
    assertTrue(rendered, rendered.contains("user <- findUser(id)‹AppIO · User›"))
    // ...but not for a different stack
    assertTrue(rendered, rendered.contains("n <- other(id)‹EitherT[IO, String, *] · Int›"))
  }

  def testMismatchedStepIsMarked(): Unit = {
    val (rendered, hints) = hintsIn(
      Header +
        """object T {
          |  def findUser(id: Int): IO[User] = IO.pure(User(id, "a"))
          |  def lookup(u: User): Option[String] = Some(u.name)
          |  def run(id: Int) = for {
          |    user <- findUser(id)
          |    name <- lookup(user)
          |  } yield name
          |}
          |""".stripMargin
    )
    assertTrue(rendered, rendered.contains("name <- lookup(user)‹Option · String ≠ IO›"))
    assertTrue(hints.exists(_.tooltip.contains("all steps must share one effect")))
  }

  def testNoHintsForCollectionsOrOptions(): Unit = {
    val (_, hints) = hintsIn(
      Header +
        """object T {
          |  val xs = for { a <- List(1, 2); b <- List(3) } yield a + b
          |  val o  = for { a <- Option(1); b <- Some(2) } yield a + b
          |}
          |""".stripMargin
    )
    assertEquals(Nil, hints.toList)
  }

  def testTypedPatternAndSettingOff(): Unit = {
    val text = Header +
      """object T {
        |  def findUser(id: Int): IO[User] = IO.pure(User(id, "a"))
        |  def run(id: Int) = for {
        |    user: User <- findUser(id)
        |  } yield user
        |}
        |""".stripMargin
    val (rendered, _) = hintsIn(text)
    assertTrue(rendered, rendered.contains("user: User <- findUser(id)\n"))
    val settings = PounceSettings()
    settings.forComprehensionHints = false
    try assertEquals(Nil, hintsIn(text)._2.toList)
    finally settings.forComprehensionHints = true
  }
}
