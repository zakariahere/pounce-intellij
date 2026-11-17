package pounce.visual

import org.junit.Assert.*

import scala.jdk.CollectionConverters.*

class TypeAliasIntentionTest extends VisualTestBase {

  private def intentionTexts: Seq[String] = myFixture.getAvailableIntentions.asScala.map(_.getText).toSeq

  private val Header =
    """import cats.effect.IO
      |import cats.data.EitherT
      |sealed trait AppError
      |final case class User(id: Int)
      |""".stripMargin

  def testIntroduceAliasReplacesOccurrences(): Unit = {
    configureFromFileText(
      Header +
        """object Users {
          |  def find(id: Int): EitherT[IO, App<caret>Error, User] = ???
          |  def save(u: User): EitherT[IO, AppError, Unit] = ???
          |  def other: EitherT[IO, String, Int] = ???
          |}
          |""".stripMargin
    )
    val action = myFixture.findSingleIntention("Introduce type alias AppIO[A] for EitherT[IO, AppError, *]")
    myFixture.launchAction(action)
    myFixture.checkResult(
      Header +
        """object Users {
          |  type AppIO[A] = EitherT[IO, AppError, A]
          |
          |  def find(id: Int): AppIO[User] = ???
          |  def save(u: User): AppIO[Unit] = ???
          |  def other: EitherT[IO, String, Int] = ???
          |}
          |""".stripMargin
    )
  }

  def testNotOfferedForPlainTypes(): Unit = {
    configureFromFileText(
      Header +
        """object Users {
          |  def all: Map[String, Either[AppError, Us<caret>er]] = ???
          |  def io: IO[User] = ???
          |}
          |""".stripMargin
    )
    assertFalse(intentionTexts.toString, intentionTexts.exists(_.startsWith("Introduce type alias")))
  }

  def testUseExistingAlias(): Unit = {
    configureFromFileText(
      Header +
        """object Users {
          |  type AppIO[A] = EitherT[IO, AppError, A]
          |  def find(id: Int): EitherT[IO, AppError, Us<caret>er] = ???
          |}
          |""".stripMargin
    )
    assertFalse(intentionTexts.toString, intentionTexts.exists(_.startsWith("Introduce type alias")))
    myFixture.launchAction(myFixture.findSingleIntention("Use type alias AppIO[User]"))
    myFixture.checkResult(
      Header +
        """object Users {
          |  type AppIO[A] = EitherT[IO, AppError, A]
          |  def find(id: Int): AppIO[User] = ???
          |}
          |""".stripMargin
    )
  }

  def testUseAliasNotOfferedForDifferentStack(): Unit = {
    configureFromFileText(
      Header +
        """object Users {
          |  type AppIO[A] = EitherT[IO, AppError, A]
          |  def find(id: Int): EitherT[IO, String, Us<caret>er] = ???
          |}
          |""".stripMargin
    )
    assertFalse(intentionTexts.toString, intentionTexts.exists(_.startsWith("Use type alias")))
  }
}
