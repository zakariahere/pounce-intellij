package pounce.completion

import com.intellij.codeInsight.completion.CompletionType
import org.junit.Assert.*
import pounce.core.settings.PounceSettings

/** A4 smart completion from the expected type, A5 curated companion / summoner completion. */
class SmartAndCompanionCompletionTest extends CompletionTestBase {

  private def smart(text: String): Seq[String] = complete(text, CompletionType.SMART)

  // ------------------------------------------------------------------------------------------------- A4 smart

  def testSmartLiftsAValueIntoOptionInsideAnEffect(): Unit = {
    smart(
      """import cats.effect.IO
        |
        |object Test {
        |  final case class User(name: String)
        |  def find(user: User): IO[Option[User]] = <caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "user.some.pure", "IO.pure", "IO.none")
    selectPounce("user.some.pure")
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object Test {
        |  final case class User(name: String)
        |  def find(user: User): IO[Option[User]] = user.some.pure[IO]<caret>
        |}
        |""".stripMargin
    )
  }

  def testSmartAsRightFillsTheErrorType(): Unit = {
    smart(
      """object Test {
        |  final case class User(name: String)
        |  def check(user: User): Either[String, User] = <caret>
        |}
        |""".stripMargin
    )
    selectPounce("user.asRight")
    checkResult(
      """import cats.syntax.all._
        |
        |object Test {
        |  final case class User(name: String)
        |  def check(user: User): Either[String, User] = user.asRight[String]<caret>
        |}
        |""".stripMargin
    )
  }

  def testSmartCompanionConstructor(): Unit = {
    smart(
      """import cats.effect.IO
        |
        |object Test {
        |  def answer: IO[Int] = <caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "IO.pure", "IO.delay", "IO.blocking")
    assertNotContains(pounceNames, "IO.unit", "IO.none")
    selectPounce("IO.blocking")
    checkResult(
      """import cats.effect.IO
        |
        |object Test {
        |  def answer: IO[Int] = IO.blocking(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testSmartTaglessUsesTheBoundInScope(): Unit = {
    smart(
      """import cats.effect.Sync
        |
        |object Test {
        |  def now[F[_]: Sync]: F[Long] = <caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "Sync[F].delay", "Sync[F].pure", "Sync[F].blocking")
    selectPounce("Sync[F].delay")
    checkResult(
      """import cats.effect.Sync
        |
        |object Test {
        |  def now[F[_]: Sync]: F[Long] = Sync[F].delay(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testSmartOptionTConstructors(): Unit = {
    smart(
      """import cats.data.OptionT
        |import cats.effect.IO
        |
        |object Test {
        |  final case class User(name: String)
        |  def find(io: IO[User]): OptionT[IO, User] = <caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "OptionT.liftF", "OptionT.some", "OptionT.none")
    selectPounce("OptionT.liftF")
    checkResult(
      """import cats.data.OptionT
        |import cats.effect.IO
        |
        |object Test {
        |  final case class User(name: String)
        |  def find(io: IO[User]): OptionT[IO, User] = OptionT.liftF(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testSmartTaglessWithoutBoundsOffersNoSummoner(): Unit = {
    smart(
      """object Test {
        |  def now[F[_]]: F[Long] = <caret>
        |}
        |""".stripMargin
    )
    assertFalse(s"$pounceNames", pounceNames.exists(_.contains("[F].")))
  }

  def testSmartSequenceOfAListOfEffects(): Unit = {
    smart(
      """import cats.effect.IO
        |
        |object Test {
        |  def all(ios: List[IO[Int]]): IO[List[Int]] = <caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "ios.sequence")
  }

  def testSmartNothingForPlainExpectedType(): Unit = {
    smart(
      """object Test {
        |  def name: String = <caret>
        |}
        |""".stripMargin
    )
    assertTrue(s"$pounceNames", pounceNames.isEmpty)
  }

  def testSmartSettingOff(): Unit = {
    PounceSettings().smartCompletion = false
    smart(
      """import cats.effect.IO
        |
        |object Test {
        |  def answer: IO[Int] = <caret>
        |}
        |""".stripMargin
    )
    assertTrue(s"$pounceNames", pounceNames.isEmpty)
  }

  // ------------------------------------------------------------------------------------------------- A5 companions

  def testIoCompanionCuratedMembersFirstAndDocumented(): Unit = {
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def run = IO.<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "blocking", "delay", "pure")
    assertTrue(LensLookup.curatedOf(element("blocking")).isDefined)
    assertTrue(presentation(element("blocking")).isItemTextBold)
    if (names.contains("asyncForIO"))
      assertTrue(s"$names", names.indexOf("blocking") < names.indexOf("asyncForIO"))
  }

  def testCompanionDoesNotAddDuplicates(): Unit = {
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def run = IO.<caret>
        |}
        |""".stripMargin
    )
    assertEquals(s"$names", 1, names.count(_ == "blocking"))
  }

  def testSyncSummonerOffersDelay(): Unit = {
    val names = complete(
      """import cats.effect.Sync
        |
        |object Test {
        |  def run[F[_]: Sync] = Sync[F].<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "delay", "blocking", "pure")
    assertEquals(s"$names", 1, names.count(_ == "delay"))
    val delay = element("delay")
    assertTrue(LensLookup.curatedOf(delay).isDefined || LensLookup.itemOf(delay).isDefined)
  }

  def testImplicitMonadThrowParameterIsASummoner(): Unit = {
    val names = complete(
      """import cats.MonadThrow
        |
        |object Test {
        |  def run[F[_]](implicit F: MonadThrow[F]) = F.<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "raiseError")
    val e = element("raiseError")
    assertTrue(LensLookup.curatedOf(e).isDefined || LensLookup.itemOf(e).isDefined)
  }

  def testCompanionSettingOff(): Unit = {
    PounceSettings().companionCompletion = false
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def run = IO.<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "blocking")
    assertTrue(LensLookup.curatedOf(element("blocking")).isEmpty)
  }
}
