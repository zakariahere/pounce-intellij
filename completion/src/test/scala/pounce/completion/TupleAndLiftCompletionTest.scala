package pounce.completion

import org.junit.Assert.*
import pounce.core.settings.PounceSettings

/** A2 tuple completion and A3 lift completion. */
class TupleAndLiftCompletionTest extends CompletionTestBase {

  // ------------------------------------------------------------------------------------------------- A2 tuples

  def testMapNInsertsArityCorrectLambda(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  val fb: IO[String] = IO.pure("x")
        |  def run = (fa, fb).mapN<caret>
        |}
        |""".stripMargin
    )
    selectPounce("mapN")
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  val fb: IO[String] = IO.pure("x")
        |  def run = (fa, fb).mapN((a, b) => <caret>)
        |}
        |""".stripMargin
    )
  }

  def testMapNOnTriple(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  def run = (fa, fa, fa).mapN<caret>
        |}
        |""".stripMargin
    )
    selectPounce("mapN")
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  def run = (fa, fa, fa).mapN((a, b, c) => <caret>)
        |}
        |""".stripMargin
    )
  }

  def testTupleOfEffectsOffersTheFamilyOnce(): Unit = {
    // IO's companion brings parMapN / parTupled into implicit scope, so the Scala plugin offers those itself; the
    // Cats Lens adds mapN / tupled (which need cats syntax) and nothing twice.
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  val fb: IO[String] = IO.pure("x")
        |  def run = (fa, fb).<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "mapN", "tupled", "parMapN", "parTupled")
    Seq("mapN", "tupled", "parMapN", "parTupled").foreach(n => assertEquals(s"$n in $names", 1, names.count(_ == n)))
    assertContains(pounceNames, "mapN", "tupled")
  }

  def testTupleOfPlainValuesHasNoMapN(): Unit = {
    complete(
      """object Test {
        |  def run = (1, "x").<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(pounceNames, "mapN", "tupled", "parMapN")
  }

  def testTupleSettingOff(): Unit = {
    PounceSettings().tupleCompletion = false
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val fa: IO[Int] = IO.pure(1)
        |  def run = (fa, fa).<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(pounceNames, "mapN", "tupled")
  }

  // ------------------------------------------------------------------------------------------------- A3 lifts

  private def pounceIndex(name: String): Int = {
    val i = pounceNames.indexOf(name)
    assertTrue(s"'$name' not offered: $pounceNames", i >= 0)
    i
  }

  def testExpectedEitherRanksAsRightFirst(): Unit = {
    complete(
      """object Test {
        |  val n: Int = 1
        |  val e: Either[String, Int] = n.<caret>
        |}
        |""".stripMargin
    )
    assertTrue(s"$pounceNames", pounceIndex("asRight") < pounceIndex("some"))
    assertTrue(s"$pounceNames", pounceIndex("asRight") < pounceIndex("asLeft"))
    assertTrue(s"$pounceNames", pounceIndex("asRight") < pounceIndex("pure"))
  }

  def testExpectedOptionRanksSomeFirst(): Unit = {
    complete(
      """object Test {
        |  val n: Int = 1
        |  val o: Option[Int] = n.<caret>
        |}
        |""".stripMargin
    )
    assertTrue(s"$pounceNames", pounceIndex("some") < pounceIndex("asRight"))
  }

  def testExpectedEffectRanksPureFirst(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val n: Int = 1
        |  val io: IO[Int] = n.<caret>
        |}
        |""".stripMargin
    )
    assertTrue(s"$pounceNames", pounceIndex("pure") < pounceIndex("some"))
  }

  def testAsRightFillsTheErrorTypeFromTheExpectedType(): Unit = {
    complete(
      """object Test {
        |  val n: Int = 1
        |  val e: Either[String, Int] = n.asRi<caret>
        |}
        |""".stripMargin
    )
    selectPounce("asRight")
    checkResult(
      """import cats.syntax.all._
        |
        |object Test {
        |  val n: Int = 1
        |  val e: Either[String, Int] = n.asRight[String]<caret>
        |}
        |""".stripMargin
    )
  }

  def testAsRightWithoutExpectedTypeLeavesTheCaretInTheTypeArgument(): Unit = {
    complete(
      """object Test {
        |  val n: Int = 1
        |  def run = n.asRi<caret>
        |}
        |""".stripMargin
    )
    selectPounce("asRight")
    checkResult(
      """import cats.syntax.all._
        |
        |object Test {
        |  val n: Int = 1
        |  def run = n.asRight[<caret>]
        |}
        |""".stripMargin
    )
  }

  def testPresentationShowsSubstitutedResultType(): Unit = {
    complete(
      """object Test {
        |  val n: Int = 1
        |  val e: Either[String, Int] = n.<caret>
        |}
        |""".stripMargin
    )
    assertEquals("Either[String, Int]", presentation(pounceElement("asRight")).getTypeText)
  }

  def testLiftSettingOff(): Unit = {
    PounceSettings().liftCompletion = false
    complete(
      """object Test {
        |  val n: Int = 1
        |  def run = n.<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(pounceNames, "some", "asRight", "pure")
  }

  def testCaseObjectIsAValue(): Unit = {
    complete(
      """object Test {
        |  sealed trait AppError
        |  case object NotFound extends AppError
        |  def run: Either[AppError, Int] = NotFound.<caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "asLeft")
  }
}
