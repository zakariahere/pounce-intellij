package pounce.completion

import org.junit.Assert.*

/** `5.seconds` and friends from `scala.concurrent.duration`, offered without the import. */
class DurationCompletionTest extends CompletionTestBase {

  private val Units = Set("seconds", "second", "millis", "milliseconds", "minutes", "minute", "hours", "hour", "days", "day")

  def testIntSecondsAddsTheDurationImport(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def run = IO.sleep(5.sec<caret>)
        |}
        |""".stripMargin
    )
    selectPounce("seconds")
    checkResult(
      """import cats.effect.IO
        |import scala.concurrent.duration._
        |
        |object Test {
        |  def run = IO.sleep(5.seconds<caret>)
        |}
        |""".stripMargin
    )
  }

  def testLongAndDoubleReceivers(): Unit = {
    complete(
      """object Test {
        |  val n: Long = 3L
        |  def run = n.min<caret>
        |}
        |""".stripMargin
    )
    assertTrue(pounceNames.toString, pounceNames.contains("minutes"))
    assertEquals("Long", LensLookup.itemOf(pounceElement("minutes")).get.receiverText)

    complete(
      """object Test {
        |  def run = 1.5.hou<caret>
        |}
        |""".stripMargin
    )
    assertTrue(pounceNames.toString, pounceNames.contains("hours"))
  }

  def testNotOfferedOnOtherTypes(): Unit = {
    complete(
      """object Test {
        |  def run = "five".sec<caret>
        |}
        |""".stripMargin
    )
    assertFalse(pounceNames.toString, pounceNames.contains("seconds"))
  }

  /**
   * With the import there, the Scala plugin offers `seconds` itself (twice, in fact: through DurationInt and, by
   * numeric widening, DurationLong). Pounce adds nothing on top.
   */
  def testNoDuplicateWhenAlreadyImported(): Unit = {
    val names = complete(
      """import scala.concurrent.duration._
        |
        |object Test {
        |  def run = 5.seco<caret>
        |}
        |""".stripMargin
    )
    assertTrue(names.toString, names.contains("seconds"))
    assertTrue(pounceNames.toString, pounceNames.isEmpty)
  }

  /** A lift: buried in a plain `5.` popup, first when a FiniteDuration is expected. */
  def testExpectedFiniteDurationRanksUnitsFirst(): Unit = {
    complete(
      """import scala.concurrent.duration.FiniteDuration
        |
        |object Test {
        |  val timeout: FiniteDuration = 5.<caret>
        |}
        |""".stripMargin
    )
    val ours = pounceNames
    assertTrue(ours.toString, ours.nonEmpty && Units.contains(ours.head))
    if (ours.contains("some")) assertTrue(ours.toString, ours.indexOf("seconds") < ours.indexOf("some"))
  }
}
