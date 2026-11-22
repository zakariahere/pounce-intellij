package pounce.completion

import org.junit.Assert.*

/** A1: syntax completion without imports, filtered by the instances that exist. */
class SyntaxCompletionTest extends CompletionTestBase {

  def testTraverseOnListWithoutImportAddsImport(): Unit = {
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def fetch(id: Int): IO[String] = IO.pure("a")
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "traverse", "traverse_")
    selectPounce("traverse")
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object Test {
        |  def fetch(id: Int): IO[String] = IO.pure("a")
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.traverse(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testImportStyleFollowsProject(): Unit = {
    myFixture.addFileToProject("Other.scala", "import cats.implicits._\nobject Other { val x = 1 }\n")
    complete(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.foldM<caret>
        |}
        |""".stripMargin
    )
    selectPounce("foldMap")
    checkResult(
      """import cats.implicits._
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.foldMap(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testOrderSyntaxOnUserTypeAddsImport(): Unit = {
    val names = complete(
      """import cats.Order
        |
        |object Test {
        |  final case class Contract(id: String, premium: BigDecimal)
        |  object Contract { implicit val byPremium: Order[Contract] = Order.by(_.premium) }
        |  val a = Contract("C1", 120)
        |  def run = a.comp<caret>
        |}
        |""".stripMargin
    )
    assertContains(names, "compare", "comparison")
    selectPounce("compare")
    checkResult(
      """import cats.Order
        |import cats.syntax.all._
        |
        |object Test {
        |  final case class Contract(id: String, premium: BigDecimal)
        |  object Contract { implicit val byPremium: Order[Contract] = Order.by(_.premium) }
        |  val a = Contract("C1", 120)
        |  def run = a.compare(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testNativeCompareIsNotDuplicated(): Unit = {
    val names = complete(
      """object Test {
        |  val n: Int = 1
        |  def run = n.comp<caret>
        |}
        |""".stripMargin
    )
    assertEquals(s"compare once in $names", 1, names.count(_ == "compare"))
  }

  def testNoDuplicatesWhenImportPresent(): Unit = {
    val names = complete(
      """import cats.syntax.all._
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.<caret>
        |}
        |""".stripMargin
    )
    assertEquals(s"traverse once in $names", 1, names.count(_ == "traverse"))
    assertEquals(s"foldMap once in $names", 1, names.count(_ == "foldMap"))
  }

  def testIoReceiverHasNoTraverse(): Unit = {
    val names = complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(names, "traverse", "sequence", "foldMap")
    assertContains(names, "attempt", "void")
  }

  def testTaglessFunctorOffersMapFamilyOnly(): Unit = {
    val names = complete(
      """import cats.Functor
        |
        |object Test {
        |  def run[F[_]: Functor](fa: F[Int]) = fa.<caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "map", "void", "as", "fproduct")
    assertNotContains(names, "flatMap", "flatTap", ">>", "traverse", "attempt")
  }

  def testTaglessMonadOffersFlatMapFamily(): Unit = {
    complete(
      """import cats.Monad
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int]) = fa.<caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "map", "flatMap", "flatTap", ">>", "whenA")
    assertNotContains(pounceNames, "attempt", "traverse")
  }

  def testTaglessFlatMapInsertsAndImports(): Unit = {
    complete(
      """import cats.Monad
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int]) = fa.flatT<caret>
        |}
        |""".stripMargin
    )
    selectPounce("flatTap")
    checkResult(
      """import cats.Monad
        |import cats.syntax.all._
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int]) = fa.flatTap(<caret>)
        |}
        |""".stripMargin
    )
  }

  def testOperatorDropsTheDot(): Unit = {
    complete(
      """import cats.Monad
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int], fb: F[String]) = fa.<caret>
        |}
        |""".stripMargin
    )
    selectPounce(">>")
    checkResult(
      """import cats.Monad
        |import cats.syntax.all._
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int], fb: F[String]) = fa >> <caret>
        |}
        |""".stripMargin
    )
  }

  def testUnboundedTaglessIsGreyedAndRankedLow(): Unit = {
    complete(
      """object Test {
        |  def run[F[_]](fa: F[Int]) = fa.<caret>
        |}
        |""".stripMargin
    )
    val map = pounceElement("map")
    assertTrue("unknown answers are greyed", presentation(map).getItemTextForeground != null &&
      presentation(map).getItemTextForeground != java.awt.Color.BLACK)
    assertTrue(LensLookup.itemOf(map).get.isUnknown)
  }

  def testOptionAndEitherSpecificSyntax(): Unit = {
    complete(
      """object Test {
        |  val e: Either[String, Int] = Right(1)
        |  def run = e.<caret>
        |}
        |""".stripMargin
    )
    assertContains(pounceNames, "leftMap", "toValidatedNel")
    assertNotContains(pounceNames, "toNel")
  }

  def testPresentationShowsInstancesAndResultType(): Unit = {
    complete(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    val p = presentation(pounceElement("traverse"))
    assertTrue(p.isItemTextBold)
    assertTrue(p.getTailText, p.getTailText.contains("Traverse[List]"))
    assertTrue(p.getTailText, p.getTailText.contains("cats"))
    assertEquals("G[List[B]]", p.getTypeText)
  }

  def testDisabledSettingOffersNothing(): Unit = {
    pounce.core.settings.PounceSettings().syntaxCompletion = false
    complete(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(pounceNames, "traverse", "foldMap")
  }

  def testCompanionOfUserClassGetsNoLifts(): Unit = {
    complete(
      """final case class User(name: String)
        |object Test {
        |  def run = User.<caret>
        |}
        |""".stripMargin
    )
    assertNotContains(pounceNames, "some", "asRight", "pure")
  }
}
