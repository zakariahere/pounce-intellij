package pounce.inspections.simplifications

class SimplifyErrorHandlingTest extends SimplificationTestBase[SimplifyErrorHandlingInspection] {
  import SimplifyErrorHandling.*

  def testOptionHighlight(): Unit = assertHighlight(attemptToOption, ce(s"io.${START}attempt.map(_.toOption)$END"))
  def testOptionFix(): Unit       = assertFix(attemptToOption, ce("io.attempt.map(_.toOption)"), ce("io.option"))
  def testOptionNegative(): Unit  = assertNone(ce("io.attempt.map(_.isRight)"))

  def testHandleErrorHighlight(): Unit = assertHighlight(handleErrorWithPure, ce(s"io.${START}handleErrorWith(_ => IO.pure(0))$END"))
  def testHandleErrorFix(): Unit =
    assertFix(handleErrorWithPure, ce("io.handleErrorWith(e => IO.pure(e.getMessage.length))"), ce("io.handleError(e => e.getMessage.length)"))
  def testHandleErrorNegative(): Unit = assertNone(ce("io.handleErrorWith(e => IO.println(e).as(0))"))

  def testRedeemHighlight(): Unit = assertHighlight(attemptMapFold, ce(s"io.${START}attempt.map(_.fold(_ => 0, _ + 1))$END"))
  def testRedeemFix(): Unit       = assertFix(attemptMapFold, ce("io.attempt.map(_.fold(_ => 0, _ + 1))"), ce("io.redeem(_ => 0, _ + 1)"))
  def testRedeemNegative(): Unit  = assertNone(ce("io.attempt.map(e => e.fold(_ => 0, _ => e.hashCode))"))

  def testRedeemWithFoldFix(): Unit =
    assertFix(attemptFlatMapFold, ce("io.attempt.flatMap(_.fold(_ => IO.pure(0), i => IO.pure(i)))"), ce("io.redeemWith(_ => IO.pure(0), i => IO.pure(i))"))
  def testRedeemWithFoldHighlight(): Unit =
    assertHighlight(attemptFlatMapFold, ce(s"io.${START}attempt.flatMap(_.fold(_ => IO.pure(0), i => IO.pure(i)))$END"))

  def testRedeemWithCasesHighlight(): Unit = assertHighlight(
    attemptFlatMapCases,
    ce(s"""io.${START}attempt.flatMap {
          |  case Left(e)  => IO.pure(0)
          |  case Right(i) => IO.pure(i + 1)
          |}$END""".stripMargin)
  )
  def testRedeemWithCasesFix(): Unit = assertFix(
    attemptFlatMapCases,
    ce("""io.attempt.flatMap {
         |  case Right(i) => IO.pure(i + 1)
         |  case Left(e)  => IO.raiseError(e)
         |}""".stripMargin),
    ce("io.redeemWith(e => IO.raiseError(e), i => IO.pure(i + 1))")
  )
  def testRedeemWithCasesGuardNegative(): Unit = assertNone(
    ce("""io.attempt.flatMap {
         |  case Left(e) if e.getMessage == null => IO.pure(0)
         |  case Left(e)  => IO.raiseError(e)
         |  case Right(i) => IO.pure(i)
         |}""".stripMargin)
  )

  def testMapHandleErrorHighlight(): Unit = assertHighlight(mapHandleError, ce(s"io.${START}map(_ + 1).handleError(_ => 0)$END"))
  def testMapHandleErrorFix(): Unit       = assertFix(mapHandleError, ce("io.map(_ + 1).handleError(_ => 0)"), ce("io.redeem(_ => 0, _ + 1)"))
  def testMapHandleErrorNegative(): Unit  = assertNone(ce("io.handleError(_ => 0).map(_ + 1)"))
}

class SimplifyForComprehensionTest extends SimplificationTestBase[SimplifyForComprehensionInspection] {
  import SimplifyForComprehension.*

  def testYieldItselfHighlight(): Unit = assertHighlight(yieldItself, ce(s"${START}for { a <- io } yield a$END"))
  def testYieldItselfFix(): Unit       = assertFix(yieldItself, ce("for { a <- io } yield a"), ce("io"))

  def testSingleMapHighlight(): Unit = assertHighlight(singleMap, ce(s"${START}for { a <- io } yield a + 1$END"))
  def testSingleMapFix(): Unit       = assertFix(singleMap, ce("for { a <- io } yield a + 1"), ce("io.map(a => a + 1)"))
  def testSingleOnCollectionNegative(): Unit = assertNone(ce("for { a <- ids } yield a + 1"))

  def testPureGeneratorHighlight(): Unit = assertHighlight(
    pureGenerator,
    ce(s"""for {
          |  u <- findUser(1)
          |  ${START}n <- IO.pure(u.name)$END
          |} yield n""".stripMargin)
  )
  def testPureGeneratorFix(): Unit = assertFix(
    pureGenerator,
    ce("""for {
         |  u <- findUser(1)
         |  n <- IO.pure(u.name)
         |} yield n""".stripMargin),
    ce("""for {
         |  u <- findUser(1)
         |  n = u.name
         |} yield n""".stripMargin)
  )
  /** The first enumerator must stay a generator. */
  def testFirstPureGeneratorNegative(): Unit = assertNone(
    ce("""for {
         |  n <- IO.pure(1)
         |  u <- findUser(n)
         |} yield u""".stripMargin)
  )

  def testUnitGeneratorHighlight(): Unit = assertHighlight(
    unitGenerator,
    ce(s"""for {
          |  u <- findUser(1)
          |  ${START}_ <- IO.unit$END
          |  _ <- save(u)
          |} yield u""".stripMargin)
  )
  def testUnitGeneratorFix(): Unit = assertFix(
    unitGenerator,
    ce("""for {
         |  u <- findUser(1)
         |  _ <- IO.unit
         |  _ <- save(u)
         |} yield u""".stripMargin),
    ce("""for {
         |  u <- findUser(1)
         |  _ <- save(u)
         |} yield u""".stripMargin)
  )
  def testUnitGeneratorParensFix(): Unit = assertFix(
    unitGenerator,
    ce("for (u <- findUser(1); _ <- IO.unit; _ <- save(u)) yield u"),
    ce("for (u <- findUser(1); _ <- save(u)) yield u")
  )
  def testRealStepsNegative(): Unit = assertNone(
    ce("""for {
         |  u <- findUser(1)
         |  _ <- save(u)
         |} yield u""".stripMargin)
  )
}

class SimplifyMapNTest extends SimplificationTestBase[SimplifyMapNInspection] {
  import SimplifyMapN.*

  def testTupledHighlight(): Unit = assertHighlight(mapNTuple, ce(s"(io, findUser(1)).${START}mapN((a, b) => (a, b))$END"))
  def testTupledFix(): Unit       = assertFix(mapNTuple, ce("(io, findUser(1)).mapN((a, b) => (a, b))"), ce("(io, findUser(1)).tupled"))
  def testTupledNegative(): Unit  = assertNone(ce("(io, findUser(1)).mapN((a, b) => (b, a))"))

  def testProductMapHighlight(): Unit = assertHighlight(productMap, ce(s"${START}io.product(findUser(1)).map { case (n, u) => u.id + n }$END"))
  def testProductMapFix(): Unit =
    assertFix(productMap, ce("io.product(findUser(1)).map { case (n, u) => u.id + n }"), ce("(io, findUser(1)).mapN((n, u) => u.id + n)"))
  def testProductMapNegative(): Unit = assertNone(ce("io.product(findUser(1)).map(_._1)"))

  def testTupledMapFix(): Unit =
    assertFix(tupledMap, ce("(io, io).tupled.map { case (a, b) => a + b }"), ce("(io, io).mapN((a, b) => a + b)"))
  def testTupledMapHighlight(): Unit = assertHighlight(tupledMap, ce(s"${START}(io, io).tupled.map { case (a, b) => a + b }$END"))
}

class SimplifyEitherTTest extends SimplificationTestBase[SimplifyEitherTInspection] {
  import SimplifyEitherT.*

  private def et(body: String) = ce(s"import cats.data.EitherT\nval r: EitherT[IO, String, Int] = $body\nr")

  def testRightTHighlight(): Unit = assertHighlight(pureRight, et(s"${START}EitherT(IO.pure(Right(1)))$END"))
  def testRightTFix(): Unit       = assertFix(pureRight, et("EitherT(IO.pure(Right(1)))"), et("EitherT.rightT[IO, String](1)"))

  def testLeftTHighlight(): Unit = assertHighlight(pureLeft, et(s"""${START}EitherT(IO.pure(Left("boom")))$END"""))
  def testLeftTFix(): Unit       = assertFix(pureLeft, et("""EitherT(IO.pure(Left("boom")))"""), et("""EitherT.leftT[IO, Int]("boom")"""))

  def testRightFix(): Unit = assertFix(mapRight, et("EitherT(io.map(Right(_)))"), et("EitherT.right[String](io)"))
  def testRightHighlight(): Unit = assertHighlight(mapRight, et(s"${START}EitherT(io.map(Right(_)))$END"))
  def testLeftFix(): Unit = assertFix(
    mapLeft,
    ce("import cats.data.EitherT\nval r: EitherT[IO, Int, String] = EitherT(io.map(Left(_)))\nr"),
    ce("import cats.data.EitherT\nval r: EitherT[IO, Int, String] = EitherT.left[String](io)\nr")
  )

  def testNegative(): Unit = assertNone(et("EitherT(io.map(i => if (i > 0) Right(i) else Left(\"neg\")))"))
}

class SimplifyOptionTTest extends SimplificationTestBase[SimplifyOptionTInspection] {
  import SimplifyOptionT.*

  private def ot(body: String) = ce(s"import cats.data.OptionT\nval r: OptionT[IO, Int] = $body\nr")

  def testSomeHighlight(): Unit = assertHighlight(pureSome, ot(s"${START}OptionT(IO.pure(Some(1)))$END"))
  def testSomeFix(): Unit       = assertFix(pureSome, ot("OptionT(IO.pure(Some(1)))"), ot("OptionT.some[IO](1)"))
  def testNoneFix(): Unit       = assertFix(pureNone, ot("OptionT(IO.pure(None))"), ot("OptionT.none[IO, Int]"))
  def testNoneHighlight(): Unit = assertHighlight(pureNone, ot(s"${START}OptionT(IO.pure(None))$END"))
  def testLiftFHighlight(): Unit = assertHighlight(mapSome, ot(s"${START}OptionT(io.map(Some(_)))$END"))
  def testLiftFFix(): Unit       = assertFix(mapSome, ot("OptionT(io.map(Some(_)))"), ot("OptionT.liftF(io)"))
  def testNegative(): Unit       = assertNone(ot("OptionT(io.map(i => Option.when(i > 0)(i)))"))
}

/**
 * `PounceUseCatsLiftSyntax` is registered at INFORMATION level ("no highlighting, only fix"): IntelliJ creates no
 * highlight for it, only the Alt+Enter fix at the caret. So these tests check the fix offered at the caret, which is
 * all a user ever sees, instead of highlights.
 */
class UseCatsLiftSyntaxTest extends pounce.inspections.InspectionFixtureTestBase {
  import UseCatsLiftSyntax.*
  import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
  import scala.jdk.CollectionConverters.*

  override def setUp(): Unit = {
    super.setUp()
    myFixture.enableInspections(classOf[UseCatsLiftSyntaxInspection])
  }

  private def file(body: String, imports: String = "import cats.syntax.all._\n"): String =
    s"${imports}object A {\n  def run = {\n    $body\n  }\n}"

  private def hintsAt(text: String): Seq[String] = {
    myFixture.configureByText("A.scala", text)
    myFixture.doHighlighting()
    myFixture.getAvailableIntentions.asScala.toSeq.map(_.getText)
  }

  private def assertFixAt(rule: Rule, before: String, after: String): Unit = {
    val hints  = hintsAt(before)
    val action = myFixture.getAvailableIntentions.asScala.find(_.getText == rule.hint)
    assertTrue(s"'${rule.hint}' not offered; intentions: $hints", action.isDefined)
    myFixture.launchAction(action.get)
    assertEquals(after, myFixture.getFile.getText)
  }

  def testRightOffered(): Unit = {
    val hints = hintsAt(file(s"val r: Either[String, Int] = Ri${CARET}ght(1)\n    r"))
    assertTrue(s"intentions: $hints", hints.contains(right.hint))
  }

  def testRightFix(): Unit =
    assertFixAt(right, file(s"val r: Either[String, Int] = Ri${CARET}ght(1)\n    r"), file("val r: Either[String, Int] = 1.asRight[String]\n    r"))

  def testLeftFix(): Unit =
    assertFixAt(left, file(s"""val r: Either[String, Int] = Le${CARET}ft("e")""" + "\n    r"), file("""val r: Either[String, Int] = "e".asLeft[Int]""" + "\n    r"))

  def testSomeFix(): Unit =
    assertFixAt(some, file(s"val o: Option[Int] = So${CARET}me(1)\n    o"), file("val o: Option[Int] = 1.some\n    o"))

  /** With granular imports in the file, the fix adds the granular syntax import it needs (IntelliJ then puts a blank line after the imports). */
  def testGranularImportAdded(): Unit =
    assertFixAt(
      some,
      file(s"val o: Option[Int] = So${CARET}me(1)\n    o", "import cats.syntax.functor._\n"),
      file("val o: Option[Int] = 1.some\n    o", "import cats.syntax.functor._\nimport cats.syntax.option._\n\n")
    )

  /** Without an expected `Either` / `Option` type the constructor's narrower type may matter: no suggestion. */
  def testNoExpectedTypeNegative(): Unit = {
    val hints = hintsAt(file(s"val s = So${CARET}me(1)\n    s.value"))
    assertFalse(s"intentions: $hints", hints.contains(some.hint))
  }
}
