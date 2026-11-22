package pounce.inspections.tagless

import com.intellij.codeInsight.intention.IntentionAction
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import pounce.inspections.{InspectionFixtureTestBase, MessageTestBase}

import scala.jdk.CollectionConverters.*

// ---------------------------------------------------------------------------------------------------------------
// B8 (a) redundant bounds
// ---------------------------------------------------------------------------------------------------------------

class RedundantBoundInspectionTest extends MessageTestBase[RedundantBoundInspection] {
  import RedundantBoundInspection.*

  override protected def isOwnMessage(s: String): Boolean = s.startsWith("Redundant bound")

  private val H = "import cats._\nimport cats.effect._\n"

  def testContextBound(): Unit =
    assertHighlight(message("Monad", "Sync"), H + s"object A { def f[F[_]: ${START}Monad$END: Sync]: F[Unit] = Sync[F].unit }")
  def testContextBoundFix(): Unit = assertFix(
    message("Monad", "Sync"),
    removeHint("Monad"),
    H + "object A { def f[F[_]: Monad: Sync]: F[Unit] = Sync[F].unit }",
    H + "object A { def f[F[_]: Sync]: F[Unit] = Sync[F].unit }"
  )
  def testImplicitParamFix(): Unit = assertFix(
    message("Functor", "Monad"),
    removeHint("Functor"),
    H + "class Repo[F[_]](implicit M: Monad[F], Fn: Functor[F]) { def unit: F[Unit] = M.unit }",
    H + "class Repo[F[_]](implicit M: Monad[F]) { def unit: F[Unit] = M.unit }"
  )

  // ---- negatives ----
  def testIndependentBounds(): Unit = assertNone(H + "object A { def f[F[_]: Monad: Parallel]: F[Unit] = Monad[F].unit }")
  def testSingleBound(): Unit       = assertNone(H + "object A { def f[F[_]: Sync]: F[Unit] = Sync[F].unit }")
}

// ---------------------------------------------------------------------------------------------------------------
// B9 conflicting cats imports
// ---------------------------------------------------------------------------------------------------------------

class ConflictingCatsImportsInspectionTest extends MessageTestBase[ConflictingCatsImportsInspection] {
  import ConflictingCatsImportsInspection.*

  override protected def isOwnMessage(s: String): Boolean = s.startsWith("Conflicting cats imports")

  private val Body = "\nobject A { def f = List(1).map(_ + 1) }"

  def testImplicitsAndSyntaxAll(): Unit = assertHighlight(
    message("cats.implicits", "cats.syntax.all"),
    s"import ${START}cats.implicits._$END\nimport cats.syntax.all._" + Body
  )
  def testRemoveFix(): Unit = assertFix(
    message("cats.implicits", "cats.syntax.all"),
    removeHint("cats.implicits"),
    "import cats.implicits._\nimport cats.syntax.all._" + Body,
    "import cats.syntax.all._" + Body
  )
  def testGranularNextToAll(): Unit = assertHighlight(
    message("cats.syntax.functor", "cats.syntax.all"),
    s"import cats.syntax.all._\nimport ${START}cats.syntax.functor._$END" + Body
  )
  def testInstancesNextToImplicits(): Unit = assertHighlight(
    message("cats.instances.list", "cats.implicits"),
    s"import cats.implicits._\nimport ${START}cats.instances.list._$END" + Body
  )
  def testEffectSyntax(): Unit = assertHighlight(
    message("cats.effect.syntax.spawn", "cats.effect.syntax.all"),
    s"import cats.effect.syntax.all._\nimport ${START}cats.effect.syntax.spawn._$END" + Body
  )

  // ---- negatives ----
  def testSingleImport(): Unit     = assertNone("import cats.syntax.all._" + Body)
  def testGranularOnly(): Unit     = assertNone("import cats.syntax.functor._\nimport cats.syntax.flatMap._" + Body)
  def testCoreAndEffect(): Unit    = assertNone("import cats.syntax.all._\nimport cats.effect.syntax.all._" + Body)
}

// ---------------------------------------------------------------------------------------------------------------
// B11 missing instance explainer
// ---------------------------------------------------------------------------------------------------------------

class MissingInstanceInspectionTest extends MessageTestBase[MissingInstanceInspection] {
  import MissingInstanceInspection.*

  override protected def isOwnMessage(s: String): Boolean = s.startsWith("No ") && (s.contains(" instance:") || s.startsWith("No implicit"))

  private val H = "import cats._\nimport cats.syntax.all._\nimport cats.effect._\n"

  def testWeakBound(): Unit = assertHighlight(
    taglessMessage("Monad", "F", Seq("Functor")),
    H + s"object A { def load[F[_]: Functor](id: Int): F[Int] = ${START}Monad[F]$END.pure(id) }"
  )
  def testStrengthenFix(): Unit = assertFix(
    taglessMessage("Monad", "F", Seq("Functor")),
    strengthenHint("Monad"),
    H + "object A { def load[F[_]: Functor](id: Int): F[Int] = Monad[F].pure(id) }",
    H + "object A { def load[F[_]: Monad](id: Int): F[Int] = Monad[F].pure(id) }"
  )
  def testNoBoundFix(): Unit = assertFix(
    taglessMessage("Sync", "F", Nil),
    addHint("F", "Sync"),
    H + "object A { def load[F[_]](id: Int): F[Int] = Sync[F].delay(id) }",
    H + "object A { def load[F[_]: Sync](id: Int): F[Int] = Sync[F].delay(id) }"
  )

  // ---- negatives ----
  def testSatisfied(): Unit = assertNone(H + "object A { def load[F[_]: Sync](id: Int): F[Int] = Monad[F].pure(id) }")
  def testConcreteKnown(): Unit = assertNone(H + "object A { val m = Monad[Option].pure(1) }")
}

// ---------------------------------------------------------------------------------------------------------------
// B8 (b) "Strengthen bound to Monad" on the Scala plugin's unresolved-reference error
// ---------------------------------------------------------------------------------------------------------------

class StrengthenBoundFixProviderTest extends InspectionFixtureTestBase {

  private val H = "import cats._\nimport cats.syntax.all._\n"

  private def intentions(text: String): Seq[IntentionAction] = {
    myFixture.configureByText("A.scala", text)
    myFixture.doHighlighting()
    myFixture.getAvailableIntentions.asScala.toSeq
  }

  private def names(text: String): Seq[String] = intentions(text).map(_.getText)

  def testOfferedForFlatMapOnFunctor(): Unit = {
    val ns = names(H + s"object A { def f[F[_]: Functor](fa: F[Int]): F[Int] = fa.flat${CARET}Map(i => fa) }")
    assertTrue(s"intentions: $ns", ns.contains(StrengthenBoundFixProvider.strengthenText("FlatMap")) || ns.contains(StrengthenBoundFixProvider.strengthenText("Monad")))
  }

  def testApplyStrengthenToMonad(): Unit = {
    val text = H + s"object A { def f[F[_]: Functor](fa: F[Int]): F[Int] = fa.flat${CARET}Map(i => fa) }"
    val action = intentions(text).find(_.getText == StrengthenBoundFixProvider.strengthenText("Monad"))
      .orElse(intentions(text).find(_.getText == StrengthenBoundFixProvider.strengthenText("FlatMap")))
    assertTrue("strengthen action", action.isDefined)
    myFixture.launchAction(action.get)
    val result = myFixture.getFile.getText
    assertTrue(result, result.contains("def f[F[_]: Monad]") || result.contains("def f[F[_]: FlatMap]"))
  }

  def testNotOfferedWhenBoundIsEnough(): Unit = {
    val ns = names(H + s"object A { def f[F[_]: Monad](fa: F[Int]): F[Int] = fa.flat${CARET}Map(i => fa) }")
    assertFalse(s"intentions: $ns", ns.exists(_.startsWith("Strengthen bound to")))
  }

  def testImportWhenSyntaxMissing(): Unit = {
    val ns = names(s"import cats._\nobject A { def f[F[_]: Monad](fa: F[Int]): F[Int] = fa.flat${CARET}Map(i => fa) }")
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  // ---- proper receivers: Order / PartialOrder / Eq syntax on a plain value ----

  private def ordered(expr: String, instance: Boolean = true): String =
    s"""import cats.Order
       |object A {
       |  final case class Contract(id: String, premium: BigDecimal)
       |  object Contract { ${if (instance) "implicit val byPremium: Order[Contract] = Order.by(_.premium)" else ""} }
       |  val a = Contract("C1", 120)
       |  val b = Contract("C2", 95)
       |  val r = $expr
       |}""".stripMargin

  def testImportForCompareDotCall(): Unit = {
    val ns = names(ordered(s"a.comp${CARET}are(b)"))
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  def testImportForInfixCompare(): Unit = {
    val ns = names(ordered(s"a ${CARET}compare b"))
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  def testImportForInfixMin(): Unit = {
    val ns = names(ordered(s"a ${CARET}min b"))
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  def testImportForPartialOrderOperatorBackedByOrder(): Unit = {
    val ns = names(ordered(s"a ${CARET}> b"))
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  def testApplyImportFixesOperator(): Unit = {
    val action = intentions(ordered(s"a ${CARET}> b")).find(_.getText == "Import cats.syntax.all._")
    assertTrue("import action", action.isDefined)
    myFixture.launchAction(action.get)
    assertTrue(myFixture.getFile.getText, myFixture.getFile.getText.contains("import cats.syntax.all._"))
  }

  def testNoImportWithoutInstance(): Unit = {
    val ns = names(ordered(s"a ${CARET}> b", instance = false))
    assertFalse(s"intentions: $ns", ns.exists(_.startsWith("Import cats.syntax")))
  }

  def testProperOpsTable(): Unit = {
    assertEquals(Seq(Some("Order")), StrengthenBoundFixProvider.properOps("compare").map(_._2))
    assertEquals(Seq(Some("PartialOrder")), StrengthenBoundFixProvider.properOps(">").map(_._2))
    assertEquals(Seq(Some("Eq")), StrengthenBoundFixProvider.properOps("===").map(_._2))
    assertEquals(Seq(None), StrengthenBoundFixProvider.properOps("invalidNel").map(_._2))
  }

  // ---- lifts that need no instance: "error".invalidNel / ().validNel ----

  private def validation(branch: String): String =
    s"""import cats.data.ValidatedNel
       |object A {
       |  val MinYear = 2000
       |  val startYear = 1999
       |  val minYearError: ValidatedNel[String, Unit] = {
       |    if (startYear < MinYear)
       |      $branch
       |    else ().validNel
       |  }
       |}""".stripMargin

  def testImportForInvalidNel(): Unit = {
    val ns = names(validation(s"""s"Start year $${startYear} should be at least $$MinYear".invalid${CARET}Nel"""))
    assertTrue(s"intentions: $ns", ns.contains("Import cats.syntax.all._"))
  }

  def testApplyImportForInvalidNelFixesBothBranches(): Unit = {
    val action = intentions(validation(s""""too old".invalid${CARET}Nel""")).find(_.getText == "Import cats.syntax.all._")
    assertTrue("import action", action.isDefined)
    myFixture.launchAction(action.get)
    assertTrue(myFixture.getFile.getText, myFixture.getFile.getText.contains("import cats.syntax.all._"))
    val unresolved = myFixture.doHighlighting().asScala.filter(_.getSeverity == com.intellij.lang.annotation.HighlightSeverity.ERROR)
    assertTrue(s"errors left: ${unresolved.map(_.getDescription)}", unresolved.isEmpty)
  }
}
