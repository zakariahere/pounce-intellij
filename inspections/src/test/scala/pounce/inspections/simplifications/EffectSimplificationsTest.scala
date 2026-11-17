package pounce.inspections.simplifications

import org.jetbrains.plugins.scala.ScalaVersion

class SimplifyVoidTest extends SimplificationTestBase[SimplifyVoidInspection] {
  import SimplifyVoid.*

  def testMapToUnitHighlight(): Unit = assertHighlight(mapToUnit, ce(s"io.${START}map(_ => ())$END"))
  def testMapToUnitTagless(): Unit   = assertHighlight(mapToUnit, tagless(s"fa.${START}map(_ => ())$END"))
  def testMapToUnitFix(): Unit       = assertFix(mapToUnit, ce("io.map(_ => ())"), ce("io.void"))
  def testMapToUnitNegative(): Unit  = assertNone(ce("io.map(x => { println(x); () })"))

  def testAsUnitHighlight(): Unit = assertHighlight(asUnit, ce(s"io.${START}as(())$END"))
  def testAsUnitFix(): Unit       = assertFix(asUnit, ce("io.as(())"), ce("io.void"))
  def testAsUnitNegative(): Unit  = assertNone(ce("io.as(1)"))

  def testThenUnitHighlight(): Unit        = assertHighlight(thenUnit, ce(s"io $START>> IO.unit$END"))
  def testFlatMapUnitHighlight(): Unit     = assertHighlight(thenUnit, ce(s"io.${START}flatMap(_ => IO.unit)$END"))
  def testProductRUnitTagless(): Unit      = assertHighlight(thenUnit, tagless(s"fa $START*> Sync[F].unit$END"))
  def testThenUnitFix(): Unit              = assertFix(thenUnit, ce("io >> IO.unit"), ce("io.void"))
  def testThenUnitNegative(): Unit         = assertNone(ce("io >> IO.println(1)"))

  /** Not an effect: `List#map` is left alone. */
  def testCollectionIsNotReported(): Unit = assertNone(ce("ids.map(_ => ())"))
}

class SimplifyAsTest extends SimplificationTestBase[SimplifyAsInspection] {
  import SimplifyAs.*

  def testMapToConstHighlight(): Unit = assertHighlight(mapToConst, ce(s"io.${START}map(_ => 42)$END"))
  def testMapToConstFix(): Unit       = assertFix(mapToConst, ce("io.map(_ => \"done\")"), ce("io.as(\"done\")"))
  def testMapToConstStableCaseClass(): Unit = assertHighlight(mapToConst, ce(s"io.${START}map(_ => User(1, \"a\"))$END"))

  /** `as` evaluates its argument eagerly; a method call may have side effects, so no rewrite. */
  def testMapToConstNegativeUnstable(): Unit = assertNone(ce("io.map(_ => System.nanoTime())"))
  def testMapUsingArgNegative(): Unit        = assertNone(ce("io.map(x => x + 1)"))

  def testVoidAsHighlight(): Unit = assertHighlight(voidAs, ce(s"io.${START}void.as(1)$END"))
  def testVoidAsFix(): Unit       = assertFix(voidAs, ce("io.void.as(1)"), ce("io.as(1)"))
  def testVoidAsNegative(): Unit  = assertNone(ce("io.as(1)"))

  def testThenPureHighlight(): Unit = assertHighlight(thenPure, ce(s"io $START*> IO.pure(1)$END"))
  def testThenPureFix(): Unit       = assertFix(thenPure, ce("io >> IO.pure(1)"), ce("io.as(1)"))
  def testThenPureNegative(): Unit  = assertNone(ce("io >> IO.pure(System.nanoTime())"))
}

class SimplifyFlatMapToSequenceTest extends SimplificationTestBase[SimplifyFlatMapToSequenceInspection] {
  import SimplifyFlatMapToSequence.*

  def testThenHighlight(): Unit = assertHighlight(flatMapToThen, ce(s"${START}io.flatMap(_ => findUser(1))$END"))
  def testThenFix(): Unit       = assertFix(flatMapToThen, ce("io.flatMap(_ => findUser(1))"), ce("io >> findUser(1)"))
  def testThenTaglessFix(): Unit =
    assertFix(flatMapToThen, tagless("fa.flatMap(_ => Sync[F].delay(1))"), tagless("fa >> Sync[F].delay(1)"))

  def testProductRHighlight(): Unit = assertHighlight(productR, ce(s"${START}findUser(1).flatMap(_ => io)$END"))
  def testProductRFix(): Unit       = assertFix(productR, ce("findUser(1).flatMap(_ => io)"), ce("findUser(1) *> io"))

  def testUsesArgumentNegative(): Unit = assertNone(ce("io.flatMap(id => findUser(id))"))

  /** `flatMap(_ => IO.unit)` belongs to the `.void` rule. */
  def testUnitIsLeftToVoid(): Unit = assertNone(ce("io.flatMap(_ => IO.unit)"))
}

class SimplifyMapPureTest extends SimplificationTestBase[SimplifyMapPureInspection] {
  import SimplifyMapPure.*

  def testHighlight(): Unit  = assertHighlight(flatMapPure, ce(s"io.${START}flatMap(a => IO.pure(a + 1))$END"))
  def testTagless(): Unit    = assertHighlight(flatMapPure, tagless(s"fa.${START}flatMap(a => Sync[F].pure(a + 1))$END"))
  def testSyntaxPure(): Unit = assertHighlight(flatMapPure, tagless(s"fa.${START}flatMap(a => (a + 1).pure[F])$END"))
  def testFix(): Unit        = assertFix(flatMapPure, ce("io.flatMap(a => IO.pure(a + 1))"), ce("io.map(a => a + 1)"))

  /** `IO(...)` suspends a side effect: not the same as `map`. */
  def testDelayNegative(): Unit = assertNone(ce("io.flatMap(a => IO(a + 1))"))
}

class SimplifyFlattenTest extends SimplificationTestBase[SimplifyFlattenInspection] {
  import SimplifyFlatten.*

  def testMapFlattenHighlight(): Unit = assertHighlight(mapFlatten, ce(s"io.${START}map(findUser).flatten$END"))
  def testMapFlattenFix(): Unit       = assertFix(mapFlatten, ce("io.map(findUser).flatten"), ce("io.flatMap(findUser)"))
  def testMapFlattenNegative(): Unit  = assertNone(ce("List(List(1)).map(identity).flatten"))

  def testFlatMapIdentityHighlight(): Unit =
    assertHighlight(flatMapIdentity, ce(s"IO.pure(io).${START}flatMap(identity)$END"))
  def testFlatMapIdentityFix(): Unit =
    assertFix(flatMapIdentity, ce("IO.pure(io).flatMap(x => x)"), ce("IO.pure(io).flatten"))
  def testFlatMapIdentityNegative(): Unit = assertNone(ce("io.flatMap(findUser)"))
}

class SimplifyFlatTapTest extends SimplificationTestBase[SimplifyFlatTapInspection] {
  import SimplifyFlatTap.*

  def testAsHighlight(): Unit  = assertHighlight(flatMapAs, ce(s"findUser(1).${START}flatMap(u => save(u).as(u))$END"))
  def testMapHighlight(): Unit = assertHighlight(flatMapAs, ce(s"findUser(1).${START}flatMap(u => save(u).map(_ => u))$END"))
  def testFix(): Unit =
    assertFix(flatMapAs, ce("findUser(1).flatMap(u => save(u).as(u))"), ce("findUser(1).flatTap(u => save(u))"))
  def testNegative(): Unit = assertNone(ce("findUser(1).flatMap(u => save(u).as(u.id))"))
}

class SimplifyUnitTest extends SimplificationTestBase[SimplifyUnitInspection] {
  import SimplifyUnit.*

  def testPureUnitHighlight(): Unit = assertHighlight(pureUnit, ce(s"${START}IO.pure(())$END"))
  def testPureUnitTagless(): Unit   = assertHighlight(pureUnit, tagless(s"${START}Sync[F].pure(())$END"))
  def testPureUnitFix(): Unit       = assertFix(pureUnit, ce("IO.pure(())"), ce("IO.unit"))
  def testSyntaxPureUnitFix(): Unit = assertFix(pureUnit, tagless("().pure[F]"), tagless("Applicative[F].unit"))
  def testPureUnitNegative(): Unit  = assertNone(ce("IO.pure(1)"))

  def testDelayUnitHighlight(): Unit = assertHighlight(delayUnit, ce(s"${START}IO(())$END"))
  def testDelayUnitFix(): Unit       = assertFix(delayUnit, tagless("Sync[F].delay(())"), tagless("Sync[F].unit"))
  def testDelayUnitNegative(): Unit  = assertNone(ce("IO(println(1))"))
}

class SimplifyWhenTest extends SimplificationTestBase[SimplifyWhenInspection] {
  import SimplifyWhen.*

  def testWhenAHighlight(): Unit = assertHighlight(whenA, ce(s"""${START}if (ids.isEmpty) IO.println("none") else IO.unit$END"""))
  def testWhenAFix(): Unit =
    assertFix(whenA, ce("""if (ids.isEmpty) IO.println("none") else IO.unit"""), ce("""IO.println("none").whenA(ids.isEmpty)"""))
  def testUnlessAFix(): Unit =
    assertFix(unlessA, tagless("if (true) Sync[F].unit else fu"), tagless("fu.unlessA(true)"))
  def testUnlessAHighlight(): Unit = assertHighlight(unlessA, tagless(s"${START}if (true) Applicative[F].unit else fu$END"))

  /** `whenA` returns `F[Unit]`: a branch with a value would lose it. */
  def testNonUnitNegative(): Unit = assertNone(ce("if (ids.isEmpty) io else IO.unit"))
  def testBothEffectsNegative(): Unit = assertNone(ce("""if (ids.isEmpty) IO.println("a") else IO.println("b")"""))
}

/** The same rules in Scala 3 user code (`import ...*`, braceless syntax). */
class SimplificationsScala3Test extends SimplificationTestBase[SimplifyVoidInspection] {
  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_3)

  private def scala3(body: String): String =
    s"""import cats.*
       |import cats.syntax.all.*
       |import cats.effect.*
       |
       |object Test:
       |  val io: IO[Int] = IO.pure(1)
       |  def run[F[_]: Sync](fa: F[Int]) =
       |    $body
       |""".stripMargin

  def testVoidHighlight(): Unit = assertHighlight(SimplifyVoid.mapToUnit, scala3(s"fa.${START}map(_ => ())$END"))
  def testVoidFix(): Unit       = assertFix(SimplifyVoid.mapToUnit, scala3("io.map(_ => ())"), scala3("io.void"))
  def testNegative(): Unit      = assertNone(scala3("io.map(_ + 1)"))
}

class SimplifyUnitScala3Test extends SimplificationTestBase[SimplifyUnitInspection] {
  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_3)

  private def scala3(body: String): String =
    s"""import cats.effect.*
       |
       |object Test:
       |  def run =
       |    $body
       |""".stripMargin

  def testPureUnitHighlight(): Unit = assertHighlight(SimplifyUnit.pureUnit, scala3(s"${START}IO.pure(())$END"))
  def testPureUnitFix(): Unit       = assertFix(SimplifyUnit.pureUnit, scala3("IO.pure(())"), scala3("IO.unit"))
  def testNegative(): Unit          = assertNone(scala3("IO.pure(1)"))
}
