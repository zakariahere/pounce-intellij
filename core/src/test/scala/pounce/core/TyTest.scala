package pounce.core

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.core.types.Ty
import pounce.core.types.Ty.*

class TyTest extends TestCase {

  private def p(s: String): Ty = Ty.parseUnsafe(s)

  def testParseSimple(): Unit = {
    assertEquals(App(Var("F"), List(Var("A"))), p("F[A]"))
    assertEquals(Con("List", List(Con("Int", Nil))), p("List[Int]"))
    assertEquals(Fun(List(Var("A")), App(Var("G"), List(Var("B")))), p("A => G[B]"))
    assertEquals(Fun(List(Var("A"), Var("B")), Var("C")), p("(A, B) => C"))
    assertEquals(Tuple(List(App(Var("F"), List(Var("A"))), App(Var("F"), List(Var("B"))))), p("(F[A], F[B])"))
    assertEquals(Con("IO", List(Var("A"))), p("IO[A]"))
  }

  def testParseCurriedAndNested(): Unit = {
    val t = p("List[A] => (A => F[B]) => F[List[B]]")
    assertEquals("List[A] => (A => F[B]) => F[List[B]]", t.show)
  }

  def testShowRoundTrip(): Unit =
    Seq("F[A]", "Either[E, A]", "(A, B) => C", "G[F[B]]", "(F[A], F[B])", "A => Boolean").foreach { s =>
      assertEquals(s, p(s).show)
    }

  def testMatchShapePartialApplication(): Unit = {
    // traverse on List[Either[String, Int]]: F := List, A := Either[String, Int]
    val s = matchShape(p("F[A]"), p("List[Either[String, Int]]")).get
    assertEquals("List", s.apply(Var("F")).show)
    assertEquals("Either[String, Int]", s.apply(Var("A")).show)
  }

  def testMatchShapeNestedBindsLambda(): Unit = {
    // sequence on List[Either[String, Int]]: F := List, G := Either[String, *], A := Int
    val s = matchShape(p("F[G[A]]"), p("List[Either[String, Int]]")).get
    assertEquals("List", s.apply(Var("F")).show)
    assertEquals("Either[String, *]", s.apply(Var("G")).show)
    assertEquals("Int", s.apply(Var("A")).show)
    // substituting the result shape gives the expected type
    assertEquals("Either[String, List[Int]]", s.apply(p("G[F[A]]")).show)
  }

  def testMatchShapeFailsOnMismatch(): Unit = {
    assertTrue(matchShape(p("F[F[A]]"), p("List[Option[Int]]")).isEmpty)
    assertTrue(matchShape(p("Either[E, A]"), p("Option[Int]")).isEmpty)
    assertTrue(matchShape(p("F[A]"), p("Int")).isEmpty)
  }

  def testMatchShapeFlatten(): Unit =
    assertTrue(matchShape(p("F[F[A]]"), p("Option[Option[Int]]")).isDefined)

  def testRigidTargetVariablesDoNotBind(): Unit = {
    // receiver F[User] in tagless code: F is rigid, the pattern's F binds to it, A to User
    val s = matchShape(p("F[A]"), Con("F", List(Con("User", Nil)), rigid = true)).get
    assertEquals(Lambda("F", Nil, "", rigid = true), s.get("F").get)
  }

  def testUnifyBothSidesWithVariables(): Unit = {
    // Hoogle query vs catalog signature
    val query   = p("List[X] => (X => IO[Y]) => IO[List[Y]]")
    val catalog = p("F[A] => (A => G[B]) => G[F[B]]")
    assertTrue(unify(catalog, query).isDefined)
  }

  def testOccursCheck(): Unit =
    assertTrue(unify(Var("A"), p("List[A]")).isEmpty)

  def testTupleWidening(): Unit = {
    val s = matchShape(p("(F[A], F[B])"), p("(IO[Int], IO[String])")).get
    assertEquals("IO", s.apply(Var("F")).show)
  }

  def testParseErrors(): Unit = {
    assertTrue(Ty.parse("F[").isLeft)
    assertTrue(Ty.parse("=> A").isLeft)
  }

  def testCanonicalFqnMapsCatsEffectAliases(): Unit = {
    assertEquals("cats.effect.kernel.Resource", Ty.canonicalFqn("cats.effect.package.Resource"))
    assertEquals("cats.effect.kernel.Sync", Ty.canonicalFqn("cats.effect.Sync"))
    assertEquals("cats.effect.IO", Ty.canonicalFqn("cats.effect.IO")) // a real class, not an alias
  }
}
