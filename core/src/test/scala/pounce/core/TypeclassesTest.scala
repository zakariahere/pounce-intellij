package pounce.core

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.core.StaticInstances.Answer
import pounce.core.types.Ty

class TypeclassesTest extends TestCase {

  def testImplied(): Unit = {
    assertTrue(Typeclasses.implied("Sync").contains("Monad"))
    assertTrue(Typeclasses.implied("Async").contains("Temporal"))
    assertTrue(Typeclasses.implied("Monad").contains("Functor"))
    assertFalse(Typeclasses.implied("Functor").contains("Monad"))
    assertTrue(Typeclasses.implied("Traverse").contains("Foldable"))
  }

  def testAliases(): Unit = {
    assertEquals(Some("MonadError"), Typeclasses.canonical("MonadThrow"))
    assertEquals(Some("Spawn"), Typeclasses.canonical("cats.effect.kernel.GenSpawn"))
    assertTrue(Typeclasses.satisfies(Seq("MonadThrow"), "FlatMap"))
  }

  def testRedundant(): Unit =
    assertEquals(Seq("Monad" -> "Sync"), Typeclasses.redundant(Seq("Monad", "Sync")))

  def testStrengthening(): Unit = {
    val candidates = Typeclasses.strengtheningCandidates(Seq("Functor"), "FlatMap")
    assertTrue(candidates.toString, candidates.contains("Monad"))
  }

  def testStaticHigherKinded(): Unit = {
    def con(fqn: String) = Ty.Lambda(fqn.split('.').last, Nil, fqn)
    assertEquals(Answer.Yes, StaticInstances.higherKinded("Traverse", con("scala.collection.immutable.List")))
    assertEquals(Answer.No, StaticInstances.higherKinded("Traverse", con("cats.effect.IO")))
    assertEquals(Answer.Yes, StaticInstances.higherKinded("Sync", con("cats.effect.IO")))
    assertEquals(Answer.No, StaticInstances.higherKinded("Monad", con("cats.data.Validated")))
    assertEquals(Answer.Yes, StaticInstances.higherKinded("Applicative", con("cats.data.Validated")))
    assertEquals(Answer.Unknown, StaticInstances.higherKinded("Monad", con("com.acme.Mystery")))
  }

  def testStaticProper(): Unit = {
    assertEquals(Answer.Yes, StaticInstances.proper("Monoid", Ty.parseUnsafe("Int")))
    assertEquals(Answer.Yes, StaticInstances.proper("Monoid", Ty.parseUnsafe("Map[String, Int]")))
    assertEquals(Answer.No, StaticInstances.proper("Monoid", Ty.parseUnsafe("Boolean")))
    assertEquals(Answer.Yes, StaticInstances.proper("Show", Ty.parseUnsafe("List[Int]")))
    assertEquals(Answer.Yes, StaticInstances.proper("Semigroup", Ty.parseUnsafe("Option[String]")))
  }

  def testBinaryTypeclassOnFullyAbstractedConstructor(): Unit = {
    // `Either` with both holes open arrives as Lambda(name, Nil): must use the binary table
    assertEquals(Answer.Yes, StaticInstances.higherKinded("Bifunctor", Ty.Lambda("Either", Nil, "scala.util.Either")))
    assertEquals(Answer.Unknown, StaticInstances.higherKinded("Bifunctor", Ty.Lambda("Mystery", Nil, "com.acme.Mystery")))
  }

  def testFilterTypeclasses(): Unit = {
    assertTrue(Typeclasses.implied("TraverseFilter").contains("FunctorFilter"))
    assertTrue(Typeclasses.implied("TraverseFilter").contains("Traverse"))
    def con(fqn: String) = Ty.Lambda(fqn.split('.').last, Nil, fqn)
    assertEquals(Answer.Yes, StaticInstances.higherKinded("TraverseFilter", con("scala.collection.immutable.List")))
    assertEquals(Answer.Yes, StaticInstances.higherKinded("FunctorFilter", con("scala.Option")))
    assertEquals(Answer.No, StaticInstances.higherKinded("FunctorFilter", con("cats.effect.IO")))
  }
}
