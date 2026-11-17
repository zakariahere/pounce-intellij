package pounce.runtime.debugger

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.runtime.debugger.renderers.CatsLayouts.*
import pounce.runtime.debugger.renderers.{CatsLayouts, Heap}

/** In-memory model of a debuggee heap: objects with fields, arrays, and leaf payloads. */
sealed trait Node
final case class Obj(cls: String, fields: Map[String, Node]) extends Node
final case class Arr(items: Vector[Node])                    extends Node
final case class Leaf(value: Any)                            extends Node

object MemHeap extends Heap[Node] {
  override def isNull(v: Node): Boolean = v == null
  override def className(v: Node): String = v match {
    case Obj(c, _) => c
    case Arr(_)    => "[Ljava.lang.Object;"
    case Leaf(x)   => x.getClass.getName
    case null      => ""
  }
  override def field(v: Node, name: String): Option[Node] = v match {
    case Obj(_, fs) => fs.get(name)
    case _          => None
  }
  override def arrayElements(v: Node): Option[IndexedSeq[Node]] = v match {
    case Arr(xs) => Some(xs)
    case _       => None
  }
}

/** F1b: the field walks behind the renderers, against the runtime layouts of cats-core 2.13 / Scala 2.13. */
class CatsLayoutsTest extends TestCase {

  private val Nil0: Node = Obj("scala.collection.immutable.Nil$", Map.empty)
  private def leaves(xs: Any*): Vector[Node] = xs.map(Leaf(_)).toVector
  private def list(xs: Any*): Node =
    xs.foldRight(Nil0)((h, t) => Obj(Fqn.Cons, Map("head" -> Leaf(h), "next" -> t)))
  private def nel(head: Any, tail: Any*): Node = Obj(Fqn.NonEmptyList, Map("head" -> Leaf(head), "tail" -> list(tail*)))
  private def single(x: Any): Node           = Obj(Fqn.ChainSingleton, Map("a" -> Leaf(x)))
  private def append(l: Node, r: Node): Node = Obj(Fqn.ChainAppend, Map("leftNE" -> l, "rightNE" -> r))
  private def wrap(seq: Node): Node          = Obj(Fqn.ChainWrap, Map("seq" -> seq))
  private val noWrapped: (Node, Int) => Option[Vector[Node]] = (_, _) => None

  def testListWalk(): Unit = {
    assertEquals(Some(leaves(1, 2, 3)), listElements(MemHeap, list(1, 2, 3), 10))
    assertEquals(Some(leaves(1, 2)), listElements(MemHeap, list(1, 2, 3), 2))
    assertEquals(Some(Vector.empty), listElements(MemHeap, Nil0, 10))
    assertEquals(Some(3), listSize(MemHeap, list(1, 2, 3)))
    assertEquals(Some(2), listSize(MemHeap, list(1, 2, 3), cap = 2))
    // Scala 2.12 names the tail field `tl`
    val legacy = Obj(Fqn.Cons, Map("head" -> Leaf("a"), "tl" -> Nil0))
    assertEquals(Some(leaves("a")), listElements(MemHeap, legacy, 10))
  }

  def testListWalkGivesUpOnUnknownLayouts(): Unit = {
    assertEquals(None, listElements(MemHeap, Obj("scala.collection.immutable.Vector1", Map.empty), 10))
    assertEquals(None, listElements(MemHeap, Obj(Fqn.Cons, Map("head" -> Leaf(1))), 10))
    assertEquals(None, listElements(MemHeap, null, 10))
    assertEquals(None, listSize(MemHeap, Leaf(1)))
  }

  def testNonEmptyList(): Unit = {
    assertEquals(Some(leaves(0, 1, 2)), nonEmptyListElements(MemHeap, nel(0, 1, 2), 100))
    assertEquals(Some(leaves(0)), nonEmptyListElements(MemHeap, nel(0, 1, 2), 1))
    assertEquals(Some(3), nonEmptyListSize(MemHeap, nel(0, 1, 2)))
    assertEquals(Some(1), nonEmptyListSize(MemHeap, nel("only")))
  }

  def testSeqLayouts(): Unit = {
    assertEquals(Some(Vector.empty), seqElements(MemHeap, Obj("scala.collection.immutable.Vector0$", Map.empty), 10))
    val v1 = Obj(Fqn.Vector1, Map("prefix1" -> Arr(leaves(1, 2, 3))))
    assertEquals(Some(leaves(1, 2)), seqElements(MemHeap, v1, 2))
    val arraySeq = Obj("scala.collection.immutable.ArraySeq$ofRef", Map("unsafeArray" -> Arr(leaves("x", "y"))))
    assertEquals(Some(leaves("x", "y")), seqElements(MemHeap, arraySeq, 10))
    assertEquals(Some(leaves(1, 2)), seqElements(MemHeap, list(1, 2), 10))
    // a large vector's trie is not walked: the renderer falls back to a pure `take(n).toList`
    assertEquals(None, seqElements(MemHeap, Obj("scala.collection.immutable.Vector2", Map.empty), 10))
  }

  def testChainWalkIsInOrder(): Unit = {
    val chain = append(single(1), append(wrap(list(2, 3)), single(4)))
    assertEquals(Some(leaves(1, 2, 3, 4)), chainElements(MemHeap, chain, 100)(noWrapped))
    assertEquals(Some(leaves(1, 2, 3)), chainElements(MemHeap, chain, 3)(noWrapped))
    assertEquals(Some(Vector.empty), chainElements(MemHeap, Obj("cats.data.Chain$Empty$", Map.empty), 100)(noWrapped))
  }

  def testChainWrapFallbackAndUnknownNodes(): Unit = {
    val bigVector = Obj("scala.collection.immutable.Vector3", Map.empty)
    val chain     = append(single("a"), wrap(bigVector))
    assertEquals(None, chainElements(MemHeap, chain, 100)(noWrapped))
    var askedLimit = -1
    val fallback = chainElements(MemHeap, chain, 100) { (seq, limit) =>
      assertSame(bigVector, seq)
      askedLimit = limit
      Some(leaves("b", "c"))
    }
    assertEquals(Some(leaves("a", "b", "c")), fallback)
    assertEquals(99, askedLimit)
    assertEquals(None, chainElements(MemHeap, Obj("cats.data.Chain$Mystery", Map.empty), 100)(noWrapped))
    assertEquals(None, chainElements(MemHeap, append(single(1), null), 100)(noWrapped))
  }

  def testDeepChainsDoNotOverflowTheStack(): Unit = {
    val deep = (1 to 20000).foldLeft(single(0): Node)((acc, i) => append(acc, single(i)))
    assertEquals(Some(CatsLayouts.ElementLimit), chainElements(MemHeap, deep, CatsLayouts.ElementLimit)(noWrapped).map(_.size))
    assertEquals(Some(leaves(0, 1, 2)), chainElements(MemHeap, deep, 3)(noWrapped))
  }

  def testPayloads(): Unit = {
    assertEquals(Some("Valid" -> Leaf(42)), payload(MemHeap, Obj(Fqn.Valid, Map("a" -> Leaf(42)))))
    assertEquals(Some("Invalid" -> Leaf("boom")), payload(MemHeap, Obj(Fqn.Invalid, Map("e" -> Leaf("boom")))))
    assertEquals(Some("Left" -> Leaf(1)), payload(MemHeap, Obj(Fqn.IorLeft, Map("a" -> Leaf(1)))))
    assertEquals(Some("Right" -> Leaf(2)), payload(MemHeap, Obj(Fqn.IorRight, Map("b" -> Leaf(2)))))
    assertEquals(Some("EitherT" -> Leaf("io")), payload(MemHeap, Obj(Fqn.EitherT, Map("value" -> Leaf("io")))))
    assertEquals(Some("OptionT" -> Leaf("io")), payload(MemHeap, Obj(Fqn.OptionT, Map("value" -> Leaf("io")))))
    val atomic = Obj("java.util.concurrent.atomic.AtomicReference", Map("value" -> Leaf(7)))
    assertEquals(Some("Ref" -> Leaf(7)), payload(MemHeap, Obj(Fqn.SyncRef, Map("ar" -> atomic))))
    assertEquals(Some("Ref" -> Leaf(7)), payload(MemHeap, Obj(Fqn.LegacySyncRef, Map("ar" -> atomic))))
  }

  def testPayloadUnknownOrBroken(): Unit = {
    assertEquals(None, payload(MemHeap, Obj(Fqn.SyncRef, Map("ar" -> null))))
    assertEquals(None, payload(MemHeap, Obj(Fqn.Valid, Map.empty)))
    // Ior.Both has two payloads: the renderer shows it itself
    assertEquals(None, payload(MemHeap, Obj(Fqn.IorBoth, Map("a" -> Leaf(1), "b" -> Leaf(2)))))
    assertEquals(None, payload(MemHeap, Obj("com.acme.Thing", Map("value" -> Leaf(1)))))
  }

  def testLabels(): Unit = {
    assertEquals("NonEmptyList(size = 3)", sizedLabel("NonEmptyList", Some(3)))
    assertEquals(s"Chain(size >= ${CatsLayouts.CountLimit})", sizedLabel("Chain", Some(CatsLayouts.CountLimit.toLong)))
    assertEquals("Chain", sizedLabel("Chain", None))
    assertEquals("Valid(42)", wrappedLabel("Valid", "42"))
    assertEquals("a b", clip("a\nb"))
    assertEquals(10, clip("x" * 50, max = 10).length)
    assertTrue(clip("x" * 50, max = 10).endsWith("…"))
  }
}
