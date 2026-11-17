package pounce.runtime.debugger.renderers

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Read-only view of a debuggee heap: just enough to walk the runtime layout of cats data types by reading fields.
 * The JDI implementation is [[JdiHeap]]; tests use an in-memory model. Reading fields never runs debuggee code.
 */
trait Heap[V] {
  def isNull(v: V): Boolean
  def className(v: V): String
  def field(v: V, name: String): Option[V]

  /** Elements of a JVM array, or None when `v` is not an array. */
  def arrayElements(v: V): Option[IndexedSeq[V]]
}

/**
 * Runtime layouts of the cats / Scala data types the renderers show (verified against cats-core 2.13.0,
 * cats-effect 3.6.3 and scala-library 2.13 class files by `RendererLayoutTest`).
 */
object CatsLayouts {

  object Fqn {
    val NonEmptyList   = "cats.data.NonEmptyList"
    val NonEmptyVector = "cats.data.NonEmptyVector"
    val Chain          = "cats.data.Chain"
    val ChainSingleton = "cats.data.Chain$Singleton"
    val ChainAppend    = "cats.data.Chain$Append"
    val ChainWrap      = "cats.data.Chain$Wrap"
    val Validated      = "cats.data.Validated"
    val Valid          = "cats.data.Validated$Valid"
    val Invalid        = "cats.data.Validated$Invalid"
    val Ior            = "cats.data.Ior"
    val IorLeft        = "cats.data.Ior$Left"
    val IorRight       = "cats.data.Ior$Right"
    val IorBoth        = "cats.data.Ior$Both"
    val EitherT        = "cats.data.EitherT"
    val OptionT        = "cats.data.OptionT"
    /** cats-effect >= 3.4. */
    val SyncRef        = "cats.effect.kernel.SyncRef"
    /** cats-effect 3.0 - 3.3 kept it nested in the Ref companion. */
    val LegacySyncRef  = "cats.effect.kernel.Ref$SyncRef"
    val Cons           = "scala.collection.immutable.$colon$colon"
    val Vector1        = "scala.collection.immutable.Vector1"
  }

  /** How many elements a renderer shows when a node is expanded. */
  val ElementLimit = 100

  /** Upper bound for counting list cells in labels (keeps huge lists cheap). */
  val CountLimit = 100000

  /**
   * Elements of a Scala `List` by walking its `::` cells (`head` / `next`, `tl` on 2.12). `Some(elements)` when the
   * layout was understood (possibly truncated to `limit`), None when `v` is not a List the walker knows.
   */
  def listElements[V](heap: Heap[V], v: V, limit: Int): Option[Vector[V]] = {
    val out = Vector.newBuilder[V]
    @tailrec def go(cell: V, n: Int): Option[Vector[V]] =
      if (heap.isNull(cell)) None
      else if (heap.className(cell) == Fqn.Cons) {
        if (n >= limit) Some(out.result())
        else
          (heap.field(cell, "head"), heap.field(cell, "next").orElse(heap.field(cell, "tl"))) match {
            case (Some(h), Some(next)) =>
              out += h
              go(next, n + 1)
            case _ => None
          }
      } else if (heap.className(cell).startsWith("scala.collection.immutable.Nil")) Some(out.result())
      else None
    go(v, 0)
  }

  /** Number of cells of a Scala `List` (capped at `cap`), None when `v` is not a List. */
  def listSize[V](heap: Heap[V], v: V, cap: Int = CountLimit): Option[Int] = {
    @tailrec def go(cell: V, n: Int): Option[Int] =
      if (heap.isNull(cell)) None
      else if (heap.className(cell) == Fqn.Cons) {
        if (n >= cap) Some(n)
        else heap.field(cell, "next").orElse(heap.field(cell, "tl")) match {
          case Some(next) => go(next, n + 1)
          case None       => None
        }
      } else if (heap.className(cell).startsWith("scala.collection.immutable.Nil")) Some(n)
      else None
    go(v, 0)
  }

  /**
   * Elements of an immutable Seq when its layout is readable through fields: List, ArraySeq (`unsafeArray`) and
   * small vectors (`Vector1.prefix1`). None means "unknown layout": the caller may fall back to a pure conversion.
   */
  def seqElements[V](heap: Heap[V], v: V, limit: Int): Option[Vector[V]] =
    if (heap.isNull(v)) None
    else {
      val cls = heap.className(v)
      if (cls == Fqn.Cons || cls.startsWith("scala.collection.immutable.Nil")) listElements(heap, v, limit)
      else if (cls == "scala.collection.immutable.Vector0$") Some(Vector.empty)
      else if (cls == Fqn.Vector1) heap.field(v, "prefix1").flatMap(heap.arrayElements).map(_.take(limit).toVector)
      else if (cls.startsWith("scala.collection.immutable.ArraySeq$") || cls.startsWith("scala.collection.mutable.ArraySeq$"))
        heap.field(v, "unsafeArray").orElse(heap.field(v, "array")).flatMap(heap.arrayElements).map(_.take(limit).toVector)
      else None
    }

  /** NonEmptyList(head, tail): `head` followed by the cells of `tail`. */
  def nonEmptyListElements[V](heap: Heap[V], nel: V, limit: Int): Option[Vector[V]] =
    for {
      head <- heap.field(nel, "head")
      tail <- heap.field(nel, "tail")
      rest <- listElements(heap, tail, math.max(limit - 1, 0))
    } yield head +: rest

  def nonEmptyListSize[V](heap: Heap[V], nel: V): Option[Int] =
    heap.field(nel, "tail").flatMap(t => listSize(heap, t)).map(_ + 1)

  /**
   * Elements of a `Chain` (and of a `NonEmptyChain`, which is the same object at runtime) by walking
   * `Singleton(a)` / `Append(leftNE, rightNE)` / `Wrap(seq)` in order, without recursion.
   *
   * @param wrapped elements of a `Wrap`'s sequence whose layout [[seqElements]] does not know (e.g. a large Vector);
   *                returning None makes the whole walk give up.
   */
  def chainElements[V](heap: Heap[V], chain: V, limit: Int)(wrapped: (V, Int) => Option[Vector[V]]): Option[Vector[V]] = {
    val out   = Vector.newBuilder[V]
    var count = 0
    val stack = mutable.Stack[V](chain)
    var ok    = true
    while (ok && stack.nonEmpty && count < limit) {
      val node = stack.pop()
      if (heap.isNull(node)) ok = false
      else
        heap.className(node) match {
          case Fqn.ChainSingleton =>
            heap.field(node, "a") match {
              case Some(a) => out += a; count += 1
              case None    => ok = false
            }
          case Fqn.ChainAppend =>
            (heap.field(node, "leftNE"), heap.field(node, "rightNE")) match {
              case (Some(l), Some(r)) => stack.push(r); stack.push(l)
              case _                  => ok = false
            }
          case Fqn.ChainWrap =>
            heap.field(node, "seq").flatMap(s => seqElements(heap, s, limit - count).orElse(wrapped(s, limit - count))) match {
              case Some(xs) => out ++= xs; count += xs.size
              case None     => ok = false
            }
          case cls if cls.startsWith("cats.data.Chain$Empty") => ()
          case _                                              => ok = false
        }
    }
    if (ok) Some(out.result().take(limit)) else None
  }

  /** The payload a wrapper type is shown as: Valid(a), Invalid(e), EitherT(value), OptionT(value), Ref(current). */
  def payload[V](heap: Heap[V], v: V): Option[(String, V)] =
    heap.className(v) match {
      case Fqn.Valid                    => heap.field(v, "a").map("Valid" -> _)
      case Fqn.Invalid                  => heap.field(v, "e").map("Invalid" -> _)
      case Fqn.IorLeft                  => heap.field(v, "a").map("Left" -> _)
      case Fqn.IorRight                 => heap.field(v, "b").map("Right" -> _)
      case Fqn.EitherT                  => heap.field(v, "value").map("EitherT" -> _)
      case Fqn.OptionT                  => heap.field(v, "value").map("OptionT" -> _)
      case Fqn.SyncRef | Fqn.LegacySyncRef =>
        for {
          ar      <- heap.field(v, "ar")
          current <- if (heap.isNull(ar)) None else heap.field(ar, "value")
        } yield "Ref" -> current
      case _ => None
    }

  /** Label helpers (pure, shared by all renderers). */
  def sizedLabel(kind: String, size: Option[Long]): String = size match {
    case Some(n) if n >= CountLimit => s"$kind(size >= $n)"
    case Some(n)                    => s"$kind(size = $n)"
    case None                       => kind
  }

  def wrappedLabel(kind: String, inner: String): String = s"$kind($inner)"

  /** Shortens a rendered payload so labels stay one line. */
  def clip(s: String, max: Int = 120): String = {
    val oneLine = s.replace('\n', ' ').replace('\r', ' ')
    if (oneLine.length <= max) oneLine else oneLine.take(max - 1) + "…"
  }
}
