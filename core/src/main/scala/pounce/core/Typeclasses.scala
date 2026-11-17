package pounce.core

/**
 * The Cats / Cats Effect typeclass hierarchy, as data.
 *
 * Used by the instance oracle (bounds in scope imply weaker typeclasses), by the "strengthen bound" quick fixes,
 * and by the Typeclass Explorer tool window.
 */
object Typeclasses {

  enum Kind {
    /** `F[_]`: Functor, Monad, Traverse, Sync, ... */
    case HigherKinded
    /** `A`: Semigroup, Monoid, Eq, Show, ... */
    case Proper
    /** `F[_, _]`: Bifunctor, ... */
    case Binary
  }

  final case class TC(
    name: String,
    fqn: String,
    parents: List[String],
    kind: Kind,
    library: Lib,
    summary: String
  )

  import Kind.*

  val all: List[TC] = List(
    // ---- cats core: higher kinded -----------------------------------------------------------------------------
    TC("Invariant", "cats.Invariant", Nil, HigherKinded, Lib.CatsCore, "imap between isomorphic types"),
    TC("Functor", "cats.Functor", List("Invariant"), HigherKinded, Lib.CatsCore, "map over a context: F[A] => F[B]"),
    TC("Semigroupal", "cats.Semigroupal", Nil, HigherKinded, Lib.CatsCore, "combine independent contexts into a tuple"),
    TC("Apply", "cats.Apply", List("Functor", "Semigroupal"), HigherKinded, Lib.CatsCore, "ap / mapN over independent effects"),
    TC("Applicative", "cats.Applicative", List("Apply"), HigherKinded, Lib.CatsCore, "lift pure values: pure, unit, whenA"),
    TC("FlatMap", "cats.FlatMap", List("Apply"), HigherKinded, Lib.CatsCore, "sequence dependent effects: flatMap, >>, flatTap"),
    TC("Monad", "cats.Monad", List("FlatMap", "Applicative"), HigherKinded, Lib.CatsCore, "Applicative + FlatMap: for-comprehensions"),
    TC("ApplicativeError", "cats.ApplicativeError", List("Applicative"), HigherKinded, Lib.CatsCore, "raise and handle errors of type E"),
    TC("MonadError", "cats.MonadError", List("ApplicativeError", "Monad"), HigherKinded, Lib.CatsCore, "Monad + error handling: attempt, redeemWith"),
    TC("UnorderedFoldable", "cats.UnorderedFoldable", Nil, HigherKinded, Lib.CatsCore, "fold without order: size, exists, forall"),
    TC("Foldable", "cats.Foldable", List("UnorderedFoldable"), HigherKinded, Lib.CatsCore, "fold structures: foldMap, combineAll, traverse_"),
    TC("UnorderedTraverse", "cats.UnorderedTraverse", List("UnorderedFoldable"), HigherKinded, Lib.CatsCore, "traverse without order"),
    TC("Traverse", "cats.Traverse", List("Functor", "Foldable", "UnorderedTraverse"), HigherKinded, Lib.CatsCore, "traverse / sequence effects through a structure"),
    TC("Reducible", "cats.Reducible", List("Foldable"), HigherKinded, Lib.CatsCore, "non-empty folds: reduce, reduceMap"),
    TC("FunctorFilter", "cats.FunctorFilter", List("Functor"), HigherKinded, Lib.CatsCore, "mapFilter / collect / filter"),
    TC("TraverseFilter", "cats.TraverseFilter", List("FunctorFilter", "Traverse"), HigherKinded, Lib.CatsCore, "traverseFilter / filterA"),
    TC("NonEmptyTraverse", "cats.NonEmptyTraverse", List("Traverse", "Reducible"), HigherKinded, Lib.CatsCore, "traverse with only Apply"),
    TC("SemigroupK", "cats.SemigroupK", Nil, HigherKinded, Lib.CatsCore, "combine contexts: <+>, combineK"),
    TC("MonoidK", "cats.MonoidK", List("SemigroupK"), HigherKinded, Lib.CatsCore, "SemigroupK with an empty context"),
    TC("Alternative", "cats.Alternative", List("Applicative", "MonoidK"), HigherKinded, Lib.CatsCore, "choice and failure: guard, separate"),
    TC("CoflatMap", "cats.CoflatMap", List("Functor"), HigherKinded, Lib.CatsCore, "coflatMap / coflatten"),
    TC("Comonad", "cats.Comonad", List("CoflatMap"), HigherKinded, Lib.CatsCore, "extract a value"),
    TC("Contravariant", "cats.Contravariant", List("Invariant"), HigherKinded, Lib.CatsCore, "contramap"),
    TC("Distributive", "cats.Distributive", List("Functor"), HigherKinded, Lib.CatsCore, "distribute / cosequence"),
    TC("Defer", "cats.Defer", Nil, HigherKinded, Lib.CatsCore, "defer evaluation (stack safety)"),
    TC("NonEmptyParallel", "cats.NonEmptyParallel", Nil, HigherKinded, Lib.CatsCore, "parMapN, parTupled"),
    TC("Parallel", "cats.Parallel", List("NonEmptyParallel"), HigherKinded, Lib.CatsCore, "parTraverse, parSequence"),
    TC("Align", "cats.Align", Nil, HigherKinded, Lib.CatsCore, "align / zipAll"),
    // ---- binary ----------------------------------------------------------------------------------------------
    TC("Bifunctor", "cats.Bifunctor", Nil, Binary, Lib.CatsCore, "bimap / leftMap"),
    TC("Bifoldable", "cats.Bifoldable", Nil, Binary, Lib.CatsCore, "bifold"),
    TC("Bitraverse", "cats.Bitraverse", List("Bifunctor", "Bifoldable"), Binary, Lib.CatsCore, "bitraverse / bisequence"),
    // ---- cats kernel: proper types ---------------------------------------------------------------------------
    TC("Semigroup", "cats.kernel.Semigroup", Nil, Proper, Lib.CatsCore, "associative combine: |+|"),
    TC("Monoid", "cats.kernel.Monoid", List("Semigroup"), Proper, Lib.CatsCore, "Semigroup with empty: combineAll"),
    TC("CommutativeSemigroup", "cats.kernel.CommutativeSemigroup", List("Semigroup"), Proper, Lib.CatsCore, "order-independent combine"),
    TC("CommutativeMonoid", "cats.kernel.CommutativeMonoid", List("Monoid", "CommutativeSemigroup"), Proper, Lib.CatsCore, "order-independent Monoid"),
    TC("Eq", "cats.kernel.Eq", Nil, Proper, Lib.CatsCore, "type-safe equality: ===, =!="),
    TC("PartialOrder", "cats.kernel.PartialOrder", List("Eq"), Proper, Lib.CatsCore, "partial ordering"),
    TC("Order", "cats.kernel.Order", List("PartialOrder"), Proper, Lib.CatsCore, "total ordering: compare, min, max"),
    TC("Hash", "cats.kernel.Hash", List("Eq"), Proper, Lib.CatsCore, "type-safe hashing"),
    TC("Show", "cats.Show", Nil, Proper, Lib.CatsCore, "type-safe toString: .show, show\"...\""),
    // ---- cats effect 3 kernel --------------------------------------------------------------------------------
    TC("MonadCancel", "cats.effect.kernel.MonadCancel", List("MonadError"), HigherKinded, Lib.CatsEffect, "cancellation & bracket: uncancelable, guarantee"),
    TC("Unique", "cats.effect.kernel.Unique", Nil, HigherKinded, Lib.CatsEffect, "unique tokens"),
    TC("Clock", "cats.effect.kernel.Clock", List("Applicative"), HigherKinded, Lib.CatsEffect, "monotonic / realTime"),
    TC("Spawn", "cats.effect.kernel.GenSpawn", List("MonadCancel", "Unique"), HigherKinded, Lib.CatsEffect, "fibers: start, background, race, both"),
    TC("Concurrent", "cats.effect.kernel.GenConcurrent", List("Spawn"), HigherKinded, Lib.CatsEffect, "Ref / Deferred, memoize, parTraverseN"),
    TC("Temporal", "cats.effect.kernel.GenTemporal", List("Concurrent", "Clock"), HigherKinded, Lib.CatsEffect, "sleep, timeout, delayBy"),
    TC("Sync", "cats.effect.kernel.Sync", List("MonadCancel", "Clock", "Unique", "Defer"), HigherKinded, Lib.CatsEffect, "suspend side effects: delay, blocking, interruptible"),
    TC("Async", "cats.effect.kernel.Async", List("Sync", "Temporal"), HigherKinded, Lib.CatsEffect, "async callbacks, fromFuture, evalOn"),
    TC("LiftIO", "cats.effect.LiftIO", Nil, HigherKinded, Lib.CatsEffect, "lift IO into F: liftIO"),
    TC("Console", "cats.effect.std.Console", Nil, HigherKinded, Lib.CatsEffect, "println / readLine as effects")
  )

  val byName: Map[String, TC] = all.map(tc => tc.name -> tc).toMap

  /** Aliases & fixed-error variants map onto their canonical node. */
  val aliases: Map[String, String] = Map(
    "MonadThrow"       -> "MonadError",
    "ApplicativeThrow" -> "ApplicativeError",
    "MonadCancelThrow" -> "MonadCancel",
    "GenSpawn"         -> "Spawn",
    "GenConcurrent"    -> "Concurrent",
    "GenTemporal"      -> "Temporal",
    "Semigroupal"      -> "Semigroupal"
  )

  /** Canonical name for a typeclass name or FQN, if it's one we know. */
  def canonical(nameOrFqn: String): Option[String] = {
    val simple = nameOrFqn.split('.').last
    val name   = aliases.getOrElse(simple, simple)
    byName.get(name).map(_.name)
  }

  private val closureCache = scala.collection.concurrent.TrieMap.empty[String, Set[String]]

  /** The typeclass itself plus everything it implies (all ancestors). */
  def implied(name: String): Set[String] =
    closureCache.getOrElseUpdate(
      name,
      canonical(name) match {
        case None => Set(name)
        case Some(n) =>
          val parents = byName.get(n).map(_.parents).getOrElse(Nil)
          parents.foldLeft(Set(n))((acc, p) => acc ++ implied(p))
      }
    )

  /** Does having all of `bounds` give you `required`? */
  def satisfies(bounds: Iterable[String], required: String): Boolean = {
    val req = canonical(required).getOrElse(required)
    bounds.exists(b => implied(b).contains(req))
  }

  /** Bounds that are already implied by another bound in the list (e.g. `Monad` next to `Sync`). */
  def redundant(bounds: Seq[String]): Seq[(String, String)] =
    for {
      b     <- bounds
      other <- bounds
      if b != other
      cb <- canonical(b)
      co <- canonical(other)
      if cb != co && implied(co).contains(cb)
    } yield b -> other

  /**
   * The weakest typeclasses that would give you `required`, in order of how common they are as bounds in
   * tagless-final code. Used by "Strengthen bound" quick fixes.
   */
  def strengtheningCandidates(current: Seq[String], required: String): Seq[String] = {
    val preferred = List("Functor", "Applicative", "Monad", "MonadThrow", "Sync", "Temporal", "Async", "Traverse", "Parallel", "Concurrent", "Spawn")
    val req       = canonical(required).getOrElse(required)
    val direct    = if (byName.contains(req)) List(required) else Nil
    val upgrades  = preferred.filter(p => implied(p).contains(req) && !current.contains(p))
    (direct ++ upgrades).distinct.take(4)
  }

  /** Strongest-first ordering for presentation. */
  def depth(name: String): Int = implied(name).size
}
