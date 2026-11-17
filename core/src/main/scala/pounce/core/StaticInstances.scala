package pounce.core

import pounce.core.types.Ty

/**
 * Known typeclass instances for common types, so the oracle can answer most questions without running the Scala
 * plugin's (expensive) implicit search. Values list the *strongest* instances; everything they imply is derived
 * via [[Typeclasses.implied]].
 *
 * Keys are canonical FQNs of type constructors (after alias expansion), plus short names as a fallback for catalog
 * shapes that carry no FQN.
 */
object StaticInstances {

  enum Answer {
    case Yes, No, Unknown

    def isYes: Boolean = this == Yes
    def &&(other: => Answer): Answer =
      if (this == No) No else if (this == Yes) other else if (other == No) No else Unknown
  }

  private val seqLike = Set("Traverse", "TraverseFilter", "Monad", "Alternative", "CoflatMap", "Align")

  /** Higher-kinded instances, keyed by constructor FQN. */
  val higherKinded: Map[String, Set[String]] = Map(
    "scala.collection.immutable.List"       -> seqLike,
    "scala.collection.immutable.Vector"     -> seqLike,
    "scala.collection.immutable.LazyList"   -> seqLike,
    "scala.collection.immutable.Seq"        -> seqLike,
    "scala.collection.immutable.Queue"      -> seqLike,
    "scala.collection.immutable.ArraySeq"   -> seqLike,
    "cats.data.Chain"                       -> seqLike,
    "scala.Option"                          -> Set("Traverse", "TraverseFilter", "MonadError", "Alternative", "CoflatMap", "Align"),
    "scala.Some"                            -> Set("Traverse", "Monad"),
    "scala.util.Either"                     -> Set("Traverse", "MonadError", "SemigroupK", "Parallel"),
    "scala.util.Try"                        -> Set("Traverse", "MonadError", "CoflatMap"),
    "scala.concurrent.Future"               -> Set("MonadError", "CoflatMap"),
    "cats.data.NonEmptyList"                -> Set("NonEmptyTraverse", "Monad", "SemigroupK", "Comonad", "Align"),
    "cats.data.NonEmptyVector"              -> Set("NonEmptyTraverse", "Monad", "SemigroupK", "Comonad", "Align"),
    "cats.data.NonEmptySeq"                 -> Set("NonEmptyTraverse", "Monad", "SemigroupK", "Comonad"),
    "cats.data.NonEmptyChainImpl.Type"      -> Set("NonEmptyTraverse", "Monad", "SemigroupK", "Comonad", "Align"),
    "cats.data.Validated"                   -> Set("Traverse", "ApplicativeError", "SemigroupK", "Parallel"),
    "cats.data.Ior"                         -> Set("Traverse", "MonadError", "Parallel"),
    "cats.data.Const"                       -> Set("Traverse", "Applicative"),
    "cats.Eval"                             -> Set("Monad", "Comonad", "Defer"),
    "cats.data.EitherT"                     -> Set("MonadError", "Defer", "Parallel"),
    "cats.data.OptionT"                     -> Set("Monad", "MonoidK", "Defer"),
    "cats.data.Kleisli"                     -> Set("Monad", "Defer", "Parallel"),
    "cats.data.IndexedStateT"               -> Set("Monad", "Defer"),
    "cats.data.WriterT"                     -> Set("Monad", "Defer"),
    "cats.data.IorT"                        -> Set("MonadError", "Defer"),
    "cats.data.Nested"                      -> Set("Functor"),
    "cats.free.Free"                        -> Set("Monad", "Defer"),
    "cats.effect.IO"                        -> Set("Async", "Parallel", "LiftIO", "SemigroupK", "Console"),
    "cats.effect.SyncIO"                    -> Set("Sync"),
    "cats.effect.kernel.Resource"           -> Set("MonadCancel", "Defer", "Parallel"),
    "cats.effect.kernel.Ref"                -> Set.empty,
    "fs2.Stream"                            -> Set("MonadError", "MonoidK", "Defer", "Align"),
    "fs2.Pull"                              -> Set("Monad"),
    "fs2.Chunk"                             -> Set("Traverse", "TraverseFilter", "Monad", "Alternative"),
    "scala.Function1"                       -> Set("Monad", "Distributive"),
    "scala.Function0"                       -> Set("Monad", "Comonad", "Distributive"),
    "scala.Tuple2"                          -> Set("Traverse", "Comonad"),
    "scala.collection.immutable.Map"        -> Set("UnorderedTraverse", "FunctorFilter", "FlatMap", "Align"),
    "scala.collection.immutable.SortedMap"  -> Set("Traverse", "TraverseFilter", "FlatMap", "Align"),
    "scala.collection.immutable.Set"        -> Set("UnorderedTraverse", "MonoidK"),
    "scala.collection.immutable.SortedSet"  -> Set("Foldable", "SemigroupK"),
    "doobie.free.connection.ConnectionIO"   -> Set("Async"),
    "play.api.libs.json.JsResult"           -> Set("Monad")
  )

  /** Binary type constructors. */
  val binary: Map[String, Set[String]] = Map(
    "scala.util.Either"     -> Set("Bitraverse"),
    "scala.Tuple2"          -> Set("Bitraverse"),
    "cats.data.Validated"   -> Set("Bitraverse"),
    "cats.data.Ior"         -> Set("Bitraverse"),
    "cats.data.EitherT"     -> Set("Bifunctor")
  )

  private val numeric = Set("CommutativeMonoid", "Order", "Hash", "Show")

  /** Proper-type instances; containers are answered recursively in [[proper]]. */
  val properLeaves: Map[String, Set[String]] = Map(
    "scala.Int"                             -> numeric,
    "scala.Long"                            -> numeric,
    "scala.Short"                           -> numeric,
    "scala.Byte"                            -> numeric,
    "scala.Double"                          -> numeric,
    "scala.Float"                           -> numeric,
    "scala.math.BigInt"                     -> numeric,
    "scala.math.BigDecimal"                 -> numeric,
    "java.lang.String"                      -> Set("Monoid", "Order", "Hash", "Show"),
    "scala.Boolean"                         -> Set("Order", "Hash", "Show"),
    "scala.Char"                            -> Set("Order", "Hash", "Show"),
    "scala.Unit"                            -> Set("CommutativeMonoid", "Order", "Hash", "Show"),
    "scala.Symbol"                          -> Set("Order", "Hash", "Show"),
    "java.util.UUID"                        -> Set("Order", "Hash", "Show"),
    "scala.concurrent.duration.FiniteDuration" -> Set("CommutativeMonoid", "Order", "Hash", "Show"),
    "scala.concurrent.duration.Duration"    -> Set("CommutativeMonoid", "Order", "Hash", "Show"),
    "java.time.Instant"                     -> Set("Order", "Hash", "Show"),
    "java.time.LocalDate"                   -> Set("Order", "Hash", "Show"),
    "java.time.ZonedDateTime"               -> Set("Order", "Hash", "Show"),
    "java.time.Duration"                    -> Set("Order", "Hash", "Show")
  )

  /** Short-name aliases so catalog shapes (no FQN) can be answered too. */
  private val shortToFqn: Map[String, String] =
    (higherKinded.keys ++ properLeaves.keys ++ binary.keys).toSeq
      .groupBy(_.split('.').last)
      .collect { case (short, Seq(single)) => short -> single } ++ Map(
      "Either"   -> "scala.util.Either",
      "List"     -> "scala.collection.immutable.List",
      "Vector"   -> "scala.collection.immutable.Vector",
      "Seq"      -> "scala.collection.immutable.Seq",
      "Map"      -> "scala.collection.immutable.Map",
      "Set"      -> "scala.collection.immutable.Set",
      "String"   -> "java.lang.String",
      "Int"      -> "scala.Int",
      "Long"     -> "scala.Long",
      "Double"   -> "scala.Double",
      "Boolean"  -> "scala.Boolean",
      "Unit"     -> "scala.Unit",
      "IO"       -> "cats.effect.IO",
      "Resource" -> "cats.effect.kernel.Resource",
      "Stream"   -> "fs2.Stream",
      "Option"   -> "scala.Option",
      "NonEmptyChain" -> "cats.data.NonEmptyChainImpl.Type",
      "StateT"   -> "cats.data.IndexedStateT",
      "ReaderT"  -> "cats.data.Kleisli",
      "ConnectionIO" -> "doobie.free.connection.ConnectionIO"
    )

  def fqnOf(name: String, fqn: String): String =
    if (fqn.nonEmpty) Ty.canonicalFqn(fqn) else shortToFqn.getOrElse(name, name)

  /** Is `tc` (a higher-kinded typeclass) available for the type constructor `ctor`? */
  def higherKinded(tc: String, ctor: Ty): Answer = ctor match {
    case Ty.Lambda(name, fixed, fqn, false) => lookupHK(tc, fqnOf(name, fqn), fixed.size)
    case Ty.Con(name, args, fqn, false)     => lookupHK(tc, fqnOf(name, fqn), args.size)
    case _                                  => Answer.Unknown
  }

  private def lookupHK(tc: String, fqn: String, fixedArgs: Int): Answer = {
    // Binary typeclasses (Bifunctor, Bitraverse, ...) use the binary table whatever arity we were given: a fully
    // abstracted constructor arrives as `Lambda(name, Nil)`, i.e. fixedArgs == 0.
    val isBinaryTc = Typeclasses.byName.get(Typeclasses.canonical(tc).getOrElse(tc)).exists(_.kind == Typeclasses.Kind.Binary)
    val table      = if (isBinaryTc && binary.contains(fqn)) binary else higherKinded
    table.get(fqn) match {
      case Some(strongest) => if (Typeclasses.satisfies(strongest, tc)) Answer.Yes else Answer.No
      case None            => Answer.Unknown
    }
  }

  /** Is `tc` (a proper typeclass like Monoid/Show/Eq) available for the fully applied type `t`? */
  def proper(tc: String, t: Ty): Answer = {
    val req = Typeclasses.canonical(tc).getOrElse(tc)
    def leaf(fqn: String): Answer = properLeaves.get(fqn) match {
      case Some(s) => if (Typeclasses.satisfies(s, req)) Answer.Yes else Answer.No
      case None    => Answer.Unknown
    }
    t match {
      case Ty.Con(name, args, fqn, false) =>
        val f = fqnOf(name, fqn)
        (f, args) match {
          case (_, Nil) => leaf(f)
          case ("scala.collection.immutable.List" | "scala.collection.immutable.Vector" | "cats.data.Chain" |
                "scala.collection.immutable.Seq" | "scala.collection.immutable.LazyList", List(a)) =>
            req match {
              case "Semigroup" | "Monoid" => Answer.Yes
              case "Eq" | "PartialOrder" | "Order" | "Hash" | "Show" => proper(req, a)
              case _                                                  => Answer.No
            }
          case ("scala.Option", List(a)) =>
            req match {
              case "Semigroup" | "Monoid" => proper("Semigroup", a)
              case "Eq" | "PartialOrder" | "Order" | "Hash" | "Show" => proper(req, a)
              case _                                                  => Answer.No
            }
          case ("cats.data.NonEmptyList" | "cats.data.NonEmptyVector", List(a)) =>
            req match {
              case "Semigroup" => Answer.Yes
              case "Monoid"    => Answer.No
              case "Eq" | "PartialOrder" | "Order" | "Hash" | "Show" => proper(req, a)
              case _ => Answer.No
            }
          case ("scala.collection.immutable.Map", List(_, v)) =>
            req match {
              case "Semigroup" | "Monoid" => proper("Semigroup", v)
              case "Eq" | "Hash" | "Show" => proper(req, v)
              case _                      => Answer.No
            }
          case ("scala.collection.immutable.Set", List(a)) =>
            req match {
              case "Semigroup" | "Monoid" => Answer.Yes
              case "Eq" | "Hash" | "Show" => proper(req, a)
              case _                      => Answer.No
            }
          case ("scala.util.Either", List(l, r)) =>
            req match {
              case "Semigroup" | "Monoid" => proper("Semigroup", r) && (if (req == "Monoid") proper("Monoid", r) else Answer.Yes)
              case "Eq" | "Order" | "PartialOrder" | "Hash" | "Show" => proper(req, l) && proper(req, r)
              case _ => Answer.No
            }
          case ("cats.effect.IO", List(a)) =>
            req match {
              case "Semigroup" | "Monoid" => proper(req, a)
              case _                      => Answer.No
            }
          case _ => Answer.Unknown
        }
      case Ty.Tuple(es) =>
        es.foldLeft(Answer.Yes)((acc, e) => acc && proper(req, e))
      case _ => Answer.Unknown
    }
  }

  /** All typeclass names (canonical) that a constructor statically has, for presentation. */
  def allFor(fqn: String): Set[String] =
    higherKinded.get(Ty.canonicalFqn(fqn)).map(_.flatMap(Typeclasses.implied)).getOrElse(Set.empty)
}
