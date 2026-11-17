package pounce.completion

import com.intellij.openapi.progress.ProgressManager
import pounce.core.Applicability
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Op, OpKind}
import pounce.core.types.Ty

/**
 * The matching engine behind every Pounce completion: receiver shape + typeclass constraints (like
 * [[pounce.core.Applicability]], but with the completion-tuned [[LensOracle]]) and the fit of an operation's result
 * against the expected type at the caret.
 */
object Lens {

  type Result = Applicability.Result

  /** Shape match + instance check for one op. `None` when the receiver doesn't have the op's shape. */
  def check(op: Op, receiver: Ty, oracle: LensOracle): Option[Result] = {
    op.receiver.flatMap(shape0 => Ty.matchShape(widenTupleShape(shape0, receiver, op).renameVars(_ + "'"), receiver)).map { subst =>
      val checks = op.requires.flatMap { case (v, tc) =>
        subst.get(s"$v'").map(bound => (v, tc, oracle.has(tc, bound)))
      }
      val answer  = checks.foldLeft(Answer.Yes)((acc, c) => acc && c._3)
      val unmet   = checks.collect { case (v, tc, Answer.No) => v -> tc }
      val renamed = Ty.Subst(subst.map.map { case (k, v) => k.stripSuffix("'") -> v })
      Applicability.Result(op, renamed, answer, unmet)
    }
  }

  /** Tuple ops are written for pairs; `mapN`, `tupled`, ... exist for every arity up to 22. */
  private def widenTupleShape(shape: Ty, receiver: Ty, op: Op): Ty = (op.kind, shape, receiver) match {
    case (OpKind.Tuple, Ty.Tuple(List(Ty.App(f, _), Ty.App(g, _))), Ty.Tuple(es)) if f == g && es.size > 2 && es.size <= 22 =>
      Ty.Tuple(es.indices.toList.map(i => Ty.App(f, List(Ty.Var(s"A$i")))))
    case _ => shape
  }

  /** Ops that apply (Yes, or Unknown when `includeUnknown`) to `receiver`, Yes first, then by rank. */
  def applicable(ops: Iterable[Op], receiver: Ty, oracle: LensOracle, includeUnknown: Boolean = true): Seq[Result] =
    ops.iterator
      .flatMap { op =>
        ProgressManager.checkCanceled()
        check(op, receiver, oracle)
      }
      .filter(r => r.answer == Answer.Yes || (includeUnknown && r.answer == Answer.Unknown))
      .toSeq
      .sortBy(r => (if (r.answer == Answer.Yes) 0 else 1, -r.op.rank))

  // ---------------------------------------------------------------------------------------------------------------
  // Expected type
  // ---------------------------------------------------------------------------------------------------------------

  /**
   * How well an op's result fits the expected type.
   *
   * @param score    3 = the result's outer constructor is the expected one (`.asRight` for `Either[String, Int]`),
   *                 2 = a function shape, 1 = it fits through a type-constructor variable (`.pure[F]`)
   * @param subst    the op's variables bound by the expected type (`E := String`)
   * @param evidence instances that had to exist for the fit, e.g. `Applicative[IO]` for `traverse`'s `G`
   */
  final case class Fit(score: Int, subst: Ty.Subst, evidence: List[String])

  /** Catalog shorthands that the bridge dealiases in real types. */
  private def expandCatalogAliases(t: Ty): Ty = t match {
    case Ty.Con("ValidatedNel", List(e, a), _, _) => Ty.Con("Validated", List(Ty.Con("NonEmptyList", List(expandCatalogAliases(e))), expandCatalogAliases(a)))
    case Ty.Con("EitherNel", List(e, a), _, _)    => Ty.Con("Either", List(Ty.Con("NonEmptyList", List(expandCatalogAliases(e))), expandCatalogAliases(a)))
    case Ty.Con(n, args, fqn, rigid)              => Ty.Con(n, args.map(expandCatalogAliases), fqn, rigid)
    case Ty.App(f, args)                          => Ty.App(f, args.map(expandCatalogAliases))
    case Ty.Fun(ps, r)                            => Ty.Fun(ps.map(expandCatalogAliases), expandCatalogAliases(r))
    case Ty.Tuple(es)                             => Ty.Tuple(es.map(expandCatalogAliases))
    case other                                    => other
  }

  /** Fit of `result` (already substituted with the receiver bindings) against the expected types (alias-preserving first). */
  def fit(op: Op, subst: Ty.Subst, expected: Seq[Ty], oracle: LensOracle): Option[Fit] =
    op.result.flatMap { res0 =>
      val res = subst(res0)
      res match {
        case Ty.Var(_) => None // a bare variable (foldMap's B) fits anything: no signal
        case _ =>
          expected.iterator.flatMap { exp =>
            Ty.matchShape(res, exp).orElse(Ty.matchShape(expandCatalogAliases(res), exp)).flatMap { s2 =>
              val newlyBound = op.requires.filter { case (v, _) => subst.get(v).isEmpty && s2.get(v).isDefined }
              val answers    = newlyBound.map { case (v, tc) => (tc, s2.get(v).get, oracle.has(tc, s2.get(v).get)) }
              if (answers.exists(_._3 == Answer.No)) None
              else {
                val score = res match {
                  case _: Ty.Con | _: Ty.Tuple | _: Ty.Lambda => 3
                  case _: Ty.Fun                              => 2
                  case _                                      => 1
                }
                val combined = Ty.Subst(subst.map ++ s2.map)
                Some(Fit(score, combined, answers.collect { case (tc, t, Answer.Yes) => s"$tc[${t.show}]" }))
              }
            }
          }.nextOption()
      }
    }

  /** The op's result type with everything known substituted (receiver bindings, then the expected-type fit). */
  def resultText(op: Op, subst: Ty.Subst, fit: Option[Fit]): String =
    op.result.map(r => fit.map(_.subst).getOrElse(subst)(r).show).getOrElse("")
}
