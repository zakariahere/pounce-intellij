package pounce.core

import com.intellij.psi.PsiElement
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Op, OpKind}
import pounce.core.types.Ty

/**
 * Decides whether a catalog operation applies to a receiver type: shape match + typeclass constraints.
 *
 * Example: `traverse` has receiver `F[A]`, requires `F: Traverse`. For a receiver `List[Either[E, Int]]` the shape
 * binds `F := List`, `A := Either[E, Int]`, and the oracle confirms `Traverse[List]`. `G: Applicative` is bound by
 * the argument later, so unbound constraint variables are skipped.
 */
object Applicability {

  final case class Result(op: Op, subst: Ty.Subst, answer: Answer, unmet: List[(String, String)]) {

    /** Receiver type with shape variables resolved, for presentation: `traverse` on `List[A]` → `List`. */
    def bindingsText: String =
      op.requires.flatMap { case (v, tc) => subst.get(v).map(t => s"$tc[${t.show}]") }.mkString(", ")
  }

  def check(op: Op, receiver: Ty, place: PsiElement, oracle: InstanceOracle): Option[Result] =
    op.receiver.flatMap(shape0 => checkShape(op, shape0, receiver, oracle))

  private def checkShape(op: Op, shape0: Ty, receiver: Ty, oracle: InstanceOracle): Option[Result] = {
    val shape = widenTupleShape(shape0, receiver, op)
    val fresh = freshen(shape, op)
    Ty.matchShape(fresh, receiver).map { subst =>
      val checks = op.requires.flatMap { case (v, tc) =>
        subst.get(s"$v'") match {
          case Some(bound) => Some((v, tc, oracle.has(tc, bound)))
          case None        => None // bound by an argument, not the receiver
        }
      }
      val answer = checks.foldLeft(Answer.Yes)((acc, c) => acc && c._3)
      val unmet  = checks.collect { case (v, tc, Answer.No) => v -> tc }
      val renamed = Ty.Subst(subst.map.map { case (k, v) => k.stripSuffix("'") -> v })
      Result(op, renamed, answer, unmet)
    }
  }

  /**
   * Tuple ops are written for pairs in the catalog (`(F[A], F[B])`); `mapN`, `tupled`, `parMapN`, ... exist for
   * every arity up to 22, so widen the shape to the receiver's arity: `(F[A1], F[A2], F[A3])`.
   */
  private def widenTupleShape(shape: Ty, receiver: Ty, op: Op): Ty = (op.kind, shape, receiver) match {
    case (OpKind.Tuple, Ty.Tuple(List(Ty.App(f, _), Ty.App(g, _))), Ty.Tuple(es)) if f == g && es.size > 2 && es.size <= 22 =>
      Ty.Tuple(es.indices.toList.map(i => Ty.App(f, List(Ty.Var(s"A$i")))))
    case _ => shape
  }

  /** Rename op variables apart from anything in the receiver (receiver vars are rigid anyway, this is belt & braces). */
  private def freshen(t: Ty, op: Op): Ty = t.renameVars(_ + "'")

  /** Ops of the given kinds that apply (or might apply) to `receiver`, best first. */
  def applicable(
    ops: Iterable[Op],
    receiver: Ty,
    place: PsiElement,
    kinds: Set[OpKind],
    includeUnknown: Boolean = true
  ): Seq[Result] = {
    val oracle = InstanceOracle(place)
    ops.iterator
      .filter(op => kinds.contains(op.kind))
      .flatMap(check(_, receiver, place, oracle))
      .filter(r => r.answer == Answer.Yes || (includeUnknown && r.answer == Answer.Unknown))
      .toSeq
      .sortBy(r => (if (r.answer == Answer.Yes) 0 else 1, -r.op.rank))
  }
}
