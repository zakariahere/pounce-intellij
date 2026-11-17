package pounce.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.simplifications.Shapes.*
import pounce.inspections.util.Patterns.*
import pounce.inspections.util.Typing

/**
 * `xs.map(f).sequence` → `xs.traverse(f)` and friends: `traverse(identity)` → `sequence`, `traverse(f).void` →
 * `traverse_(f)`, the `par*` variants, and `sequence.map(_.flatten)` → `flatSequence`.
 */
final class SimplifyTraverseInspection extends SimplificationInspection(SimplifyTraverse.rules)

object SimplifyTraverse {

  val mapSequence: Rule = Rule("xs.map(f).sequence can be replaced with xs.traverse(f)", "Replace with .traverse") {
    case e @ `.sequence`(xs `.map` f) => Rewrite(e, s"${op(xs)}.traverse(${f.getText})", Some("traverse"), from = Some(xs))
  }

  val mapSequence_ : Rule = Rule("xs.map(f).sequence_ can be replaced with xs.traverse_(f)", "Replace with .traverse_") {
    case e @ `.sequence_`(xs `.map` f) => Rewrite(e, s"${op(xs)}.traverse_(${f.getText})", Some("foldable"), from = Some(xs))
  }

  val traverseIdentity: Rule = Rule("xs.traverse(identity) can be replaced with xs.sequence", "Replace with .sequence") {
    case e @ xs `.traverse` IdentityFn() => Rewrite(e, s"${op(xs)}.sequence", Some("traverse"), from = Some(xs))
  }

  val traverse_Identity: Rule = Rule("xs.traverse_(identity) can be replaced with xs.sequence_", "Replace with .sequence_") {
    case e @ xs `.traverse_` IdentityFn() => Rewrite(e, s"${op(xs)}.sequence_", Some("foldable"), from = Some(xs))
  }

  val traverseVoid: Rule = Rule("xs.traverse(f).void can be replaced with xs.traverse_(f)", "Replace with .traverse_") {
    case e @ `.void`(xs `.traverse` f) => Rewrite(e, s"${op(xs)}.traverse_(${f.getText})", Some("foldable"), from = Some(xs))
  }

  val sequenceVoid: Rule = Rule("xs.sequence.void can be replaced with xs.sequence_", "Replace with .sequence_") {
    case e @ `.void`(`.sequence`(xs)) => Rewrite(e, s"${op(xs)}.sequence_", Some("foldable"), from = Some(xs))
  }

  val mapParSequence: Rule = Rule("xs.map(f).parSequence can be replaced with xs.parTraverse(f)", "Replace with .parTraverse") {
    case e @ `.parSequence`(xs `.map` f) => Rewrite(e, s"${op(xs)}.parTraverse(${f.getText})", Some("parallel"), from = Some(xs))
  }

  val mapParSequence_ : Rule = Rule("xs.map(f).parSequence_ can be replaced with xs.parTraverse_(f)", "Replace with .parTraverse_") {
    case e @ `.parSequence_`(xs `.map` f) => Rewrite(e, s"${op(xs)}.parTraverse_(${f.getText})", Some("parallel"), from = Some(xs))
  }

  val parTraverseIdentity: Rule = Rule("xs.parTraverse(identity) can be replaced with xs.parSequence", "Replace with .parSequence") {
    case e @ xs `.parTraverse` IdentityFn() => Rewrite(e, s"${op(xs)}.parSequence", Some("parallel"), from = Some(xs))
  }

  val flatSequence: Rule = Rule("xs.sequence.map(_.flatten) can be replaced with xs.flatSequence", "Replace with .flatSequence") {
    case e @ `.sequence`(xs) `.map` `_.flatten`() => Rewrite(e, s"${op(xs)}.flatSequence", Some("traverse"), from = Some(xs))
  }

  val flatTraverse: Rule = Rule("xs.traverse(f).map(_.flatten) can be replaced with xs.flatTraverse(f)", "Replace with .flatTraverse") {
    case e @ (xs `.traverse` f) `.map` `_.flatten`() => Rewrite(e, s"${op(xs)}.flatTraverse(${f.getText})", Some("traverse"), from = Some(xs))
  }

  val rules: Seq[Rule] = Seq(
    mapSequence, mapSequence_, traverseIdentity, traverse_Identity, traverseVoid, sequenceVoid,
    mapParSequence, mapParSequence_, parTraverseIdentity, flatSequence, flatTraverse
  )
}

/** `xs.map(f).combineAll` → `xs.foldMap(f)`; `xs.foldLeft(Monoid[A].empty)(_ |+| _)` → `xs.combineAll`. */
final class SimplifyFoldMapInspection extends SimplificationInspection(SimplifyFoldMap.rules)

object SimplifyFoldMap {

  val mapCombineAll: Rule = Rule("xs.map(f).combineAll can be replaced with xs.foldMap(f)", "Replace with .foldMap") {
    case e @ `.combineAll`(xs `.map` f) => Rewrite(e, s"${op(xs)}.foldMap(${f.getText})", Some("foldable"), from = Some(xs))
  }

  val foldLeftCombine: Rule = Rule("xs.foldLeft(Monoid[A].empty)(_ |+| _) can be replaced with xs.combineAll", "Replace with .combineAll") {
    case e: ScMethodCall if foldLeftOf(e).isDefined => Rewrite(e, s"${op(foldLeftOf(e).get)}.combineAll", Some("foldable"))
  }

  /** `xs.foldLeft(Monoid[A].empty)(_ |+| _)` → `xs`. */
  private def foldLeftOf(e: ScMethodCall): Option[ScExpression] = e match {
    case Curried(xs, "foldLeft", Seq(zero), Seq(fn)) if isMonoidEmpty(zero) && isCombine(fn) => Some(xs)
    case _                                                                                    => None
  }

  private def isMonoidEmpty(e: ScExpression): Boolean = {
    val ref = stripped(e) match {
      case r: ScReferenceExpression => Some(r)
      case g: ScGenericCall =>
        g.referencedExpr match {
          case r: ScReferenceExpression => Some(r)
          case _                        => None
        }
      case _ => None
    }
    ref.exists(r => r.refName == "empty" && Typing.resolvedOwner(r).exists(_.startsWith("cats.kernel.Monoid")))
  }

  private val combineOps = Set("|+|", "combine")

  /** `_ |+| _`, `_ combine _`, `_.combine(_)`, `(a, b) => a |+| b`, `(a, b) => a.combine(b)`. */
  private def isCombine(fn: ScExpression): Boolean = stripped(fn) match {
    case ScFunctionExpr(Seq(p1, p2), Some(body)) =>
      stripped(body) match {
        case MethodRepr(_, Some(l), Some(ref), Seq(r)) => combineOps.contains(ref.refName) && isRefTo(l, p1) && isRefTo(r, p2)
        case _                                         => false
      }
    case other =>
      val compact = other.getText.filterNot(_.isWhitespace)
      compact == "_|+|_" || compact == "_combine_" || compact == "_.combine(_)"
  }

  val rules: Seq[Rule] = Seq(mapCombineAll, foldLeftCombine)
}

/** `opt.fold(F.unit)(f)` / `opt.map(f).getOrElse(F.unit)` → `opt.traverse_(f)`. */
final class SimplifyOptionTraverseInspection extends SimplificationInspection(SimplifyOptionTraverse.rules)

object SimplifyOptionTraverse {

  private def isOption(e: ScExpression): Boolean =
    headFqn(e).exists(f => f == "scala.Option" || f == "scala.Some")

  val foldUnit: Rule = Rule("opt.fold(F.unit)(f) can be replaced with opt.traverse_(f)", "Replace with .traverse_") {
    case e: ScMethodCall if isUnitEffect(e) && foldUnitOf(e).isDefined =>
      val (opt, f) = foldUnitOf(e).get
      Rewrite(e, s"${op(opt)}.traverse_(${f.getText})", Some("foldable"))
  }

  private def foldUnitOf(e: ScMethodCall): Option[(ScExpression, ScExpression)] = e match {
    case Curried(opt, "fold", Seq(UnitEffect()), Seq(f)) if isOption(opt) => Some((opt, f))
    case _                                                                 => None
  }

  val mapGetOrElseUnit: Rule = Rule("opt.map(f).getOrElse(F.unit) can be replaced with opt.traverse_(f)", "Replace with .traverse_") {
    case e @ (opt `.map` f) `.getOrElse` UnitEffect() if isOption(opt) && isUnitEffect(e) =>
      Rewrite(e, s"${op(opt)}.traverse_(${f.getText})", Some("foldable"))
  }

  val rules: Seq[Rule] = Seq(foldUnit, mapGetOrElseUnit)
}
