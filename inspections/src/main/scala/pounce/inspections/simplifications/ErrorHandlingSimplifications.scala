package pounce.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{isIndependentOf, stripped, underscore, MethodRepr, Qualified}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScConstructorPattern, ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.simplifications.Shapes.*
import pounce.inspections.util.Patterns.*

/**
 * Error handling: `attempt.map(_.toOption)` → `option`, `handleErrorWith(e => F.pure(x))` → `handleError(e => x)`,
 * `attempt.map(_.fold(g, f))` → `redeem(g, f)`, `attempt.flatMap(_.fold(g, f))` / `attempt.flatMap { case Left / Right }`
 * → `redeemWith(g, f)`, `map(f).handleError(g)` → `redeem(g, f)`.
 */
final class SimplifyErrorHandlingInspection extends SimplificationInspection(SimplifyErrorHandling.rules)

object SimplifyErrorHandling {

  private val `.handleError` = new Qualified(_ == "handleError")

  val attemptToOption: Rule = Rule("fa.attempt.map(_.toOption) can be replaced with fa.option", "Replace with .option") {
    case e @ `.attempt`(fa) `.map` `_.toOption`() if isEffect(fa) => Rewrite(e, s"${op(fa)}.option", Some("applicativeError"), from = Some(fa))
  }

  val handleErrorWithPure: Rule = Rule("fa.handleErrorWith(e => F.pure(x)) can be replaced with fa.handleError(e => x)", "Replace with .handleError") {
    case e @ fa `.handleErrorWith` Lambda1(p, PureOf(x)) if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.handleError(${paramName(p)} => ${x.getText})", Some("applicativeError"), from = Some(fa))
  }

  val attemptMapFold: Rule = Rule("fa.attempt.map(_.fold(g, f)) can be replaced with fa.redeem(g, f)", "Replace with .redeem") {
    case e @ `.attempt`(fa) `.map` FoldOfArg(g, f) if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.redeem(${g.getText}, ${f.getText})", Some("applicativeError"), from = Some(fa))
  }

  val attemptFlatMapFold: Rule = Rule("fa.attempt.flatMap(_.fold(g, f)) can be replaced with fa.redeemWith(g, f)", "Replace with .redeemWith") {
    case e @ `.attempt`(fa) `.flatMap` FoldOfArg(g, f) if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.redeemWith(${g.getText}, ${f.getText})", Some("monadError"), from = Some(fa))
  }

  val attemptFlatMapCases: Rule =
    Rule("fa.attempt.flatMap { case Left(e) => ...; case Right(a) => ... } can be replaced with fa.redeemWith(...)", "Replace with .redeemWith") {
      case e @ `.attempt`(fa) `.flatMap` EitherCases(left, right) if isEffect(fa) =>
        Rewrite(e, s"${op(fa)}.redeemWith(${left._1} => ${bodyText(left._2)}, ${right._1} => ${bodyText(right._2)})", Some("monadError"), from = Some(fa))
    }

  val mapHandleError: Rule = Rule("fa.map(f).handleError(g) can be replaced with fa.redeem(g, f)", "Replace with .redeem") {
    case e @ (fa `.map` f) `.handleError` g if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.redeem(${g.getText}, ${f.getText})", Some("applicativeError"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(attemptToOption, handleErrorWithPure, attemptMapFold, attemptFlatMapFold, attemptFlatMapCases, mapHandleError)

  /** `_.fold(g, f)` or `x => x.fold(g, f)` (with `g`, `f` not using `x`): returns (g, f). */
  object FoldOfArg {
    def unapply(fn: ScExpression): Option[(ScExpression, ScExpression)] = stripped(fn) match {
      case Lambda1(p, MethodRepr(_, Some(q), Some(ref), Seq(l, r)))
          if ref.refName == "fold" && isRefTo(q, p) && isIndependentOf(l, p) && isIndependentOf(r, p) => Some((l, r))
      case MethodRepr(_, Some(q), Some(ref), Seq(l, r)) if ref.refName == "fold" && underscore.unapply(q) => Some((l, r))
      case _ => None
    }
  }

  /** `{ case Left(e) => a; case Right(x) => b }` in either order: returns ((e, a), (x, b)). */
  object EitherCases {
    def unapply(fn: ScExpression): Option[((String, ScExpression), (String, ScExpression))] = stripped(fn) match {
      case b: ScBlockExpr =>
        b.caseClauses.map(_.caseClauses).flatMap {
          case Seq(c1, c2) if c1.guard.isEmpty && c2.guard.isEmpty =>
            val parsed = Seq(c1, c2).flatMap(c => c.pattern.zip(c.expr).flatMap { case (p, body) => side(p).map { case (s, n) => s -> (n, body) } })
            for {
              l <- parsed.collectFirst { case ("Left", v) => v }
              r <- parsed.collectFirst { case ("Right", v) => v }
            } yield (l, r)
          case _ => None
        }
      case _ => None
    }

    private def side(p: org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScPattern): Option[(String, String)] = p match {
      case c: ScConstructorPattern if Set("Left", "Right").contains(c.ref.refName) =>
        c.args.patterns match {
          case Seq(r: ScReferencePattern) => Some(c.ref.refName -> r.name)
          case Seq(_: ScWildcardPattern)  => Some(c.ref.refName -> "_")
          case _                          => None
        }
      case _ => None
    }
  }
}
