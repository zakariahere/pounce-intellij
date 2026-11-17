package pounce.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.simplifications.Shapes.*
import pounce.inspections.util.Patterns.*

/**
 * Products: `(fa, fb).mapN((a, b) => (a, b))` → `(fa, fb).tupled`; `fa.product(fb).map { case (a, b) => … }` and
 * `(fa, fb).tupled.map { case (a, b) => … }` → `(fa, fb).mapN((a, b) => …)`.
 */
final class SimplifyMapNInspection extends SimplificationInspection(SimplifyMapN.rules)

object SimplifyMapN {

  val mapNTuple: Rule = Rule("(fa, fb).mapN((a, b) => (a, b)) can be replaced with (fa, fb).tupled", "Replace with .tupled") {
    case e @ (t: ScTuple) `.mapN` fn if t.exprs.forall(isEffect) && rebuildsTuple(fn, t.exprs.size) =>
      Rewrite(e, s"${t.getText}.tupled", Some("apply"), from = Some(t))
  }

  /** `(a, b) => (a, b)` with the parameters in order. */
  private def rebuildsTuple(fn: ScExpression, arity: Int): Boolean = stripped(fn) match {
    case ScFunctionExpr(ps, Some(body)) if ps.size == arity =>
      stripped(body) match {
        case t: ScTuple => t.exprs.size == arity && t.exprs.zip(ps).forall { case (x, p) => isRefTo(x, p) }
        case _          => false
      }
    case _ => false
  }

  val productMap: Rule = Rule("fa.product(fb).map { case (a, b) => ... } can be replaced with (fa, fb).mapN((a, b) => ...)", "Replace with .mapN") {
    case e @ (fa `.product` fb) `.map` CaseTuple(Seq(a, b), body) if isEffect(fa) =>
      Rewrite(e, s"(${fa.getText}, ${fb.getText}).mapN(($a, $b) => ${bodyText(body)})", Some("apply"))
  }

  val tupledMap: Rule = Rule("(fa, fb).tupled.map { case (a, b) => ... } can be replaced with (fa, fb).mapN((a, b) => ...)", "Replace with .mapN") {
    case e @ `.tupled`(t: ScTuple) `.map` CaseTuple(names, body) if names.size == t.exprs.size && t.exprs.forall(isEffect) =>
      Rewrite(e, s"${t.getText}.mapN((${names.mkString(", ")}) => ${bodyText(body)})", Some("apply"))
  }

  val rules: Seq[Rule] = Seq(mapNTuple, productMap, tupledMap)
}

/** Shared by the EitherT / OptionT rules: `EitherT(arg)` / `OptionT(arg)` (the case-class apply). */
private[simplifications] final class TransformerApply(name: String, fqn: String) {
  def unapply(e: ScExpression): Option[ScExpression] = e match {
    case ApplyOf(r, Seq(arg)) if r.refName == name && headFqn(e).contains(fqn) => Some(arg)
    case _                                                                    => None
  }
}

/**
 * `EitherT(F.pure(Right(a)))` → `EitherT.rightT[F, E](a)`, `EitherT(F.pure(Left(e)))` → `EitherT.leftT[F, A](e)`,
 * `EitherT(fa.map(Right(_)))` → `EitherT.right[E](fa)`, `EitherT(fa.map(Left(_)))` → `EitherT.left[A](fa)`.
 *
 * `EitherT.right[E]` rather than `EitherT.liftF`: `liftF` infers `E = Nothing` without an expected type (e.g. in a
 * for-comprehension), which then fails to unify with the other steps.
 */
final class SimplifyEitherTInspection extends SimplificationInspection(SimplifyEitherT.rules)

object SimplifyEitherT {
  private val EitherTApply = new TransformerApply("EitherT", "cats.data.EitherT")

  /** (F, E, A) as source text, from the expression's `EitherT[F, E, A]` type. */
  private def params(e: ScExpression): Option[(String, String, String)] = typeArgs(e) match {
    case Seq(f, l, r) => Some((show(f, e), show(l, e), show(r, e)))
    case _            => None
  }

  val pureRight: Rule = Rule("EitherT(F.pure(Right(a))) can be replaced with EitherT.rightT[F, E](a)", "Replace with EitherT.rightT") {
    case e @ EitherTApply(PureOf(RightCall(a))) if params(e).isDefined =>
      val (f, l, _) = params(e).get
      Rewrite(e, s"EitherT.rightT[$f, $l](${a.getText})")
  }

  val pureLeft: Rule = Rule("EitherT(F.pure(Left(e))) can be replaced with EitherT.leftT[F, A](e)", "Replace with EitherT.leftT") {
    case e @ EitherTApply(PureOf(LeftCall(err))) if params(e).isDefined =>
      val (f, _, r) = params(e).get
      Rewrite(e, s"EitherT.leftT[$f, $r](${err.getText})")
  }

  val mapRight: Rule = Rule("EitherT(fa.map(Right(_))) can be replaced with EitherT.right[E](fa)", "Replace with EitherT.right") {
    case e @ EitherTApply(fa `.map` RightFn()) if params(e).isDefined && isEffect(fa) =>
      Rewrite(e, s"EitherT.right[${params(e).get._2}](${fa.getText})")
  }

  val mapLeft: Rule = Rule("EitherT(fa.map(Left(_))) can be replaced with EitherT.left[A](fa)", "Replace with EitherT.left") {
    case e @ EitherTApply(fa `.map` LeftFn()) if params(e).isDefined && isEffect(fa) =>
      Rewrite(e, s"EitherT.left[${params(e).get._3}](${fa.getText})")
  }

  val rules: Seq[Rule] = Seq(pureRight, pureLeft, mapRight, mapLeft)
}

/**
 * `OptionT(F.pure(Some(a)))` → `OptionT.some[F](a)`, `OptionT(F.pure(None))` → `OptionT.none[F, A]`,
 * `OptionT(fa.map(Some(_)))` → `OptionT.liftF(fa)`.
 */
final class SimplifyOptionTInspection extends SimplificationInspection(SimplifyOptionT.rules)

object SimplifyOptionT {
  private val OptionTApply = new TransformerApply("OptionT", "cats.data.OptionT")

  private def params(e: ScExpression): Option[(String, String)] = typeArgs(e) match {
    case Seq(f, a) => Some((show(f, e), show(a, e)))
    case _         => None
  }

  val pureSome: Rule = Rule("OptionT(F.pure(Some(a))) can be replaced with OptionT.some[F](a)", "Replace with OptionT.some") {
    case e @ OptionTApply(PureOf(SomeCall(a))) if params(e).isDefined => Rewrite(e, s"OptionT.some[${params(e).get._1}](${a.getText})")
  }

  val pureNone: Rule = Rule("OptionT(F.pure(None)) can be replaced with OptionT.none[F, A]", "Replace with OptionT.none") {
    case e @ OptionTApply(PureOf(NoneRef())) if params(e).isDefined =>
      val (f, a) = params(e).get
      Rewrite(e, s"OptionT.none[$f, $a]")
  }

  val mapSome: Rule = Rule("OptionT(fa.map(Some(_))) can be replaced with OptionT.liftF(fa)", "Replace with OptionT.liftF") {
    case e @ OptionTApply(fa `.map` SomeFn()) if isEffect(fa) => Rewrite(e, s"OptionT.liftF(${fa.getText})")
  }

  val rules: Seq[Rule] = Seq(pureSome, pureNone, mapSome)
}

/**
 * Opt-in style (INFORMATION level): `Right(x)` → `x.asRight[E]`, `Left(e)` → `e.asLeft[A]`, `Some(x)` → `x.some`,
 * offered only where the expected type is the wider `Either` / `Option` (so the inferred type doesn't change).
 */
final class UseCatsLiftSyntaxInspection extends SimplificationInspection(UseCatsLiftSyntax.rules)

object UseCatsLiftSyntax {

  /** Type arguments of the expected `Either[E, A]` / `Option[A]`. */
  private def expectedArgs(e: ScExpression, fqn: String): Option[Seq[org.jetbrains.plugins.scala.lang.psi.types.ScType]] =
    (try e.expectedType() catch { case _: Exception => None }).filter(t => pounce.inspections.util.Typing.headFqn(t, e).contains(fqn)).map(pounce.inspections.util.Typing.typeArgs(_, e))

  val right: Rule = Rule("Right(x) can be written as x.asRight[E]", "Replace with .asRight") {
    case e @ RightCall(x) if expectedArgs(e, "scala.util.Either").exists(_.size == 2) =>
      Rewrite(e, s"${op(x)}.asRight[${show(expectedArgs(e, "scala.util.Either").get.head, e)}]", Some("either"), check = Check.ExpectedType)
  }

  val left: Rule = Rule("Left(e) can be written as e.asLeft[A]", "Replace with .asLeft") {
    case e @ LeftCall(x) if expectedArgs(e, "scala.util.Either").exists(_.size == 2) =>
      Rewrite(e, s"${op(x)}.asLeft[${show(expectedArgs(e, "scala.util.Either").get(1), e)}]", Some("either"), check = Check.ExpectedType)
  }

  val some: Rule = Rule("Some(x) can be written as x.some", "Replace with .some") {
    case e @ SomeCall(x) if expectedArgs(e, "scala.Option").isDefined =>
      Rewrite(e, s"${op(x)}.some", Some("option"), check = Check.ExpectedType)
  }

  val rules: Seq[Rule] = Seq(right, left, some)
}
