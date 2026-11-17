package pounce.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.simplifications.Shapes.*
import pounce.inspections.util.Patterns.*

// Rule shapes inspired by zio-intellij's simplifications (Apache-2.0); no code copied, written for cats.

/** `fa.map(_ => ())`, `fa.as(())`, `fa >> F.unit` → `fa.void`. */
final class SimplifyVoidInspection extends SimplificationInspection(SimplifyVoid.rules)

object SimplifyVoid {
  val hint = "Replace with .void"

  val mapToUnit: Rule = Rule("fa.map(_ => ()) can be replaced with fa.void", hint) {
    case e @ fa `.map` ConstFn(UnitLit()) if isEffect(fa) => Rewrite(e, s"${op(fa)}.void", Some("functor"), from = Some(fa))
  }

  val asUnit: Rule = Rule("fa.as(()) can be replaced with fa.void", hint) {
    case e @ fa `.as` UnitLit() if isEffect(fa) => Rewrite(e, s"${op(fa)}.void", Some("functor"), from = Some(fa))
  }

  val thenUnit: Rule = Rule("Sequencing fa with a unit effect (fa >> F.unit) can be replaced with fa.void", hint) {
    case e @ fa `.>>` UnitEffect() if isEffect(fa)                => Rewrite(e, s"${op(fa)}.void", Some("functor"), from = Some(fa))
    case e @ fa `.*>` UnitEffect() if isEffect(fa)                => Rewrite(e, s"${op(fa)}.void", Some("functor"), from = Some(fa))
    case e @ fa `.flatMap` ConstFn(UnitEffect()) if isEffect(fa) => Rewrite(e, s"${op(fa)}.void", Some("functor"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(mapToUnit, asUnit, thenUnit)
}

/** `fa.map(_ => b)`, `fa.void.as(b)`, `fa *> F.pure(b)` → `fa.as(b)` (only for stable `b`). */
final class SimplifyAsInspection extends SimplificationInspection(SimplifyAs.rules)

object SimplifyAs {
  val hint = "Replace with .as"

  val mapToConst: Rule = Rule("fa.map(_ => b) can be replaced with fa.as(b)", hint) {
    case e @ fa `.map` ConstFn(b) if isEffect(fa) && !UnitLit.unapply(b) && isStableValue(b) =>
      Rewrite(e, s"${op(fa)}.as(${b.getText})", Some("functor"), from = Some(fa))
  }

  val voidAs: Rule = Rule("fa.void.as(b) can be replaced with fa.as(b)", hint) {
    case e @ `.void`(fa) `.as` b if isEffect(fa) => Rewrite(e, s"${op(fa)}.as(${b.getText})", Some("functor"), from = Some(fa))
  }

  val thenPure: Rule = Rule("fa >> F.pure(b) can be replaced with fa.as(b)", hint) {
    case e @ fa `.*>` PureOf(b) if isEffect(fa) && !UnitLit.unapply(b)                               => Rewrite(e, s"${op(fa)}.as(${b.getText})", Some("functor"), from = Some(fa))
    case e @ fa `.>>` PureOf(b) if isEffect(fa) && !UnitLit.unapply(b) && isStableValue(b)           => Rewrite(e, s"${op(fa)}.as(${b.getText})", Some("functor"), from = Some(fa))
    case e @ fa `.flatMap` ConstFn(PureOf(b)) if isEffect(fa) && !UnitLit.unapply(b) && isStableValue(b) => Rewrite(e, s"${op(fa)}.as(${b.getText})", Some("functor"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(mapToConst, voidAs, thenPure)
}

/** `fa.flatMap(_ => fb)` → `fa >> fb` (or `fa *> fb` when `fb` is a stable value). */
final class SimplifyFlatMapToSequenceInspection extends SimplificationInspection(SimplifyFlatMapToSequence.rules)

object SimplifyFlatMapToSequence {

  /** Left to the `.void` / `.as` rules. */
  private def handledElsewhere(fb: ScExpression): Boolean = fb match {
    case UnitEffect()                     => true
    case PureOf(b) if isStableValue(b)    => true
    case _                                => false
  }

  val productR: Rule = Rule("fa.flatMap(_ => fb) can be replaced with fa *> fb", "Replace with *>") {
    case e @ fa `.flatMap` ConstFn(fb) if isEffect(fa) && !handledElsewhere(fb) && isStableValue(fb) =>
      Rewrite(e, s"${op(fa)} *> ${op(fb)}", Some("apply"))
  }

  val flatMapToThen: Rule = Rule("fa.flatMap(_ => fb) can be replaced with fa >> fb", "Replace with >>") {
    case e @ fa `.flatMap` ConstFn(fb) if isEffect(fa) && !handledElsewhere(fb) && !isStableValue(fb) =>
      Rewrite(e, s"${op(fa)} >> ${op(fb)}", Some("flatMap"))
  }

  val rules: Seq[Rule] = Seq(productR, flatMapToThen)
}

/** `fa.flatMap(a => F.pure(f(a)))` → `fa.map(a => f(a))`. */
final class SimplifyMapPureInspection extends SimplificationInspection(SimplifyMapPure.rules)

object SimplifyMapPure {
  val flatMapPure: Rule = Rule("fa.flatMap(a => F.pure(f(a))) can be replaced with fa.map(a => f(a))", "Replace with .map") {
    case e @ fa `.flatMap` Lambda1(p, PureOf(body)) if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.map(${paramName(p)} => ${body.getText})", Some("functor"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(flatMapPure)
}

/** `fa.map(f).flatten` → `fa.flatMap(f)`; `fa.flatMap(identity)` → `fa.flatten`. */
final class SimplifyFlattenInspection extends SimplificationInspection(SimplifyFlatten.rules)

object SimplifyFlatten {
  val mapFlatten: Rule = Rule("fa.map(f).flatten can be replaced with fa.flatMap(f)", "Replace with .flatMap") {
    case e @ `.flatten`(fa `.map` f) if isEffect(fa) => Rewrite(e, s"${op(fa)}.flatMap(${f.getText})", Some("flatMap"), from = Some(fa))
  }

  val flatMapIdentity: Rule = Rule("fa.flatMap(identity) can be replaced with fa.flatten", "Replace with .flatten") {
    case e @ fa `.flatMap` IdentityFn() if isEffect(fa) => Rewrite(e, s"${op(fa)}.flatten", Some("flatMap"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(mapFlatten, flatMapIdentity)
}

/** `fa.flatMap(a => f(a).as(a))` → `fa.flatTap(a => f(a))`. */
final class SimplifyFlatTapInspection extends SimplificationInspection(SimplifyFlatTap.rules)

object SimplifyFlatTap {

  /** `fb.as(a)`, `fb.map(_ => a)`, `fb >> F.pure(a)`, `fb *> F.pure(a)` where `a` is the lambda parameter. */
  private def tapped(body: ScExpression, p: org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter): Option[ScExpression] =
    body match {
      case fb `.as` a if isRefTo(a, p)                  => Some(fb)
      case fb `.map` ConstFn(a) if isRefTo(a, p)        => Some(fb)
      case fb `.>>` PureOf(a) if isRefTo(a, p)          => Some(fb)
      case fb `.*>` PureOf(a) if isRefTo(a, p)          => Some(fb)
      case _                                            => None
    }

  val flatMapAs: Rule = Rule("fa.flatMap(a => f(a).as(a)) can be replaced with fa.flatTap(a => f(a))", "Replace with .flatTap") {
    case e @ fa `.flatMap` Lambda1(p, body) if isEffect(fa) && !p.isWildcard && tapped(body, p).isDefined =>
      Rewrite(e, s"${op(fa)}.flatTap(${p.name} => ${tapped(body, p).get.getText})", Some("flatMap"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(flatMapAs)
}

/** `F.pure(())` → `F.unit`; `IO(())` / `IO.delay(())` → `IO.unit`. */
final class SimplifyUnitInspection extends SimplificationInspection(SimplifyUnit.rules)

object SimplifyUnit {
  val pureUnit: Rule = Rule("F.pure(()) can be replaced with F.unit", "Replace with .unit") {
    case e @ Pure(UnitLit(), Some(qual)) => Rewrite(e, s"${qual.getText}.unit")
    case e: ScGenericCall if Pure.unapply(e).exists { case (a, q) => q.isEmpty && UnitLit.unapply(a) } =>
      // `().pure[F]` → `Applicative[F].unit`
      val f = e.typeArguments.headOption.map(_.getText).getOrElse("F")
      Rewrite(e, s"Applicative[$f].unit")
  }

  val delayUnit: Rule = Rule("IO(()) / F.delay(()) can be replaced with IO.unit / F.unit", "Replace with .unit") {
    case e @ Delay(call, UnitLit()) if invokedRef(call).isDefined =>
      val ref = invokedRef(call).get
      // IO(()) → IO.unit; IO.delay(()) / Sync[F].delay(()) → <qualifier>.unit
      Rewrite(e, if (ref.isQualified) s"${ref.qualifier.get.getText}.unit" else s"${ref.getText}.unit")
  }

  val rules: Seq[Rule] = Seq(pureUnit, delayUnit)
}

/** `if (c) fa else F.unit` → `fa.whenA(c)`; `if (c) F.unit else fa` → `fa.unlessA(c)` (only for `F[Unit]`). */
final class SimplifyWhenInspection extends SimplificationInspection(SimplifyWhen.rules)

object SimplifyWhen {
  val whenA: Rule = Rule("if (c) fa else F.unit can be replaced with fa.whenA(c)", "Replace with .whenA") {
    case e: ScIf if e.condition.isDefined && e.thenExpression.exists(isUnitEffect) && e.elseExpression.exists(UnitEffect.unapply) =>
      val fa = stripped(e.thenExpression.get)
      Rewrite(e, s"${op(fa)}.whenA(${e.condition.get.getText})", Some("applicative"))
  }

  val unlessA: Rule = Rule("if (c) F.unit else fa can be replaced with fa.unlessA(c)", "Replace with .unlessA") {
    case e: ScIf if e.condition.isDefined && e.elseExpression.exists(isUnitEffect) && e.thenExpression.exists(UnitEffect.unapply) =>
      val fa = stripped(e.elseExpression.get)
      Rewrite(e, s"${op(fa)}.unlessA(${e.condition.get.getText})", Some("applicative"))
  }

  val rules: Seq[Rule] = Seq(whenA, unlessA)
}
