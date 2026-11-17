package pounce.inspections.effects

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.Effects
import pounce.core.Effects.Kind
import pounce.inspections.util.Typing
import pounce.inspections.{PounceInspection, ReplaceExpressionFix}

/**
 * B10. A for-comprehension whose generators use different effects (`IO` then `Option`): Scala reports a confusing
 * type mismatch on the desugared `flatMap`. This explains it and lifts the odd generator into the comprehension's
 * effect: `IO.fromOption`, `IO.fromEither`, `.liftTo[F]`, `EitherT.right`, `OptionT.liftF`, `Stream.eval`,
 * `IO.fromFuture` — or suggests a transformer (`OptionT` / `EitherT`).
 */
final class MixedEffectsInForInspection extends PounceInspection {

  import MixedEffectsInForInspection.*

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case f: ScFor =>
      val gens = f.enumerators.toSeq.flatMap(_.generators).flatMap(g => g.expr.map(g -> _))
      gens.headOption.foreach { case (_, firstExpr) =>
        Typing.typeOf(firstExpr).foreach { ctxType =>
          val ctxKey  = Typing.headKey(ctxType, firstExpr)
          val ctxKind = Typing.kind(ctxType, firstExpr)
          gens.drop(1).foreach { case (_, e) =>
            Typing.typeOf(e).foreach { t =>
              val key  = Typing.headKey(t, e)
              val kind = Typing.kind(t, e)
              if (key.isDefined && ctxKey.isDefined && key != ctxKey && (isEffectKind(ctxKind) || isEffectKind(kind))) {
                val ctxName = Typing.ty(ctxType, e).map(Effects.displayName).getOrElse(Typing.show(ctxType, e))
                report(holder, e, message(Typing.show(t, e), ctxName), fixes(e, t, ctxType, ctxKind))
              }
            }
          }
        }
      }
    case _ =>
  }

  private def isEffectKind(k: Kind): Boolean = k != Kind.NotAnEffect

  private def fixes(e: ScExpression, t: ScType, ctx: ScType, ctxKind: Kind): Seq[LocalQuickFix] = {
    val ctxFqn  = Typing.headFqn(ctx, e)
    val rigid   = Typing.rigidHead(ctx)
    val oddFqn  = Typing.headFqn(t, e)
    val x       = Typing.operand(e)
    val ctxArgs = Typing.typeArgs(ctx, e)

    val candidates: Seq[(String, String, Option[String])] = (oddFqn, ctxFqn) match {
      case (Some(o), Some("cats.effect.IO")) if isOption(o) =>
        Seq((s"IO.fromOption($x)(new NoSuchElementException(\"${escape(e.getText)} is empty\"))", "Lift with IO.fromOption", None))
      case (Some(o), Some("cats.effect.IO")) if isEither(o) =>
        Seq((s"IO.fromEither($x)", "Lift with IO.fromEither", None))
      case (Some("scala.concurrent.Future"), Some("cats.effect.IO")) =>
        Seq((s"IO.fromFuture(IO($x))", "Lift with IO.fromFuture", None))
      case (Some(o), None) if rigid.isDefined && isOption(o) =>
        Seq((s"$x.liftTo[${rigid.get}](new NoSuchElementException(\"${escape(e.getText)} is empty\"))", s"Lift with .liftTo[${rigid.get}]", Some("option")))
      case (Some(o), None) if rigid.isDefined && isEither(o) =>
        Seq((s"$x.liftTo[${rigid.get}]", s"Lift with .liftTo[${rigid.get}]", Some("either")))
      case (_, Some("cats.data.EitherT")) if Typing.isEffect(t, e) && ctxArgs.size == 3 =>
        Seq((s"EitherT.right[${Typing.show(ctxArgs(1), e)}]($x)", "Lift with EitherT.right", None))
      case (_, Some("cats.data.OptionT")) if Typing.isEffect(t, e) =>
        Seq((s"OptionT.liftF($x)", "Lift with OptionT.liftF", None))
      case (_, Some("fs2.Stream")) if Typing.isEffect(t, e) =>
        Seq((s"fs2.Stream.eval($x)", "Lift with Stream.eval", None))
      case _ => Nil
    }
    candidates.flatMap { case (text, hint, syntax) =>
      Typing.checkReplacement(e, text, syntax, sameType = false).map(imp => new ReplaceExpressionFix(hint, e, text, imp))
    }
  }
}

object MixedEffectsInForInspection {

  private def isOption(fqn: String) = fqn == "scala.Option" || fqn == "scala.Some" || fqn == "scala.None"

  private def isEither(fqn: String) = fqn == "scala.util.Either" || fqn == "scala.util.Right" || fqn == "scala.util.Left"

  private def escape(s: String) = s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", " ")

  def message(generator: String, context: String): String =
    s"Mixed effects in for-comprehension: this generator is $generator but the for-comprehension runs in $context " +
      "(lift it, or use a transformer such as OptionT / EitherT)"
}
