package pounce.visual.hints

import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScTypedPatternLike, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScFor, ScGenerator}
import pounce.core.types.Ty
import pounce.core.{Bounds, Effects}
import pounce.visual.util.VisualPsi

/**
 * D1: the for-comprehension lens.
 *
 * {{{
 * for ‹IO› {
 *   user  <- findUser(id)     ‹IO · User›
 *   order <- findOrder(user)  ‹IO · Order›
 *   name  <- lookup(user)     ‹Option · String ≠ IO›   (a step in another effect: won't compile)
 * } yield order
 * }}}
 *
 * Tagless code shows the bounds in scope on `for` (`‹F: Sync›`). Wildcard (`_ <- ...`) and explicitly typed
 * generators get no hint (they would only repeat what is written), unless their effect differs.
 *
 * The Scala plugin itself shows no type hints for generator patterns (only in X-Ray mode), so nothing is duplicated.
 */
object ForComprehensionLens {

  enum Kind {
    case ForKeyword, Step, Mismatch
  }

  final case class LensHint(offset: Int, text: String, tooltip: String, kind: Kind)

  private val MaxPart = 32

  def hintsFor(f: ScFor): Seq[LensHint] = {
    val gens = f.enumerators.toSeq.flatMap(_.generators)
    val result = for {
      first                 <- gens.headOption
      (firstFull, firstRaw) <- first.expr.flatMap(VisualPsi.tysOf)
      if Effects.kindOf(firstFull, f) != Effects.Kind.NotAnEffect
      aliases = TypeAliases.inScope(f)
      (effectText, _) <- TypeAliases.effectAndValue(firstFull, firstRaw, aliases)
    } yield {
      val effect = VisualPsi.shorten(effectText, MaxPart)
      val forHint = Option(f.getFirstChild).map { kw =>
        val bounds = boundsText(firstFull, f)
        val text =
          if (bounds.isEmpty) effect
          else if (bounds.startsWith(effect + ":")) bounds
          else s"$effect · $bounds"
        LensHint(kw.getTextRange.getEndOffset, text, s"for-comprehension in $effectText", Kind.ForKeyword)
      }
      forHint.toSeq ++ gens.flatMap(stepHint(_, firstFull, effectText, aliases))
    }
    result.getOrElse(Nil)
  }

  private def stepHint(g: ScGenerator, firstFull: Ty, forEffect: String, aliases: Seq[TypeAliases.Alias]): Option[LensHint] =
    for {
      expr        <- g.expr
      (full, raw) <- VisualPsi.tysOf(expr)
      (eff, value) <- TypeAliases.effectAndValue(full, raw, aliases).orElse(Some((full.show, "")))
      hint <- {
        val offset   = expr.getTextRange.getEndOffset
        val effect   = VisualPsi.shorten(eff, MaxPart)
        val shown    = if (value.isEmpty) effect else s"$effect · ${VisualPsi.shorten(value, MaxPart)}"
        val mismatch = !VisualPsi.sameHead(full, firstFull)
        if (mismatch)
          Some(
            LensHint(
              offset,
              s"$shown ≠ ${VisualPsi.shorten(forEffect, MaxPart)}",
              s"This step is in $eff but the for-comprehension runs in $forEffect: all steps must share one effect " +
                "(lift it, e.g. with OptionT / EitherT, .liftTo, fromOption or fromEither)",
              Kind.Mismatch
            )
          )
        else if (value.isEmpty || isUninformativePattern(g)) None
        else Some(LensHint(offset, shown, s"Runs in $eff, binds a value of type $value", Kind.Step))
      }
    } yield hint

  private def isUninformativePattern(g: ScGenerator): Boolean = g.pattern match {
    case _: ScWildcardPattern  => true
    case _: ScTypedPatternLike => true
    case p                     => p.getText.startsWith("_") || p.getText.contains(":")
  }

  /** `F: Sync` / `F: Sync: Parallel` for each rigid type parameter of the effect with bounds in scope. */
  def boundsText(effect: Ty, place: org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement): String = {
    val names = VisualPsi.rigidNames(effect).distinct
    if (names.isEmpty) ""
    else {
      val all = Bounds.all(place)
      names.flatMap { n =>
        val tcs = all.filter(_.typeParam == n).map(_.typeclass).distinct
        if (tcs.isEmpty) None else Some(s"$n: ${tcs.mkString(": ")}")
      }.mkString(", ")
    }
  }
}
