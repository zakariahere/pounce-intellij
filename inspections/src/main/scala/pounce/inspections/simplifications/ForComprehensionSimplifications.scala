package pounce.inspections.simplifications

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.stripped
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.inspections.simplifications.Shapes.*
import pounce.inspections.util.Patterns.*

/**
 * For-comprehensions: `for { a <- fa } yield a` → `fa`, a single generator → `fa.map(...)`, a later
 * `x <- F.pure(v)` → `x = v`, and a later `_ <- F.unit` removed.
 */
final class SimplifyForComprehensionInspection extends SimplificationInspection(SimplifyForComprehension.rules)

object SimplifyForComprehension {

  private def enumeratorsOf(f: ScFor): Seq[ScEnumerator] =
    f.enumerators.toSeq.flatMap(_.enumerators)

  /** The only enumerator, when it's a generator with a simple pattern: (name, expr). */
  private def singleGenerator(f: ScFor): Option[(String, ScReferencePattern | ScWildcardPattern, ScExpression)] =
    enumeratorsOf(f) match {
      case Seq(g: ScGenerator) =>
        (g.pattern, g.expr) match {
          case (p: ScReferencePattern, Some(e)) => Some((p.name, p, e))
          case (p: ScWildcardPattern, Some(e))  => Some(("_", p, e))
          case _                                => None
        }
      case _ => None
    }

  val yieldItself: Rule = Rule("for { a <- fa } yield a can be replaced with fa", "Replace with the generator's effect") {
    case f: ScFor if f.isYield && singleGenerator(f).exists { case (_, p, fa) => isEffect(fa) && yieldsPattern(f, p) } =>
      Rewrite(f, singleGenerator(f).get._3.getText)
  }

  private def yieldsPattern(f: ScFor, p: PsiElement): Boolean = (p, f.body.map(stripped)) match {
    case (rp: ScReferencePattern, Some(r: ScReferenceExpression)) => !r.isQualified && (try r.resolve() == rp catch { case _: Exception => false })
    case _                                                        => false
  }

  val singleMap: Rule = Rule("A for-comprehension with a single generator can be replaced with fa.map(...)", "Replace with .map") {
    case f: ScFor if f.isYield && f.body.isDefined && singleGenerator(f).exists { case (_, p, fa) => isEffect(fa) && !yieldsPattern(f, p) } =>
      val (name, _, fa) = singleGenerator(f).get
      Rewrite(f, s"${op(fa)}.map($name => ${bodyText(f.body.get)})", Some("functor"))
  }

  /** `x <- F.pure(v)` after the first generator → `x = v`. */
  val pureGenerator: Rule = Rule("x <- F.pure(v) inside a for-comprehension can be replaced with x = v", "Replace with a value definition") {
    case f: ScFor if laterGenerators(f).exists(g => pureValue(g).isDefined) =>
      val g = laterGenerators(f).find(g => pureValue(g).isDefined).get
      Rewrite(f, edit(f, g, s"${g.pattern.getText} = ${pureValue(g).get.getText}", fromPrevious = false), highlight = Some(g))
  }

  private def pureValue(g: ScGenerator): Option[ScExpression] = g.expr.flatMap {
    case Pure(v, _) => Some(v)
    case _          => None
  }

  /** `_ <- F.unit` after the first generator → removed. */
  val unitGenerator: Rule = Rule("_ <- F.unit inside a for-comprehension does nothing and can be removed", "Remove the no-op generator") {
    case f: ScFor if laterGenerators(f).exists(isUnitGenerator) =>
      val g = laterGenerators(f).find(isUnitGenerator).get
      Rewrite(f, edit(f, g, "", fromPrevious = true), highlight = Some(g))
  }

  private def isUnitGenerator(g: ScGenerator): Boolean =
    g.pattern.isInstanceOf[ScWildcardPattern] && g.expr.exists(UnitEffect.unapply)

  /** Generators after the first enumerator (a for-comprehension must start with a generator). */
  private def laterGenerators(f: ScFor): Seq[ScGenerator] =
    enumeratorsOf(f).drop(1).collect { case g: ScGenerator => g }

  /**
   * The for-comprehension's text with `g` replaced by `replacement`. With `fromPrevious`, the deleted range starts
   * at the end of the previous enumerator, which removes the separator (`;` or newline) together with `g`.
   */
  private def edit(f: ScFor, g: ScGenerator, replacement: String, fromPrevious: Boolean): String = {
    val base  = f.getTextRange.getStartOffset
    val text  = f.getText
    val prev  = enumeratorsOf(f).takeWhile(_ ne g).lastOption
    val start = if (fromPrevious) prev.map(_.getTextRange.getEndOffset).getOrElse(g.getTextRange.getStartOffset) else g.getTextRange.getStartOffset
    text.substring(0, start - base) + replacement + text.substring(g.getTextRange.getEndOffset - base)
  }

  val rules: Seq[Rule] = Seq(yieldItself, singleMap, pureGenerator, unitGenerator)
}
