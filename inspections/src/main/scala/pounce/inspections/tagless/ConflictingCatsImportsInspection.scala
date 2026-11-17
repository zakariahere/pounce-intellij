package pounce.inspections.tagless

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportExpr
import pounce.core.Imports
import pounce.core.settings.ImportStyle
import pounce.inspections.{EditFix, PounceInspection}

/**
 * B9. Two cats imports that bring the same implicits into scope:
 *   - `cats.implicits._` together with `cats.syntax.all._` (and the cats-effect equivalents);
 *   - a granular `cats.syntax.functor._` next to `cats.syntax.all._` / `cats.implicits._`;
 *   - `cats.instances.list._` next to `cats.implicits._` (which already contains every instance).
 * In Scala 2 the same implicit imported twice under one name is ambiguous, so it's dropped from implicit search and
 * `.map` / `Monad[List]` suddenly stop resolving with a confusing error. Fix: remove the redundant import.
 */
final class ConflictingCatsImportsInspection extends PounceInspection {

  import ConflictingCatsImportsInspection.*

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case expr: ScImportExpr if isWildcard(expr) =>
      expr.qualifier.map(_.qualName).foreach { root =>
        if (isCatsRoot(root)) {
          val visible = wildcardImportsInScope(expr).filterNot(_._1 eq expr)
          visible.find { case (_, other) => conflicts(root, other) }.foreach { case (otherExpr, other) =>
            val sameHolder = holderOf(otherExpr) eq holderOf(expr)
            val isLoser    = loser(root, other, Imports.effectiveStyle(expr)) == root
            // In the same scope only the import we'd drop is reported; across scopes the inner one is (the outer
            // import can't see it), with a removal fix only when it's the one to drop.
            if (!sameHolder || isLoser)
              report(holder, expr, message(root, other), if (isLoser) Seq(removeFix(expr, root)) else Nil)
          }
        }
      }
    case _ =>
  }

  private def removeFix(expr: ScImportExpr, root: String) =
    new EditFix[ScImportExpr](removeHint(root), expr)((e, _) => e.deleteExpr())
}

object ConflictingCatsImportsInspection {

  private val CoreAll    = Imports.CoreAll
  private val EffectAll  = Imports.EffectAll
  private val Implicits  = "cats.implicits"
  private val SyntaxAll  = "cats.syntax.all"
  private val EImplicits = "cats.effect.implicits"
  private val ESyntaxAll = "cats.effect.syntax.all"

  def message(root: String, other: String): String =
    s"Conflicting cats imports: $root._ and $other._ import the same implicits, which makes them ambiguous in Scala 2 " +
      "(syntax and instances stop resolving); keep only one"

  def removeHint(root: String): String = s"Remove import $root._"

  private def isCatsRoot(root: String) = root.startsWith("cats.")

  private def isGranularSyntax(r: String) = r.startsWith("cats.syntax.") && r != SyntaxAll

  private def isGranularEffectSyntax(r: String) = r.startsWith("cats.effect.syntax.") && r != ESyntaxAll

  private def isInstances(r: String) = r.startsWith("cats.instances.")

  /** Do imports `a._` and `b._` bring the same implicit names into scope? */
  def conflicts(a: String, b: String): Boolean = a != b && (pair(a, b) || pair(b, a))

  private def pair(a: String, b: String): Boolean =
    (a == Implicits && b == SyntaxAll) ||
      (a == EImplicits && b == ESyntaxAll) ||
      (isGranularSyntax(a) && CoreAll.contains(b)) ||
      (isGranularEffectSyntax(a) && EffectAll.contains(b)) ||
      (isInstances(a) && b == Implicits) ||
      (a == "cats.instances.all" && isInstances(b))

  /** Which of two conflicting imports to remove: the granular / instances one, or the one not matching the style. */
  def loser(a: String, b: String, style: ImportStyle): String = {
    def rank(r: String): Int =
      if (isInstances(r) && r != "cats.instances.all") 0
      else if (isGranularSyntax(r) || isGranularEffectSyntax(r)) 0
      else if (r == "cats.instances.all") 1
      else if (r == Implicits || r == EImplicits) { if (style == ImportStyle.Implicits) 3 else 2 }
      else { if (style == ImportStyle.Implicits) 2 else 3 }
    if (rank(a) <= rank(b)) a else b
  }

  private def isWildcard(expr: ScImportExpr): Boolean =
    expr.hasWildcardSelector || expr.selectors.exists(_.isWildcardSelector)

  private def holderOf(expr: ScImportExpr): PsiElement = {
    var e: PsiElement = expr.getContext
    while (e != null && !e.isInstanceOf[ScImportsHolder]) e = e.getContext
    e
  }

  /** Wildcard import expressions visible at `place`, with their root (`cats.implicits`). */
  private def wildcardImportsInScope(place: PsiElement): Seq[(ScImportExpr, String)] = {
    val out = Seq.newBuilder[(ScImportExpr, String)]
    var holder: PsiElement = place
    while (holder != null) {
      holder match {
        case h: ScImportsHolder =>
          h.getImportStatements.foreach { stmt =>
            stmt.importExprs.foreach { e =>
              if (isWildcard(e)) e.qualifier.foreach(q => out += (e -> q.qualName))
            }
          }
        case _ =>
      }
      holder = holder.getContext
    }
    out.result()
  }
}
