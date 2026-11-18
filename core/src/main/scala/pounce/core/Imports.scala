package pounce.core

import com.intellij.openapi.project.Project
import com.intellij.psi.search.{GlobalSearchScope, PsiSearchHelper, UsageSearchContext}
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, PsiModificationTracker, PsiTreeUtil}
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScPackaging
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import pounce.core.catalog.Op
import pounce.core.settings.{ImportStyle, PounceSettings}

import scala.util.control.NonFatal

/** Syntax imports: which import brings an operation into scope, whether it's already there, and adding it. */
object Imports {

  /** Wildcard import roots that bring *all* cats core syntax into scope. */
  val CoreAll: Set[String]   = Set("cats.syntax.all", "cats.implicits")
  val EffectAll: Set[String] = Set("cats.effect.syntax.all", "cats.effect.implicits")

  private def isEffectSyntax(syntax: String) = syntax.startsWith("effect.")

  /**
   * A full package root rather than a cats syntax module: `scala.concurrent.duration` (for `5.seconds`). Cats modules
   * are bare (`traverse`) or `effect.`-prefixed (`effect.spawn`); anything else with a dot is a root, imported as-is
   * whatever the import style.
   */
  def isPackageRoot(syntax: String): Boolean = syntax.contains('.') && !isEffectSyntax(syntax)

  private def module(syntax: String) = syntax.stripPrefix("effect.")

  /** Import roots (without the trailing `._`) that provide `syntax`. */
  def providers(syntax: String): Set[String] =
    if (isPackageRoot(syntax)) Set(syntax)
    else if (isEffectSyntax(syntax)) EffectAll + s"cats.effect.syntax.${module(syntax)}"
    else CoreAll + s"cats.syntax.${module(syntax)}"

  /** The import root to add for `syntax` according to the configured (or detected) style. */
  def rootFor(syntax: String, style: ImportStyle): String =
    if (isPackageRoot(syntax)) syntax else catsRootFor(syntax, style)

  private def catsRootFor(syntax: String, style: ImportStyle): String = (style, isEffectSyntax(syntax)) match {
    case (ImportStyle.Granular, true)  => s"cats.effect.syntax.${module(syntax)}"
    case (ImportStyle.Granular, false) => s"cats.syntax.${module(syntax)}"
    case (ImportStyle.Implicits, true) => "cats.effect.implicits"
    case (ImportStyle.Implicits, false) => "cats.implicits"
    case (_, true)                     => "cats.effect.syntax.all"
    case (_, false)                    => "cats.syntax.all"
  }

  /**
   * The import root to add for `syntax` at `place`: follows the style already used in the file (a file with granular
   * `cats.syntax.functor._` imports gets another granular import, not `cats.syntax.all._`), else the effective style.
   */
  def rootFor(syntax: String, place: PsiElement): String = {
    if (isPackageRoot(syntax)) return syntax
    val inScope  = try wildcardRootsInScope(place) catch { case NonFatal(_) => Set.empty[String] }
    val granular = inScope.exists { r =>
      (r.startsWith("cats.syntax.") && r != "cats.syntax.all") ||
      (r.startsWith("cats.effect.syntax.") && r != "cats.effect.syntax.all")
    }
    val style =
      if (granular) ImportStyle.Granular
      else if (inScope.contains("cats.implicits")) ImportStyle.Implicits
      else if (inScope.contains("cats.syntax.all")) ImportStyle.All
      else effectiveStyle(place)
    rootFor(syntax, style)
  }

  def wildcard(place: PsiElement): String = if (place.isInScala3File) "*" else "_"

  def importText(root: String, place: PsiElement): String = s"$root.${wildcard(place)}"

  // ------------------------------------------------------------------------------------------------------------
  // What's in scope
  // ------------------------------------------------------------------------------------------------------------

  /** Wildcard import roots visible at `place` (`cats.syntax.all`, `cats.implicits`, ...). */
  def wildcardRootsInScope(place: PsiElement): Set[String] = {
    val roots = Set.newBuilder[String]
    var holder: PsiElement = place
    while (holder != null) {
      holder match {
        case h: ScImportsHolder =>
          h.getImportStatements.foreach { stmt =>
            stmt.importExprs.foreach { expr =>
              if (isWildcard(expr)) expr.qualifier.foreach(q => roots += q.qualName)
            }
          }
        case _ =>
      }
      holder = holder.getContext
    }
    roots.result()
  }

  private def isWildcard(expr: ScImportExpr): Boolean =
    expr.hasWildcardSelector || expr.selectors.exists(_.isWildcardSelector)

  def isProvided(op: Op, place: PsiElement): Boolean =
    op.syntax.forall(s => providers(s).exists(wildcardRootsInScope(place).contains))

  def isRootInScope(root: String, place: PsiElement): Boolean = wildcardRootsInScope(place).contains(root)

  // ------------------------------------------------------------------------------------------------------------
  // Style detection
  // ------------------------------------------------------------------------------------------------------------

  /** Effective style: the setting, or for `Auto` the project's majority style (cached until PSI changes). */
  def effectiveStyle(place: PsiElement): ImportStyle =
    PounceSettings().style match {
      case ImportStyle.Auto => detectProjectStyle(place.getProject)
      case explicit         => explicit
    }

  def detectProjectStyle(project: Project): ImportStyle =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val helper = PsiSearchHelper.getInstance(project)
        val scope  = GlobalSearchScope.projectScope(project)
        def count(word: String): Int = {
          var n = 0
          helper.processAllFilesWithWord(word, scope, (_: PsiFile) => { n += 1; n < 200 }, true)
          n
        }
        // "implicits" files are a superset signal for cats.implicits._, "all" for cats.syntax.all._.
        val implicits = count("implicits")
        val all       = count("syntax")
        val style     = if (implicits > all) ImportStyle.Implicits else ImportStyle.All
        CachedValueProvider.Result.create(style, PsiModificationTracker.getInstance(project))
      }
    )

  // ------------------------------------------------------------------------------------------------------------
  // Adding imports
  // ------------------------------------------------------------------------------------------------------------

  /**
   * Adds `import <root>._` (or `.*` in Scala 3) for `place` unless something already provides it.
   * Must be called inside a write action.
   */
  def ensureWildcardImport(root: String, place: PsiElement, alsoSatisfiedBy: Set[String] = Set.empty): Unit = {
    val inScope = wildcardRootsInScope(place)
    if (inScope.contains(root) || alsoSatisfiedBy.exists(inScope.contains)) return
    val holder = ScImportsHolder.forNewImportInsertion(place)
    val stmt   = ScalaPsiElementFactory.createImportFromText(s"import ${importText(root, place)}", holder)
    holder.getImportStatements.lastOption match {
      case Some(last) => holder.addImportAfter(stmt, last)
      case None =>
        val anchor = firstMeaningfulChild(holder)
        anchor match {
          case Some(a) =>
            // addImportBefore already separates the import from `a` with a newline; one more makes one blank line.
            val added = holder.addImportBefore(stmt, a)
            holder.addAfter(ScalaPsiElementFactory.createNewLine("\n")(using place.getProject), added)
          case None => holder.addImport(stmt)
        }
    }
  }

  /** Ensures the syntax import for `op` is present (according to the effective style). */
  def ensureImportFor(op: Op, place: PsiElement): Unit =
    op.syntax.foreach { s =>
      ensureWildcardImport(rootFor(s, place), place, providers(s))
    }

  private def firstMeaningfulChild(holder: ScImportsHolder): Option[PsiElement] = {
    val children = holder.getChildren.toSeq
    holder match {
      case p: ScPackaging =>
        // skip the `package a.b` header
        val headerEnd = p.reference.map(_.getTextRange.getEndOffset).getOrElse(p.getTextRange.getStartOffset)
        children.find(c => c.getTextRange.getStartOffset >= headerEnd && !c.isWhitespaceOrComment)
      case _: ScalaFile =>
        children.find(c => !c.isWhitespaceOrComment && !c.isInstanceOf[ScPackaging] && !isPackageStatement(c))
          .orElse(children.find(_.isInstanceOf[ScPackaging]))
      case _ =>
        children.find(c => !c.isWhitespaceOrComment && c.getTextLength > 1)
    }
  }

  private def isPackageStatement(e: PsiElement) = e.getText.startsWith("package ")

  /** All import statements in a file (for import-conflict inspections). */
  def allImportStatements(file: PsiFile): Seq[ScImportStmt] =
    PsiTreeUtil.findChildrenOfType(file, classOf[ScImportStmt]).toArray(Array.empty[ScImportStmt]).toSeq
}
