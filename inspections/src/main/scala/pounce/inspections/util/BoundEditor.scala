package pounce.inspections.util

import com.intellij.psi.PsiElement
import com.intellij.psi.codeStyle.CodeStyleManager
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScContextBound, ScSimpleTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterClause, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.Context
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.project.{ProjectContext, ScalaFeatures}
import pounce.core.types.ScTypeBridge
import pounce.core.{Lib, Typeclasses}

import scala.util.control.NonFatal

/**
 * Reads and edits the typeclass bounds of a tagless-final type parameter: context bounds (`F[_]: Monad`) and
 * implicit / using parameters (`(implicit F: Monad[F])`). Shared by the "strengthen bound" and "add bound" fixes.
 */
object BoundEditor {

  /** One typeclass bound of a type parameter, with the PSI to edit. */
  sealed trait Declared {
    def typeclass: String
    def canonical: String = Typeclasses.canonical(typeclass).getOrElse(typeclass)
  }
  final case class ContextBound(bound: ScContextBound, typeclass: String)  extends Declared
  final case class ImplicitParam(param: ScParameter, typeclass: String) extends Declared

  /** Name of the typeclass in a context bound / implicit parameter type (`Monad`, `MonadError`, ...). */
  private def tcName(t: org.jetbrains.plugins.scala.lang.psi.types.ScType, place: PsiElement): Option[String] = {
    given Context = Context(place)
    try
      t match {
        case ParameterizedType(designator, _) => Some(ScTypeBridge.nameAndFqn(designator)._1).filter(_.nonEmpty)
        case other                            => Some(ScTypeBridge.nameAndFqn(other)._1).filter(_.nonEmpty)
      }
    catch { case NonFatal(_) => None }
  }

  def declared(tp: ScTypeParam): Seq[Declared] = {
    val fromContext = tp.contextBounds.flatMap { b =>
      b.typeElement.`type`().toOption.flatMap(tcName(_, b)).orElse(Some(b.typeElement.getText.takeWhile(_ != '['))).map(ContextBound(b, _))
    }
    val fromImplicits = implicitClauses(tp.owner).flatMap(_.parameters).flatMap { p =>
      p.`type`().toOption.flatMap {
        case t @ ParameterizedType(_, args) if args.exists {
              case tpt: TypeParameterType => tpt.name == tp.name
              case _                      => false
            } =>
          tcName(t, p).map(ImplicitParam(p, _))
        case _ => None
      }
    }
    fromContext ++ fromImplicits
  }

  def implicitClauses(owner: ScTypeParametersOwner): Seq[ScParameterClause] = owner match {
    case f: ScFunction => f.paramClauses.clauses.filter(_.isImplicit)
    case c: ScClass    => c.clauses.toSeq.flatMap(_.clauses).filter(_.isImplicit)
    case _             => Nil
  }

  /** Import path for a typeclass name as users write it (`cats.Monad`, `cats.effect.Sync`, `cats.MonadThrow`). */
  def importPath(tc: String): String = {
    val canonical = Typeclasses.canonical(tc).getOrElse(tc)
    Typeclasses.byName.get(canonical) match {
      case Some(info) if info.library == Lib.CatsEffect =>
        if (tc == "Console") "cats.effect.std.Console" else s"cats.effect.$tc"
      case _ => s"cats.$tc"
    }
  }

  /** Context-bound text for a type parameter: `Monad` (bounds are written without type arguments). */
  def boundText(tc: String): String = tc

  /**
   * Makes `tc` available for `tp`: replaces an existing weaker bound that `tc` implies (Functor → Monad), otherwise
   * adds a new one. Scala 2 forbids context bounds next to an implicit parameter list, so in that case the bound is
   * added as an implicit parameter.
   */
  def strengthen(tp: ScTypeParam, tc: String): Unit = {
    // `add` may replace `tp` itself, so the import is anchored on the owner, which stays valid.
    val anchor: PsiElement = tp.owner
    val implied = Typeclasses.implied(tc)
    declared(tp).find(d => implied.contains(d.canonical) && d.canonical != Typeclasses.canonical(tc).getOrElse(tc)) match {
      case Some(ContextBound(bound, _)) =>
        val replacement = ScalaPsiElementFactory.createTypeElementFromText(boundText(tc), bound, null)
        withoutReformat(bound)(bound.typeElement.replace(replacement))
      case Some(ImplicitParam(param, _)) =>
        param.typeElement.foreach { te =>
          val args = te.getText.dropWhile(_ != '[')
          te.replace(ScalaPsiElementFactory.createTypeElementFromText(s"$tc${if (args.isEmpty) s"[${tp.name}]" else args}", te, null))
        }
      case None => add(tp, tc)
    }
    ensureImport(tc, anchor)
  }

  private def add(tp: ScTypeParam, tc: String): Unit = {
    val clauses = implicitClauses(tp.owner)
    if (clauses.nonEmpty && !tp.isInScala3File) {
      val clause   = clauses.last
      val taken    = clause.parameters.map(_.name).toSet
      val name     = if (taken.contains(tp.name)) s"${tp.name}${tc}" else tp.name
      val features = ScalaFeatures.forPsiOrDefault(tp)
      val param    = ScalaPsiElementFactory.createParameterFromText(s"$name: $tc[${tp.name}]", features)(using ProjectContext.fromProject(tp.getProject))
      clause.addParameter(param)
    } else {
      val newTp = ScalaPsiElementFactory.createTypeParameterFromText(s"${tp.getText}: ${boundText(tc)}", ScalaFeatures.forPsiOrDefault(tp))(
        using ProjectContext.fromProject(tp.getProject)
      )
      withoutReformat(tp)(tp.replace(newTp))
    }
  }

  /**
   * Runs a bound edit with postponed reformatting disabled: the default Scala code style would otherwise turn the
   * edited `F[_]: Sync` into `F[_] : Sync`, touching formatting the user didn't ask us to change.
   */
  private def withoutReformat(place: PsiElement)(edit: => Unit): Unit =
    CodeStyleManager.getInstance(place.getProject).performActionWithFormatterDisabled((() => edit): Runnable)

  /**
   * Removes one context bound (`: Monad`) from its type parameter by deleting the PSI range from its colon, so the
   * rest of the user's formatting is kept (re-creating the parameter from text would reformat it).
   */
  def removeContextBound(bound: ScContextBound): Unit = {
    import com.intellij.psi.PsiWhiteSpace
    val parent      = bound.getParent
    val colonInside = Option(bound.getFirstChild).exists(_.getText == ":")
    var first: PsiElement = bound
    if (!colonInside) {
      var p = bound.getPrevSibling
      while (p != null && p.isInstanceOf[PsiWhiteSpace]) p = p.getPrevSibling
      if (p != null && p.getText == ":") first = p
    }
    val before = first.getPrevSibling
    if (before != null && before.isInstanceOf[PsiWhiteSpace]) first = before
    val from = first
    withoutReformat(parent)(parent.deleteChildRange(from, bound))
  }

  /** Adds `import cats.Monad` (or the cats-effect equivalent) unless the name already resolves here. */
  def ensureImport(tc: String, place: PsiElement): Unit = {
    val resolves =
      try
        ScalaPsiElementFactory.createTypeElementFromText(tc, place, null) match {
          case s: ScSimpleTypeElement => s.reference.exists(_.resolve() != null)
          case _                      => false
        }
      catch { case NonFatal(_) => false }
    if (!resolves) ScImportsHolder.forNewImportInsertion(place).addImportForPath(importPath(tc), place)
  }

  /** Adds `import a.b.member` (e.g. `cats.effect.unsafe.implicits.global`) unless an import with that text exists. */
  def ensureMemberImport(path: String, place: PsiElement): Unit = {
    val present = place.withContexts.exists {
      case h: ScImportsHolder => h.getImportStatements.exists(_.importExprs.exists(_.getText == path))
      case _                  => false
    }
    if (!present) ScImportsHolder.forNewImportInsertion(place).addImportForPath(path, place)
  }

  /** The type parameter of an enclosing owner named `name`, if it's declared in source. */
  def typeParam(place: PsiElement, name: String): Option[ScTypeParam] =
    place.withContexts.collect { case o: ScTypeParametersOwner => o }.flatMap(_.typeParameters.find(_.name == name)).nextOption()
}
