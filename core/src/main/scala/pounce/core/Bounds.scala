package pounce.core

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterClause, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType}
import pounce.core.types.ScTypeBridge

import scala.util.control.NonFatal

/**
 * Typeclass bounds in scope for the type parameters of tagless-final code.
 *
 * For `def save[F[_]: Sync](...)` or `class Repo[F[_]](implicit F: MonadThrow[F])`, `Bounds.at(place)("F")` returns
 * the canonical typeclass names `Set("Sync")` / `Set("MonadError")` - context bounds and implicit/using parameters,
 * from every enclosing method and class.
 */
object Bounds {

  /** A bound found in source, with the element that declares it (for quick fixes). */
  final case class Bound(typeParam: String, typeclass: String, canonical: String, declaredBy: PsiElement)

  def at(place: PsiElement): Map[String, Set[String]] =
    all(place).groupMap(_.typeParam)(_.canonical).view.mapValues(_.toSet).toMap

  def forParam(place: PsiElement, typeParam: String): Set[String] = at(place).getOrElse(typeParam, Set.empty)

  def all(place: PsiElement): Seq[Bound] = {
    val owners = place.withContexts.collect { case o: ScTypeParametersOwner => o }.toSeq
    owners.flatMap(boundsOf)
  }

  /** The nearest enclosing owner declaring type parameter `name` (where a "strengthen bound" fix edits). */
  def declaringTypeParam(place: PsiElement, name: String): Option[ScTypeParam] =
    place.withContexts.collect { case o: ScTypeParametersOwner => o }.flatMap(_.typeParameters.find(_.name == name)).nextOption()

  private def boundsOf(owner: ScTypeParametersOwner): Seq[Bound] =
    try {
      val tps = owner.typeParameters
      val fromContextBounds = tps.flatMap { tp =>
        tp.contextBound.flatMap(b => typeclassName(b, owner).map(n => Bound(tp.name, n, Typeclasses.canonical(n).getOrElse(n), tp)))
      }
      val fromImplicitParams = implicitClauses(owner).flatMap(_.parameters).flatMap(boundFromParam(_, owner))
      (fromContextBounds ++ fromImplicitParams).distinctBy(b => (b.typeParam, b.canonical))
    } catch {
      case NonFatal(_) => Seq.empty
    }

  private def implicitClauses(owner: ScTypeParametersOwner): Seq[ScParameterClause] = owner match {
    case f: ScFunction => f.paramClauses.clauses.filter(_.isImplicit)
    case c: ScClass    => c.clauses.toSeq.flatMap(_.clauses).filter(_.isImplicit)
    case _             => Seq.empty
  }

  private def boundFromParam(p: ScParameter, owner: PsiElement): Option[Bound] = {
    given Context = Context(p)
    p.`type`().toOption.flatMap {
      case ParameterizedType(designator, args) =>
        args.lastOption.orElse(args.headOption) match {
          case Some(tpt: TypeParameterType) =>
            val (name, _) = ScTypeBridge.nameAndFqn(designator)
            Some(Bound(tpt.name, name, Typeclasses.canonical(name).getOrElse(name), p))
          case _ => None
        }
      case _ => None
    }
  }

  /** Context bounds come as `Monad` (designator) or `Monad[F]` depending on version; normalise to the name. */
  private def typeclassName(t: ScType, place: PsiElement): Option[String] = {
    given Context = Context(place)
    t match {
      case ParameterizedType(designator, _) => Some(ScTypeBridge.nameAndFqn(designator)._1)
      case other                            => Some(ScTypeBridge.nameAndFqn(other)._1).filter(_.nonEmpty)
    }
  }

  /** Enclosing template, used to scope class-level bounds. */
  def enclosingTemplate(place: PsiElement): Option[ScTemplateDefinition] =
    place.withContexts.collect { case t: ScTemplateDefinition => t }.nextOption()
}
