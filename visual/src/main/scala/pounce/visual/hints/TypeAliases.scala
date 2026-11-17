package pounce.visual.hints

import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScPackaging
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import pounce.core.Effects
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.visual.util.VisualPsi


/**
 * D2: type-alias awareness. Finds the user's parameterised aliases (`type AppIO[A] = EitherT[IO, AppError, A]`) and
 * re-applies them when presenting types, so hints say `AppIO · User` instead of `EitherT[IO, AppError, *] · User`.
 *
 * Only aliases with at least one type parameter whose body is a type constructor application are used (an alias such as
 * `type Id[A] = A` or `type UserId = Int` would match far too much).
 */
object TypeAliases {

  /**
   * @param params  alias type parameters, in order
   * @param pattern the aliased type with the parameters turned into shape variables
   * @param valueParam the parameter that is the last argument of the aliased constructor (the "value" slot), if any
   */
  final case class Alias(name: String, params: List[String], pattern: Ty, valueParam: Option[String]) {

    /** Presents `subst` as `AppIO[User]`. */
    def render(subst: Ty.Subst): Ty =
      Ty.Con(name, params.map(p => subst(Ty.Var(p))))

    /** The effect part only: `AppIO` (or `Eff[IO, *]` when other parameters remain). */
    def renderEffect(subst: Ty.Subst): String = {
      val others = params.filterNot(valueParam.contains).map(p => subst(Ty.Var(p)))
      Ty.show(Ty.Lambda(name, others))
    }
  }

  /**
   * Aliases visible at `place`: members of enclosing templates, top-level aliases of the file (Scala 3), and package
   * objects of the file's package and its parents. Cached per file and enclosing template.
   */
  def inScope(place: PsiElement): Seq[Alias] = {
    val owners = place.withContexts.collect { case t: ScTemplateDefinition => t }.toList
    val key    = owners.map(_.getTextRange.getStartOffset).mkString("typeAliases@", ",", "")
    VisualPsi.fileCached[Seq[Alias]](place, key)(definitionsInScope(place).flatMap(fromDefinition).distinctBy(_.name))
  }

  def definitionsInScope(place: PsiElement): Seq[ScTypeAliasDefinition] = {
    val fromTemplates = place.withContexts.collect { case t: ScTemplateDefinition => t }.toSeq.flatMap { t =>
      t.aliases.collect { case d: ScTypeAliasDefinition => d }
    }
    val file = place.getContainingFile
    val topLevel = file match {
      case f: ScalaFile =>
        f.depthFirst(e => e == f || e.isInstanceOf[ScPackaging]).collect { case d: ScTypeAliasDefinition => d }.toSeq
      case _ => Seq.empty
    }
    fromTemplates ++ topLevel ++ packageObjectAliases(file)
  }

  private def packageObjectAliases(file: PsiFile): Seq[ScTypeAliasDefinition] = file match {
    case sf: ScalaFile =>
      val pkg = VisualPsi.safe(sf.getPackageName).getOrElse("")
      if (pkg.isEmpty) Seq.empty
      else {
        val parts    = pkg.split('.').toList
        val prefixes = parts.indices.map(i => parts.take(i + 1).mkString("."))
        val scope    = GlobalSearchScope.allScope(file.getProject)
        val manager  = ScalaPsiManager.instance(file.getProject)
        prefixes.flatMap { p =>
          VisualPsi.safe(manager.getCachedClass(scope, s"$p.package")).flatten.toSeq.flatMap {
            case o: ScObject if o.getContainingFile != file =>
              o.aliases.collect { case d: ScTypeAliasDefinition => d }
            case _ => Seq.empty
          }
        }
      }
    case _ => Seq.empty
  }

  def fromDefinition(d: ScTypeAliasDefinition): Option[Alias] = {
    val params = d.typeParameters.map(_.name).toList
    if (params.isEmpty) None
    else
      for {
        aliased <- VisualPsi.safe(d.aliasedType.toOption).flatten
        ty      <- VisualPsi.safe(ScTypeBridge.toTy(aliased, d, dealias = true))
        pattern = toPattern(ty, params.toSet)
        if pattern.isInstanceOf[Ty.Con]
      } yield {
        val valueParam = pattern match {
          case Ty.Con(_, args, _, _) =>
            args.lastOption.collect { case Ty.Var(v) if params.contains(v) => v }.filter { v =>
              args.init.forall(a => !a.vars.contains(v))
            }
          case _ => None
        }
        Alias(d.name, params, pattern, valueParam)
      }
  }

  /** Alias type parameters arrive as rigid constructors; make them shape variables. */
  def toPattern(t: Ty, params: Set[String]): Ty = t match {
    case Ty.Con(n, Nil, _, true) if params(n)  => Ty.Var(n)
    case Ty.Con(n, args, _, true) if params(n) => Ty.App(Ty.Var(n), args.map(toPattern(_, params)))
    case c @ Ty.Con(_, args, _, _)             => c.copy(args = args.map(toPattern(_, params)))
    case Ty.Fun(ps, r)                         => Ty.Fun(ps.map(toPattern(_, params)), toPattern(r, params))
    case Ty.Tuple(es)                          => Ty.Tuple(es.map(toPattern(_, params)))
    case l @ Ty.Lambda(_, fixed, _, _)         => l.copy(fixed = fixed.map(toPattern(_, params)))
    case other                                 => other
  }

  /** The first alias matching `t` exactly, with its substitution. */
  def matching(t: Ty, aliases: Seq[Alias]): Option[(Alias, Ty.Subst)] =
    aliases.iterator.flatMap(a => Ty.matchShape(a.pattern, t).map(a -> _)).nextOption()

  /** Re-applies aliases anywhere inside `t` (outermost first). */
  def present(t: Ty, aliases: Seq[Alias]): Ty =
    if (aliases.isEmpty) t
    else
      matching(t, aliases) match {
        case Some((a, s)) => a.render(s)
        case None =>
          t match {
            case c @ Ty.Con(_, args, _, _) => c.copy(args = args.map(present(_, aliases)))
            case Ty.Fun(ps, r)             => Ty.Fun(ps.map(present(_, aliases)), present(r, aliases))
            case Ty.Tuple(es)              => Ty.Tuple(es.map(present(_, aliases)))
            case other                     => other
          }
      }

  /**
   * Splits an effect type into the effect and the value: `EitherT[IO, AppError, User]` → (`AppIO`, `User`) when
   * `AppIO` is in scope, else (`EitherT[IO, AppError, *]`, `User`).
   *
   * @param full the dealiased type
   * @param raw  the same type with aliases kept (an alias we could not find, e.g. from a library, still shows)
   */
  def effectAndValue(full: Ty, raw: Ty, aliases: Seq[Alias]): Option[(String, String)] = full match {
    case Ty.Con(_, args, _, _) if args.nonEmpty =>
      val viaAlias = matching(full, aliases).collect {
        case (a, s) if a.valueParam.isDefined => (a.renderEffect(s), present(s(Ty.Var(a.valueParam.get)), aliases).show)
      }
      viaAlias.orElse {
        val value = raw match {
          case Ty.Con(rn, rargs, _, _) if rargs.nonEmpty && sameName(rn, full) => present(rargs.last, aliases).show
          case _                                                             => present(args.last, aliases).show
        }
        val effect = raw match {
          case Ty.Con(rn, rargs, _, _) if rargs.nonEmpty && !sameName(rn, full) => Ty.show(Ty.Lambda(rn, rargs.init))
          case _ => Effects.displayName(Ty.Con(full.headName.getOrElse("?"), args.map(present(_, aliases)), full.headFqn.getOrElse("")))
        }
        Some((effect, value))
      }
    case _ => None
  }

  private def sameName(n: String, t: Ty): Boolean = t.headName.contains(n)
}
