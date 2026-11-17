package pounce.inspections.util

import com.intellij.psi.{PsiElement, PsiMember, PsiNamedElement}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.ImplicitArgumentsOwner
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScObject}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt, TypePresentationContext}
import pounce.core.Effects.Kind
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.{Effects, Imports}
import pounce.core.settings.ImportStyle

import scala.util.control.NonFatal

/** Type queries shared by the inspections; every function is total (failures become `None` / `false`). */
object Typing {

  def typeOf(e: ScExpression): Option[ScType] =
    try e.`type`().toOption
    catch { case NonFatal(_) => None }

  def kind(e: ScExpression): Kind =
    try Effects.exprKind(e)
    catch { case NonFatal(_) => Kind.NotAnEffect }

  def kind(t: ScType, place: PsiElement): Kind =
    try Effects.kindOf(t, place)
    catch { case NonFatal(_) => Kind.NotAnEffect }

  /** IO, Resource, EitherT, ... or a tagless `F[A]` with a Functor-implying bound. */
  def isEffect(e: ScExpression): Boolean = kind(e) match {
    case Kind.Lazy | Kind.Tagless => true
    case _                        => false
  }

  def isEffect(t: ScType, place: PsiElement): Boolean = kind(t, place) match {
    case Kind.Lazy | Kind.Tagless => true
    case _                        => false
  }

  def ty(t: ScType, place: PsiElement): Option[Ty] =
    try Some(ScTypeBridge.toTy(t, place))
    catch { case NonFatal(_) => None }

  /** Head FQN of a (dealiased) type, or the type parameter name for rigid `F[A]`. */
  def headKey(t: ScType, place: PsiElement): Option[String] = ty(t, place).flatMap {
    case Ty.Con(name, _, _, true) => Some("?" + name)
    case Ty.Con(_, _, fqn, _)     => Some(catsEffectAlias(Ty.canonicalFqn(fqn))).filter(_.nonEmpty)
    case _                        => None
  }

  /** `cats.effect.Resource` (the package-object alias) and friends, as their `cats.effect.kernel` definitions. */
  private val kernelAliases: Set[String] =
    Set("Resource", "Ref", "Deferred", "Fiber", "Outcome", "Poll", "Clock", "Unique", "MonadCancel", "Spawn", "Concurrent",
      "GenTemporal", "Temporal", "Sync", "Async", "Cont", "ParallelF")

  private def catsEffectAlias(fqn: String): String = {
    val name = fqn.substring(fqn.lastIndexOf('.') + 1)
    val prefix = fqn.stripSuffix(name)
    if ((prefix == "cats.effect.package." || prefix == "cats.effect.") && kernelAliases(name)) s"cats.effect.kernel.$name" else fqn
  }

  def headFqn(t: ScType, place: PsiElement): Option[String] = headKey(t, place).filterNot(_.startsWith("?"))

  /** Type arguments of a parameterized type (after dealiasing). */
  def typeArgs(t: ScType, place: PsiElement): Seq[ScType] = {
    given Context = Context(place)
    try
      t.removeAliasDefinitions().widen match {
        case ParameterizedType(_, args) => args
        case _                          => Nil
      }
    catch { case NonFatal(_) => Nil }
  }

  def lastTypeArg(t: ScType, place: PsiElement): Option[ScType] = typeArgs(t, place).lastOption

  /** `F` for a tagless `F[A]`. */
  def rigidHead(t: ScType): Option[String] = (try t.widen catch { case NonFatal(_) => t }) match {
    case ParameterizedType(tpt: TypeParameterType, _) => Some(tpt.name)
    case _                                            => None
  }

  def isUnit(t: ScType): Boolean = t.isUnit

  def isFuture(t: ScType, place: PsiElement): Boolean = headFqn(t, place).contains("scala.concurrent.Future")

  def show(t: ScType, place: PsiElement): String =
    try {
      given TypePresentationContext = TypePresentationContext(place)
      given Context                 = Context(place)
      t.presentableText
    } catch { case NonFatal(_) => t.toString }

  def equivalent(a: ScType, b: ScType, place: PsiElement): Boolean = {
    given Context = Context(place)
    try a.equiv(b)
    catch { case NonFatal(_) => false }
  }

  def conforms(sub: ScType, sup: ScType, place: PsiElement): Boolean = {
    given Context = Context(place)
    try sub.conforms(sup)
    catch { case NonFatal(_) => false }
  }

  // ------------------------------------------------------------------------------------------------------------
  // Resolution helpers
  // ------------------------------------------------------------------------------------------------------------

  /** Qualified name (without the `$` of objects) of the class/object that declares a resolved element. */
  def ownerFqn(resolved: PsiElement): Option[String] = {
    val owner = resolved match {
      case o: ScObject => Option(o.containingClass)
      case m: PsiMember => Option(m.getContainingClass)
      case n: PsiNamedElement =>
        ScalaPsiUtil.inNameContext.unapply(n).collect { case m: ScMember => m.containingClass }.flatMap(Option(_))
      case _ => None
    }
    owner.flatMap(c => Option(c.getQualifiedName)).map(_.stripSuffix("$"))
  }

  def resolvedOwner(ref: ScReferenceExpression): Option[String] =
    try Option(ref.resolve()).flatMap(ownerFqn)
    catch { case NonFatal(_) => None }

  /** The reference resolves to a member of a cats / cats-effect type (methods, syntax ops, companions). */
  def fromCats(ref: ScReferenceExpression): Boolean = resolvedOwner(ref).exists(_.startsWith("cats."))

  def resolvesTo(ref: ScReferenceExpression, ownerPrefix: String): Boolean = resolvedOwner(ref).exists(_.startsWith(ownerPrefix))

  /** Object (companion) a reference points at, e.g. `cats.effect.IO` for `IO` in `IO.pure`. */
  def objectFqn(e: ScExpression): Option[String] = e match {
    case r: ScReferenceExpression =>
      try
        r.resolve() match {
          case o: ScObject => Option(o.qualifiedName)
          case _ =>
            typeOf(r).collect { case ScDesignatorType(o: ScObject) => o.qualifiedName }
        }
      catch { case NonFatal(_) => None }
    case _ => None
  }

  // ------------------------------------------------------------------------------------------------------------
  // Checking replacements before offering them
  // ------------------------------------------------------------------------------------------------------------

  /** `text` parsed as if it were written in place of `at` (same scope), optionally under extra imports. */
  def synthetic(at: ScExpression, text: String, imports: Seq[String] = Nil): Option[ScExpression] =
    try {
      val full = if (imports.isEmpty) text else imports.map(i => s"import $i").mkString("{ ", "; ", s"; $text }")
      Option(ScalaPsiElementFactory.createExpressionWithContextFromText(full, at.getContext, at))
    } catch { case NonFatal(_) => None }

  /** Every reference resolves (and is applicable) and no implicit argument is missing. */
  def resolvesCleanly(e: ScExpression): Boolean =
    try
      e.depthFirst().forall {
        case r: ScReferenceExpression  => r.bind().exists(_.isApplicable()) && implicitsOk(r)
        case o: ImplicitArgumentsOwner => implicitsOk(o)
        case _                         => true
      }
    catch { case NonFatal(_) => false }

  private def implicitsOk(o: ImplicitArgumentsOwner): Boolean =
    o.findImplicitArguments.forall(_.args.forall(!_.isImplicitParameterProblem))

  /**
   * Does `text` typecheck in place of `target` with a type compatible with the original? Returns the syntax import
   * root to add (`Some(None)` = no import needed), or `None` when the replacement isn't valid here.
   *
   * @param syntax cats syntax module the replacement may need (`functor`, `traverse`, `effect.spawn`, ...)
   * @param sameType require the new type to be equivalent to (or conform to) the old one
   */
  def checkReplacement(
    target: ScExpression,
    text: String,
    syntax: Option[String],
    sameType: Boolean = true
  ): Option[Option[String]] = {
    val oldType = typeOf(target)
    if (sameType && oldType.isEmpty) return None
    def ok(e: ScExpression): Boolean =
      resolvesCleanly(e) && (!sameType || typeOf(e).exists { n =>
        val o = oldType.get
        (!n.isNothing || o.isNothing) && (equivalent(n, o, target) || conforms(n, o, target))
      })
    synthetic(target, text).filter(ok).map(_ => Option.empty[String]).orElse {
      syntax.flatMap { s =>
        val root = importRootFor(s, target)
        synthetic(target, text, Seq(Imports.importText(root, target))).filter(ok).map(_ => Some(root))
      }
    }
  }

  /**
   * The syntax import to add at `place`: follows the style already used in scope (granular `cats.syntax.functor._`
   * stays granular, `cats.implicits._` stays implicits), so a fix never adds `cats.syntax.all._` next to granular
   * imports (which would make the implicits ambiguous). Falls back to the configured / detected project style.
   */
  def importRootFor(syntax: String, place: PsiElement): String = {
    val inScope = try Imports.wildcardRootsInScope(place) catch { case NonFatal(_) => Set.empty[String] }
    val granular = inScope.exists(r => (r.startsWith("cats.syntax.") && r != "cats.syntax.all") || (r.startsWith("cats.effect.syntax.") && r != "cats.effect.syntax.all"))
    val style =
      if (granular) ImportStyle.Granular
      else if (inScope.contains("cats.implicits")) ImportStyle.Implicits
      else if (inScope.contains("cats.syntax.all")) ImportStyle.All
      else Imports.effectiveStyle(place)
    Imports.rootFor(syntax, style)
  }

  /** Wraps `e`'s text in parentheses when it's not a simple (postfix-safe) expression. */
  def operand(e: ScExpression): String = {
    import org.jetbrains.plugins.scala.lang.psi.api.expr.*
    e match {
      case _: ScReferenceExpression | _: ScMethodCall | _: ScGenericCall | _: ScParenthesisedExpr | _: ScTuple |
          _: ScBlockExpr | _: ScUnitExpr | _: org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral => e.getText
      case _ => s"(${e.getText})"
    }
  }
}
