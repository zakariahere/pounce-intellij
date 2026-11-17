package pounce.inspections.simplifications

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{rightRangeInParent, stripped, underscore, MethodRepr}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.inspections.util.Patterns.{isRefTo, Lambda1}
import pounce.inspections.util.Typing
import pounce.inspections.{PounceInspection, ReplaceExpressionFix}

import scala.util.control.NonFatal

/** How a candidate rewrite is validated before it's offered. */
enum Check {

  /** The replacement's type must be equivalent to (or conform to) the original's. */
  case SameType

  /** The replacement must conform to the original's expected type (when it has one), e.g. `Some(x)` → `x.some`. */
  case ExpectedType
}

/**
 * A proposed rewrite of `target` into `text`.
 *
 * @param syntax    cats syntax module the replacement may need (`functor`, `traverse`, ...); imported by the fix
 * @param from      highlight from this sub-expression's end (`fa.|map(_ => ())|`) instead of the whole target
 * @param highlight highlight exactly this element (e.g. one generator of a for-comprehension)
 */
final case class Rewrite(
  target: ScExpression,
  text: String,
  syntax: Option[String] = None,
  from: Option[ScExpression] = None,
  highlight: Option[PsiElement] = None,
  check: Check = Check.SameType
)

/**
 * One simplification rule. `description` and `hint` are constants (they don't quote the user's code), so the
 * problem text reads like a rule (`fa.map(_ => ()) can be replaced with fa.void`) and tests can match it exactly.
 */
final class Rule(val description: String, val hint: String)(val rewrite: PartialFunction[ScExpression, Rewrite])

/**
 * Base for the Pillar C simplifications (zio-intellij style). Each rule is a short pattern match on the Scala
 * plugin's collection extractors; the base validates every candidate with [[Typing.checkReplacement]] (it must
 * resolve, have no missing implicits and keep a compatible type) and offers a fix that adds the cats syntax import
 * when the replacement needs one. One problem per expression: the first rule whose rewrite validates wins.
 */
abstract class SimplificationInspection(val rules: Seq[Rule]) extends PounceInspection {

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    // The collection extractors look through `{ ... }` and `( ... )` (`stripped`): match the inner expression only.
    case _: ScBlock | _: ScParenthesisedExpr =>
    case e: ScExpression =>
      rules.iterator
        .flatMap(r => candidate(r, e).map(r -> _))
        .flatMap { case (r, rw) => validate(rw).map(imp => (r, rw, imp)) }
        .nextOption()
        .foreach { case (rule, rw, importRoot) =>
          val range = rw.highlight
            .map(h => h.getTextRange.shiftLeft(rw.target.getTextRange.getStartOffset))
            .orElse(rw.from.map(q => rightRangeInParent(q, rw.target)))
            .orNull
          report(holder, rw.target, rule.description, Seq(new ReplaceExpressionFix(rule.hint, rw.target, rw.text, importRoot)), range = range)
        }
    case _ =>
  }

  private def candidate(r: Rule, e: ScExpression): Option[Rewrite] =
    try r.rewrite.lift(e)
    catch {
      case pce: ProcessCanceledException => throw pce
      case NonFatal(_)                   => None
    }

  private def validate(rw: Rewrite): Option[Option[String]] = rw.check match {
    case Check.SameType => Typing.checkReplacement(rw.target, rw.text, rw.syntax)
    case Check.ExpectedType =>
      val expected = try rw.target.expectedType() catch { case NonFatal(_) => None }
      expected.flatMap { exp =>
        Typing.checkReplacement(rw.target, rw.text, rw.syntax, sameType = false).filter { imp =>
          val imports = imp.toSeq.map(root => pounce.core.Imports.importText(root, rw.target))
          Typing.synthetic(rw.target, rw.text, imports).flatMap(Typing.typeOf).exists(t => Typing.conforms(t, exp, rw.target))
        }
      }
  }
}

/** Small helpers shared by the rules. */
object Shapes {

  def op(e: ScExpression): String = Typing.operand(e)

  def isEffect(e: ScExpression): Boolean = Typing.isEffect(e)

  /** The expression's type is an effect whose value type is `Unit` (`IO[Unit]`, `F[Unit]`). */
  def isUnitEffect(e: ScExpression): Boolean =
    isEffect(e) && Typing.typeOf(e).flatMap(Typing.lastTypeArg(_, e)).exists(Typing.isUnit)

  def headFqn(e: ScExpression): Option[String] = Typing.typeOf(e).flatMap(Typing.headFqn(_, e))

  def typeArgs(e: ScExpression): Seq[ScType] = Typing.typeOf(e).toSeq.flatMap(Typing.typeArgs(_, e))

  def show(t: ScType, place: PsiElement): String = Typing.show(t, place)

  /** A lambda body as source: a single expression as is, several statements in braces. */
  def bodyText(body: ScExpression): String = body match {
    case b: ScBlock if !b.isInstanceOf[ScBlockExpr] || b.statements.size > 1 =>
      b.statements match {
        case Seq(single: ScExpression) => single.getText
        case _                         => s"{\n${b.statements.map(_.getText).mkString("\n")}\n}"
      }
    case other => other.getText
  }

  /** Name to use for a lambda parameter / generator pattern (`a`, `_`). */
  def paramName(p: org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter): String =
    if (p.isWildcard) "_" else p.name

  /**
   * `X(args)` / `X.apply(args)` / `X[T](args)` with a simple name `X`. The Scala plugin's `MethodRepr` reports such
   * apply calls without a method reference, so constructor-like calls are matched here instead.
   */
  object ApplyOf {
    def unapply(e: ScExpression): Option[(ScReferenceExpression, Seq[ScExpression])] = stripped(e) match {
      case call: ScMethodCall =>
        call.getInvokedExpr match {
          case r: ScReferenceExpression if !r.isQualified => Some((r, call.argumentExpressions))
          case r: ScReferenceExpression if r.refName == "apply" =>
            r.qualifier.collect { case q: ScReferenceExpression if !q.isQualified => (q, call.argumentExpressions) }
          case g: ScGenericCall =>
            g.referencedExpr match {
              case r: ScReferenceExpression if !r.isQualified => Some((r, call.argumentExpressions))
              case _                                          => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** `q.name(a*)(b*)`: a curried call such as `opt.fold(z)(f)` or `xs.foldLeft(z)(op)`. */
  object Curried {
    def unapply(e: ScExpression): Option[(ScExpression, String, Seq[ScExpression], Seq[ScExpression])] = stripped(e) match {
      case outer: ScMethodCall =>
        outer.getInvokedExpr match {
          case inner: ScMethodCall =>
            inner.getInvokedExpr match {
              case r: ScReferenceExpression if r.qualifier.isDefined =>
                Some((r.qualifier.get, r.refName, inner.argumentExpressions, outer.argumentExpressions))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** `Right(_)`, `Right.apply`, `x => Right(x)`, `_.asRight`, `_.asRight[E]` (and the same for Left / Some). */
  final class CtorFn(ctor: String, syntaxName: String) {
    def unapply(e: ScExpression): Boolean = stripped(e) match {
      case r: ScReferenceExpression =>
        (r.refName == ctor && !r.isQualified) || (r.refName == "apply" && r.qualifier.exists(q => q.getText == ctor))
      case ApplyOf(r, Seq(u)) if r.refName == ctor => underscore.unapply(u)
      case MethodRepr(_, Some(u), Some(r), Seq()) if r.refName == syntaxName => underscore.unapply(u)
      case g: ScGenericCall =>
        g.referencedExpr match {
          case MethodRepr(_, Some(u), Some(r), Seq()) if r.refName == syntaxName => underscore.unapply(u)
          case _                                                                  => false
        }
      case Lambda1(p, ApplyOf(r, Seq(arg))) if r.refName == ctor => isRefTo(arg, p)
      case _                                                    => false
    }
  }

  val RightFn = new CtorFn("Right", "asRight")
  val LeftFn  = new CtorFn("Left", "asLeft")
  val SomeFn  = new CtorFn("Some", "some")

  /** `Right(a)` / `Left(e)` / `Some(a)`: a call of the standard constructor with one argument. */
  final class CtorCall(ctor: String, fqns: Set[String]) {
    def unapply(e: ScExpression): Option[ScExpression] = e match {
      case ApplyOf(r, Seq(arg)) if r.refName == ctor && headFqn(e).exists(fqns.contains) => Some(arg)
      case _                                                                            => None
    }
  }

  val RightCall = new CtorCall("Right", Set("scala.util.Right", "scala.util.Either"))
  val LeftCall  = new CtorCall("Left", Set("scala.util.Left", "scala.util.Either"))
  val SomeCall  = new CtorCall("Some", Set("scala.Some", "scala.Option"))

  /** `None` (the standard one). */
  object NoneRef {
    def unapply(e: ScExpression): Boolean = stripped(e) match {
      case r: ScReferenceExpression => r.refName == "None" && headFqn(r).exists(f => f == "scala.None" || f == "scala.Option")
      case _                        => false
    }
  }

  /** `{ case (a, b) => body }` with simple names: returns the names and the body. */
  object CaseTuple {
    def unapply(e: ScExpression): Option[(Seq[String], ScExpression)] = stripped(e) match {
      case b: ScBlockExpr =>
        b.caseClauses.map(_.caseClauses).collect {
          case Seq(clause) if clause.guard.isEmpty =>
            clause.pattern.zip(clause.expr).collect {
              case (t: org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScTuplePattern, body) if simpleNames(t.subpatterns).isDefined =>
                (simpleNames(t.subpatterns).get, body)
            }
        }.flatten
      case _ => None
    }
  }

  /** Names of simple patterns (`a`, `_`), or None if any is complex. */
  def simpleNames(ps: Seq[org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScPattern]): Option[Seq[String]] = {
    val names = ps.map {
      case r: ScReferencePattern => Some(r.name)
      case _: ScWildcardPattern  => Some("_")
      case _                     => None
    }
    if (names.forall(_.isDefined)) Some(names.flatten) else None
  }

  /** Range helper for tests and rules: an element's range inside `parent`. */
  def rangeIn(e: PsiElement, parent: PsiElement): TextRange = e.getTextRange.shiftLeft(parent.getTextRange.getStartOffset)
}
