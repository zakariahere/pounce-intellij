package pounce.inspections.effects

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{rightRangeInParent, stripped, underscore, MethodRepr}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.Lib
import pounce.inspections.PounceInspection
import pounce.inspections.util.Patterns.*
import pounce.inspections.util.Typing

/**
 * B7. The acquired value escapes `Resource#use`: `res.use(IO.pure)`, `res.use(x => IO.pure(x))`, `res.use(_.pure[F])`
 * return a value whose resource has already been released when the caller gets it. Also `res.allocated.map(_._1)`,
 * which drops the release action. Plain data values (numbers, strings) are not reported.
 */
final class ResourceEscapesUseInspection extends PounceInspection {

  import ResourceEscapesUseInspection.*

  override protected def requiredLibraries: Set[Lib] = Set(Lib.CatsEffect)

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case expr @ res `.use` f if isResource(res) && returnsItsArgument(f) && !isPlainData(expr) =>
      report(holder, f, useMessage)
    case expr @ alloc `.map` `_._1`() if isAllocated(alloc) =>
      report(holder, expr, allocatedMessage, range = rightRangeInParent(alloc, expr))
    case _ =>
  }

  private def isResource(e: ScExpression): Boolean =
    Typing.typeOf(e).flatMap(Typing.headFqn(_, e)).contains("cats.effect.kernel.Resource")

  private def isAllocated(e: ScExpression): Boolean = e match {
    case `.allocated`(res) => isResource(res)
    case _                 => false
  }

  /** `IO.pure`, `F.pure`, `x => IO.pure(x)`, `x => IO(x)`, `_.pure[F]`, `IO.pure(_)`. */
  private def returnsItsArgument(f: ScExpression): Boolean = stripped(f) match {
    case r: ScReferenceExpression                           => r.refName == "pure" && Typing.fromCats(r)
    case u: ScUnderscoreSection                             => u.bindingExpr.exists(returnsItsArgument)
    case Lambda1(p, PureOf(arg)) if isRefTo(arg, p)         => true
    case Lambda1(p, Delay(_, arg)) if isRefTo(arg, p)       => true
    case PureOf(arg) if underscore.unapply(arg)             => true // IO.pure(_)
    case Delay(_, arg) if underscore.unapply(arg)           => true // IO(_)
    case g: ScGenericCall =>
      g.referencedExpr match {
        case MethodRepr(_, Some(q), Some(ref), _) => ref.refName == "pure" && underscore.unapply(q) // _.pure[F]
        case _                                    => false
      }
    case _ => false
  }

  /** The resource's value type is plain data (AnyVal, String): returning it is harmless. */
  private def isPlainData(use: ScExpression): Boolean =
    Typing.typeOf(use).flatMap(Typing.lastTypeArg(_, use)).exists(isPlain(_, use))

  private def isPlain(t: ScType, place: PsiElement): Boolean =
    Typing.headFqn(t, place).exists(plainTypes.contains) || Typing.isUnit(t)
}

object ResourceEscapesUseInspection {
  val plainTypes: Set[String] =
    Set("scala.Int", "scala.Long", "scala.Short", "scala.Byte", "scala.Char", "scala.Boolean", "scala.Double", "scala.Float", "java.lang.String", "scala.Predef.String")

  val useMessage = "The acquired value escapes Resource.use: it has already been released when the caller gets it"

  val allocatedMessage = "allocated.map(_._1) drops the release action: the resource is never released"
}
