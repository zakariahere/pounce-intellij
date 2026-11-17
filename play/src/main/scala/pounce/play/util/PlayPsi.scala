package pounce.play.util

import com.intellij.openapi.diagnostic.ControlFlowException
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.TextRange
import com.intellij.psi.util.InheritanceUtil
import com.intellij.psi.{PsiClass, PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScInterpolatedStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.*

import scala.util.control.NonFatal

/** Small PSI helpers shared by the Play features (config keys, messages, Guice). */
object PlayPsi {

  /** A plain (non-interpolated) string literal with a value. */
  def plainLiteral(e: PsiElement): Option[ScStringLiteral] = e match {
    case _: ScInterpolatedStringLiteral                    => None
    case lit: ScStringLiteral if lit.getValue != null     => Some(lit)
    case _                                                 => None
  }

  /** Range of the literal's content (between the quotes), relative to the literal. */
  def contentRange(lit: ScStringLiteral): TextRange = lit.contentRangeInParent

  /**
   * A string literal passed directly as an argument of a resolved method call.
   *
   * @param receiver the qualifier of the invoked method (`config` in `config.get[String]("k")`), if any
   * @param typeArgs explicit type arguments (`[String]` in `config.get[String]("k")`)
   */
  final case class LiteralCall(
    literal: ScStringLiteral,
    call: ScMethodCall,
    method: PsiMethod,
    owner: PsiClass,
    argIndex: Int,
    typeArgs: Seq[ScTypeElement],
    receiver: Option[ScExpression]
  ) {
    def name: String = method.getName
  }

  /** The call a literal is a direct argument of (resolved), or None. Cheap structural checks first. */
  def literalCall(lit: ScStringLiteral): Option[LiteralCall] =
    lit.getParent match {
      case args: ScArgumentExprList =>
        args.getParent match {
          case call: ScMethodCall if call.args == args =>
            val index = args.exprs.indexOf(lit)
            if (index < 0) None
            else
              for {
                method <- resolveMethod(call)
                owner  <- Option(method.getContainingClass)
              } yield LiteralCall(lit, call, method, owner, index, typeArgsOf(call), receiverOf(call))
          case _ => None
        }
      case _ => None
    }

  def resolveMethod(call: MethodInvocation): Option[PsiMethod] =
    try {
      // `messages("k")`, `Action { ... }`: the `apply` actually invoked, not the value / accessor it is called on
      call.applyOrUpdateElement.map(_.element) match {
        case Some(m: PsiMethod) => Some(m)
        case _ =>
          val direct = call.getInvokedExpr match {
            case r: ScReferenceExpression => Option(r.resolve())
            case g: ScGenericCall =>
              g.referencedExpr match {
                case r: ScReferenceExpression => Option(r.resolve())
                case _                        => None
              }
            case _ => None
          }
          direct match {
            case Some(m: PsiMethod) => Some(m)
            case _                  => call.target.map(_.element).collect { case m: PsiMethod => m }
          }
      }
    } catch {
      case e: ControlFlowException => throw e
      case NonFatal(_)             => None
    }

  def typeArgsOf(call: ScMethodCall): Seq[ScTypeElement] = call.getInvokedExpr match {
    case g: ScGenericCall => g.typeArguments.flatMap(_.typeElement)
    case _                => Nil
  }

  def receiverOf(call: ScMethodCall): Option[ScExpression] = call.getInvokedExpr match {
    case r: ScReferenceExpression => r.qualifier
    case g: ScGenericCall =>
      g.referencedExpr match {
        case r: ScReferenceExpression => r.qualifier
        case _                        => None
      }
    // `messagesApi.preferred(request)("key")`: the invoked expression itself is the receiver of `apply`
    case other => Some(other)
  }

  /** FQN without the trailing `$` of Scala objects. */
  def fqn(cls: PsiClass): String = Option(cls.getQualifiedName).map(_.stripSuffix("$")).getOrElse("")

  /** `cls` is one of `fqns` or inherits from one of them. */
  def isOwner(cls: PsiClass, fqns: Set[String]): Boolean =
    fqns.contains(fqn(cls)) || fqns.exists(f => InheritanceUtil.isInheritor(cls, f))

  def isInProjectContent(e: PsiElement): Boolean = {
    val file = e.getContainingFile
    if (file == null) false
    else {
      val vf = file.getOriginalFile.getVirtualFile
      vf != null && ProjectFileIndex.getInstance(e.getProject).isInContent(vf)
    }
  }

  /** Swallows ordinary failures (incomplete code, stale PSI) but never cancellation. */
  def safe[T](f: => T): Option[T] =
    try Option(f)
    catch {
      case e: ControlFlowException => throw e
      case NonFatal(_)             => None
    }
}
