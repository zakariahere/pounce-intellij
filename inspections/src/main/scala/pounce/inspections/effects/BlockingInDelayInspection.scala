package pounce.inspections.effects

import com.intellij.codeInspection.{LocalQuickFix, ProblemsHolder}
import com.intellij.psi.util.InheritanceUtil
import com.intellij.psi.{PsiClass, PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.codeInspection.collections.stripped
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructorInvocation
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import pounce.core.Lib
import pounce.inspections.util.Patterns.{invokedRef, Delay}
import pounce.inspections.util.Typing
import pounce.inspections.{PounceInspection, ReplaceExpressionFix}

import scala.util.control.NonFatal

/**
 * B3. A known blocking call (see [[BlockingCalls]]) inside `IO(...)`, `IO.delay`, `Sync[F].delay` or `F.delay` runs
 * on the compute pool and starves it. Fix: `IO.blocking` / `Sync[F].blocking` (or `interruptible`).
 */
final class BlockingInDelayInspection extends PounceInspection {

  import BlockingInDelayInspection.*

  override protected def requiredLibraries: Set[Lib] = Set(Lib.CatsEffect)

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case Delay(call, body) =>
      blockingCalls(body).foreach { case (at, api) =>
        report(holder, at, message(api.label, delayName(call)), fixes(call, body, api))
      }
    case _ =>
  }

  /** `IO.delay` / `IO(...)` / `Sync[F].delay` for the message. */
  private def delayName(call: ScMethodCall): String = invokedRef(call) match {
    case Some(r) if r.refName == "IO" => "IO(...)"
    case Some(r)                      => r.qualifier.map(q => s"${q.getText}.${r.refName}").getOrElse(r.refName)
    case None                         => "delay"
  }

  private def fixes(call: ScMethodCall, body: ScExpression, api: BlockingApi): Seq[LocalQuickFix] = {
    val names = Seq("blocking") ++ (if (api.interruptible) Seq("interruptible") else Nil)
    val renames = names.flatMap(n => renamed(call, n).map(text => new ReplaceExpressionFix(hint(n, call), call, text)))
    renames ++ sleepFix(call, body).toSeq
  }

  /** The same call with the delay method renamed: `IO.blocking(...)`, `Sync[F].blocking(...)`. */
  private def renamed(call: ScMethodCall, name: String): Option[String] = invokedRef(call).flatMap { ref =>
    val typeArgs = call.getInvokedExpr match {
      case g: ScGenericCall => g.typeArgs.getText
      case _                => ""
    }
    val qualText = ref.qualifier match {
      case Some(q)                  => Some(q.getText)
      case None if ref.refName == "IO" => Some("IO")
      case None                     => None
    }
    qualText.map(q => s"$q.$name$typeArgs${call.args.getText}")
  }

  /** `IO(Thread.sleep(n))` → `IO.sleep(n.millis)`. */
  private def sleepFix(call: ScMethodCall, body: ScExpression): Option[LocalQuickFix] = stripped(body) match {
    case c: ScMethodCall if isIoDelay(call) =>
      (invokedRef(c), c.argumentExpressions) match {
        case (Some(r), Seq(arg)) if r.refName == "sleep" && Typing.resolvedOwner(r).contains("java.lang.Thread") =>
          val pretty = s"IO.sleep(${Typing.operand(arg)}.millis)"
          Typing.checkReplacement(call, pretty, None, sameType = false) match {
            case Some(_) => Some(new ReplaceExpressionFix(sleepHint, call, pretty))
            case None    => Some(new SleepFix(call, pretty))
          }
        case _ => None
      }
    case _ => None
  }

  private def isIoDelay(call: ScMethodCall): Boolean = invokedRef(call).exists { r =>
    r.refName == "IO" || r.qualifier.exists(q => Typing.objectFqn(q).contains("cats.effect.IO"))
  }

  /** Blocking calls in `body`, not descending into nested delay / blocking calls or local defs. */
  private def blockingCalls(body: ScExpression): Seq[(PsiElement, BlockingApi)] = {
    val descend: PsiElement => Boolean = {
      case c: ScMethodCall if c ne body =>
        !Delay.isDelayCall(c) && !invokedRef(c).exists(r => Set("blocking", "interruptible", "interruptibleMany").contains(r.refName))
      case _: ScFunction => false
      case _             => true
    }
    body.depthFirst(descend).flatMap {
      case r: ScReferenceExpression   => matchMethod(r).map(api => (r.nameId, api))
      case n: ScNewTemplateDefinition => matchConstructor(n).map(api => (n, api))
      case _                          => None
    }.toSeq.distinctBy(_._1)
  }

  private def matchMethod(r: ScReferenceExpression): Option[BlockingApi] =
    try
      r.resolve() match {
        case m: PsiMethod =>
          val owner = m.getContainingClass
          if (owner == null) None else lookup(owner, m.getName)
        case _ => None
      }
    catch { case NonFatal(_) => None }

  private def matchConstructor(n: ScNewTemplateDefinition): Option[BlockingApi] =
    n.firstConstructorInvocation.flatMap { (ci: ScConstructorInvocation) =>
      ci.reference.flatMap(ref => Option(ref.resolve())).collect {
        case c: PsiClass => c
        case m: PsiMethod if m.isConstructor && m.getContainingClass != null => m.getContainingClass
      }
    }.flatMap(c => lookup(c, "<init>"))

  private def lookup(owner: PsiClass, member: String): Option[BlockingApi] = {
    val fqn = Option(owner.getQualifiedName).map(_.stripSuffix("$")).getOrElse("")
    if (BlockingCalls.excludedOwners.contains(fqn)) None
    else
      BlockingCalls.all.find { api =>
        (api.members.isEmpty || api.members.contains(member)) &&
        (api.owner == fqn || (api.subclasses && member != "<init>" && isInheritor(owner, api.owner)))
      }
  }

  private def isInheritor(c: PsiClass, base: String): Boolean =
    try InheritanceUtil.isInheritor(c, base)
    catch { case NonFatal(_) => false }
}

object BlockingInDelayInspection {

  def message(call: String, delay: String): String =
    s"Blocking call ($call) inside $delay runs on the compute pool and starves it"

  def hint(name: String, call: ScMethodCall): String = invokedRef(call) match {
    case Some(r) if r.refName == "IO" || r.qualifier.exists(q => Typing.objectFqn(q).contains("cats.effect.IO")) => s"Replace with IO.$name"
    case Some(r) => r.qualifier.map(q => s"Replace with ${q.getText}.$name").getOrElse(s"Replace with $name")
    case None    => s"Replace with $name"
  }

  val sleepHint = "Replace with IO.sleep"

  /** `IO.sleep(n.millis)` + `import scala.concurrent.duration._` when the duration syntax isn't imported. */
  private final class SleepFix(call: ScMethodCall, text: String)
      extends org.jetbrains.plugins.scala.codeInspection.AbstractFixOnPsiElement[ScMethodCall](sleepHint, call) {
    override protected def doApplyFix(e: ScMethodCall)(implicit project: com.intellij.openapi.project.Project): Unit = {
      val replaced = pounce.inspections.Rewriter.replace(e, text, None)
      pounce.core.Imports.ensureWildcardImport("scala.concurrent.duration", replaced)
    }
  }
}
