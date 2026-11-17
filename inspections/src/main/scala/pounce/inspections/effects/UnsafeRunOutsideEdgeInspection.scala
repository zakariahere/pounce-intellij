package pounce.inspections.effects

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.psi.util.InheritanceUtil
import com.intellij.psi.{PsiClass, PsiElement}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import pounce.core.Lib
import pounce.core.settings.PounceSettings
import pounce.inspections.PounceInspection
import pounce.inspections.util.Typing

import scala.util.control.NonFatal

/**
 * B4. `unsafeRunSync()` & co. outside "the edge of the world". Allowed:
 *  - inside classes extending anything in Settings | Pounce | unsafeRun allow-list (IOApp, Play controllers, ...)
 *    and Play's own Future-based extension points (action functions, filters, error handlers);
 *  - inside the argument of a Play `Action.async { ... }` (the IO → Future bridge) or `addStopHook(...)`;
 *  - inside `main` methods;
 *  - in test sources.
 */
final class UnsafeRunOutsideEdgeInspection extends PounceInspection {

  import UnsafeRunOutsideEdgeInspection.*

  override protected def requiredLibraries: Set[Lib] = Set(Lib.CatsEffect)

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case ref: ScReferenceExpression if unsafeNames.contains(ref.refName) && Typing.resolvedOwner(ref).exists(ioOwners.contains) =>
      if (!allowed(ref)) report(holder, ref.nameId, message(ref.refName))
    case _ =>
  }

  private def allowed(ref: ScReferenceExpression): Boolean =
    inTestSources(ref) || inMain(ref) || inBridgeArgument(ref) || inAllowedClass(ref)

  private def inTestSources(e: PsiElement): Boolean =
    try {
      val vfile = Option(e.getContainingFile).flatMap(f => Option(f.getOriginalFile.getVirtualFile))
      vfile.exists(ProjectFileIndex.getInstance(e.getProject).isInTestSourceContent)
    } catch { case NonFatal(_) => false }

  private def inMain(e: PsiElement): Boolean =
    e.parentsInFile.exists {
      case f: ScFunctionDefinition => f.name == "main"
      case _                       => false
    }

  /** Inside the argument list of `Action.async(...)` / `ActionBuilder#async` / `ApplicationLifecycle#addStopHook`. */
  private def inBridgeArgument(e: PsiElement): Boolean =
    e.parentsInFile.exists {
      case call: ScMethodCall => isBridgeCall(call) && call.args.isAncestorOf(e)
      case _                  => false
    }

  private def isBridgeCall(call: ScMethodCall): Boolean = {
    def bridge(r: ScReferenceExpression) =
      bridgeMethods.contains(r.refName) && Typing.resolvedOwner(r).exists(o => o.startsWith("play.api.") || o.startsWith("play.mvc."))
    call.getInvokedExpr match {
      case r: ScReferenceExpression => bridge(r)
      case c: ScMethodCall =>
        c.getInvokedExpr match {
          case r: ScReferenceExpression => bridge(r) // Action.async(parse.json) { ... }
          case _                        => false
        }
      case _ => false
    }
  }

  private def inAllowedClass(e: PsiElement): Boolean = {
    val configured = PounceSettings().unsafeRunAllowList.split(',').map(_.trim).filter(_.nonEmpty).toSeq
    val patterns   = configured ++ builtInAllowed
    e.parentsInFile.exists {
      case td: ScTemplateDefinition => patterns.exists(matches(td, _))
      case _                        => false
    }
  }

  private def matches(c: PsiClass, pattern: String): Boolean =
    try
      if (pattern.endsWith("._")) {
        val prefix = pattern.stripSuffix("_")
        Option(c.getQualifiedName).exists(_.startsWith(prefix)) || c.getSupers.exists(s => Option(s.getQualifiedName).exists(_.startsWith(prefix)))
      } else InheritanceUtil.isInheritor(c, pattern) || Option(c.getQualifiedName).contains(pattern)
    catch { case NonFatal(_) => false }
}

object UnsafeRunOutsideEdgeInspection {

  val unsafeNames: Set[String] = Set(
    "unsafeRunSync",
    "unsafeRunAndForget",
    "unsafeRunTimed",
    "unsafeToFuture",
    "unsafeToFutureCancelable",
    "unsafeRunCancelable",
    "unsafeRunAsync",
    "unsafeRunAsyncOutcome",
    "unsafeToCompletableFuture"
  )

  /** `unsafeRunSync` & co. live in IO itself and in the JVM-specific IOPlatform parent. */
  val ioOwners: Set[String] = Set("cats.effect.IO", "cats.effect.IOPlatform")

  /** Play methods whose function argument is expected to return a Future: legitimate IO → Future bridges. */
  val bridgeMethods: Set[String] = Set("async", "addStopHook")

  /** Play's Future-based extension points, always allowed in addition to the configured list. */
  val builtInAllowed: Seq[String] = Seq(
    "play.api.mvc.ActionFunction",
    "play.api.mvc.Filter",
    "play.api.mvc.EssentialFilter",
    "play.api.http.HttpErrorHandler"
  )

  def message(name: String): String =
    s"$name outside the edge of the world: run effects from IOApp, main, tests or a Play action (allow-list in Settings | Pounce)"
}
