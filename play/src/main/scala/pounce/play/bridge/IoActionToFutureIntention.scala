package pounce.play.bridge

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDefinition, ScPatternDefinition, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt}
import pounce.core.Libraries
import pounce.play.util.PlayPsi

/**
 * E4: turns a Play action whose body is an `IO[Result]` (or `F[Result]` with a `Dispatcher[F]` in scope) into the
 * Play 3 bridge `Action.async { io.unsafeToFuture() }`, using what the class already has:
 *  - an implicit `IORuntime` in scope (or `import cats.effect.unsafe.implicits.global`) → `io.unsafeToFuture()`
 *  - a non-implicit `runtime: IORuntime` → `io.unsafeToFuture()(runtime)`
 *  - a `dispatcher: Dispatcher[IO]` / `Dispatcher[F]` → `dispatcher.unsafeToFuture(io)`
 *  - nothing → `io.unsafeToFuture()` plus `import cats.effect.unsafe.implicits.global` (said in the intention text)
 */
final class IoActionToFutureIntention extends PsiElementBaseIntentionAction {

  import IoActionToFutureIntention.*

  override def getFamilyName: String = "Convert IO action to Action.async with unsafeToFuture"

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    analyze(element) match {
      case Some(plan) =>
        setText(plan.text)
        true
      case None => false
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    analyze(element).foreach { plan =>
      val result = plan.result
      val bridged = plan.runtime match {
        case Runtime.Dispatcher(name, _) => s"$name.unsafeToFuture(${result.getText})"
        case Runtime.Explicit(name)      => s"${wrap(result)}.unsafeToFuture()($name)"
        case _                           => s"${wrap(result)}.unsafeToFuture()"
      }
      result.replace(ScalaPsiElementFactory.createExpressionWithContextFromText(bridged, result.getContext, result))
      plan.newInvoked.foreach { text =>
        val invoked = plan.call.getInvokedExpr
        invoked.replace(ScalaPsiElementFactory.createExpressionWithContextFromText(text, invoked.getContext, invoked))
      }
      if (plan.runtime == Runtime.ImportGlobal)
        ScImportsHolder.forNewImportInsertion(plan.call).addImportForPath(GlobalImport)
    }

  override def startInWriteAction(): Boolean = true
}

object IoActionToFutureIntention {

  val GlobalImport = "cats.effect.unsafe.implicits.global"

  enum Runtime {
    case Implicit
    case ImportGlobal
    case Explicit(name: String)
    case Dispatcher(name: String, effect: String)
  }

  /**
   * @param call       the `Action { ... }` / `Action.async { ... }` call
   * @param result     the expression producing `IO[Result]` (the last expression of the action body)
   * @param newInvoked replacement for the invoked part (`Action` → `Action.async`) or None when already async
   */
  final case class Plan(call: ScMethodCall, result: ScExpression, runtime: Runtime, newInvoked: Option[String]) {
    def text: String = runtime match {
      case Runtime.ImportGlobal        => s"Convert to Action.async { io.unsafeToFuture() } (adds import $GlobalImport)"
      case Runtime.Explicit(n)         => s"Convert to Action.async { io.unsafeToFuture()($n) }"
      case Runtime.Dispatcher(n, _)    => s"Convert to Action.async { $n.unsafeToFuture(io) }"
      case Runtime.Implicit            => "Convert to Action.async { io.unsafeToFuture() }"
    }
  }

  def wrap(e: ScExpression): String = e match {
    case _: ScReferenceExpression | _: ScMethodCall | _: ScGenericCall | _: ScParenthesisedExpr | _: ScBlockExpr => e.getText
    case _ => s"(${e.getText})"
  }

  def analyze(element: PsiElement): Option[Plan] =
    if (element == null || !Libraries.hasPlay(element) || !Libraries.hasCatsEffect(element)) None
    else
      Iterator
        .iterate(PsiTreeUtil.getParentOfType(element, classOf[ScMethodCall]))(c => PsiTreeUtil.getParentOfType(c, classOf[ScMethodCall]))
        .takeWhile(_ != null)
        .take(6)
        .flatMap(planFor)
        .nextOption()

  private def planFor(call: ScMethodCall): Option[Plan] =
    for {
      name    <- actionMethod(call)
      arg     <- call.args.exprs.headOption
      result  <- resultExpr(arg)
      tpe     <- result.`type`().toOption
      effect  <- effectOfResult(tpe, result)
      runtime <- runtimeFor(effect, call)
    } yield Plan(call, result, runtime, newInvoked(call, name))

  private val ActionBuilder = Set("play.api.mvc.ActionBuilder")

  private def isActionBuilder(e: ScExpression): Boolean =
    PlayPsi.safe {
      e.`type`().toOption.flatMap(t => t.extractClass(using Context(e))).exists(c => PlayPsi.isOwner(c, ActionBuilder))
    }.getOrElse(false)

  /**
   * "apply" / "async" when `call` is `Action { ... }` / `Action.async { ... }` / `Action(parser) { ... }` on a Play
   * `ActionBuilder`. Works on bodies that don't type-check yet (`Action { io }` is a type error before the fix).
   */
  private def actionMethod(call: ScMethodCall): Option[String] =
    PlayPsi.resolveMethod(call)
      .filter(m => (m.getName == "apply" || m.getName == "async") && Option(m.getContainingClass).exists(PlayPsi.isOwner(_, ActionBuilder)))
      .map(_.getName)
      .orElse(candidateActionMethod(call))
      .orElse(call.getInvokedExpr match {
        case ref: ScReferenceExpression if ref.refName == "async" && ref.qualifier.exists(isActionBuilder) => Some("async")
        case ref: ScReferenceExpression if ref.refName == "apply" && ref.qualifier.exists(isActionBuilder) => Some("apply")
        case invoked if isActionBuilder(invoked)                                                          => Some("apply")
        case _                                                                                             => None
      })

  /**
   * `Action { io }`: every `ActionBuilder.apply` overload is inapplicable (by-name `Result` vs `R[B] => Result`), so
   * nothing resolves and `Action` itself has no type. The inapplicable candidates still say which method it is.
   */
  private def candidateActionMethod(call: ScMethodCall): Option[String] =
    call.getInvokedExpr match {
      case ref: ScReferenceExpression =>
        val names = PlayPsi.safe(ref.multiResolveScala(false).toSeq.map(_.element)).getOrElse(Nil).collect {
          case m: PsiMethod if Option(m.getContainingClass).exists(PlayPsi.isOwner(_, ActionBuilder)) => m.getName
        }.distinct
        names match {
          case Seq(n @ ("apply" | "async")) => Some(n)
          case _                            => None
        }
      case _ => None
    }

  /** `{ implicit request => ...; io }` / `{ ...; io }` / `io` → the expression whose value is the effect. */
  private def resultExpr(arg: ScExpression): Option[ScExpression] = {
    def last(e: ScExpression): Option[ScExpression] = e match {
      case f: ScFunctionExpr => f.result.flatMap(last)
      case b: ScBlock =>
        b.exprs match {
          case Seq(f: ScFunctionExpr) => last(f)
          case _                      => b.resultExpression.flatMap(r => if (r == b) None else last(r))
        }
      case p: ScParenthesisedExpr => p.innerElement.flatMap(last)
      case other                  => Some(other)
    }
    last(arg)
  }

  /** `IO[Result]` → Some("IO"); `F[Result]` → Some("F"); anything else → None. */
  private def effectOfResult(tpe: ScType, place: PsiElement): Option[String] = {
    given Context = Context(place)
    def isResult(t: ScType): Boolean =
      t.extractClass.exists(c => PlayPsi.isOwner(c, Set("play.api.mvc.Result")))
    PlayPsi.safe(tpe.widen match {
      case ParameterizedType(des, Seq(arg)) if isResult(arg) =>
        des match {
          case tpt: TypeParameterType => Some(tpt.name)
          case other if other.extractClass.exists(c => PlayPsi.fqn(c) == "cats.effect.IO") => Some("IO")
          case _ => None
        }
      case _ => None
    }).flatten
  }

  /** Values visible from the action: enclosing class constructor params, members, enclosing method params. */
  private def visibleValues(place: PsiElement): Seq[(String, Boolean, Option[ScType])] = {
    val out = Seq.newBuilder[(String, Boolean, Option[ScType])]
    var current: PsiElement = place
    while (current != null && !current.isInstanceOf[ScalaFile]) {
      current match {
        case f: ScFunction =>
          f.parameters.foreach(p => out += ((p.name, p.isImplicit, p.`type`().toOption)))
        case td: ScTemplateDefinition =>
          td match {
            case c: ScClass =>
              c.constructor.toSeq.flatMap(_.parameters).foreach(p => out += ((p.name, p.isImplicit, p.`type`().toOption)))
            case _ =>
          }
          td.members.foreach {
            case v: ScValueOrVariable =>
              v.declaredElements.foreach(e => out += ((e.name, v.hasModifierProperty("implicit"), e.`type`().toOption)))
            case f: ScFunctionDefinition if f.parameters.isEmpty && f.hasModifierProperty("implicit") =>
              out += ((f.name, true, f.returnType.toOption))
            case _ =>
          }
        case _ =>
      }
      current = current.getContext
    }
    out.result()
  }

  private def classFqn(t: ScType, place: PsiElement): Option[String] = {
    given Context = Context(place)
    PlayPsi.safe(t.extractClass.map(PlayPsi.fqn)).flatten
  }

  private def runtimeFor(effect: String, call: ScMethodCall): Option[Runtime] = {
    val values = visibleValues(call)
    def dispatcherFor(e: String) = values.collectFirst {
      case (name, _, Some(t @ ParameterizedType(des, Seq(arg))))
          if classFqn(des, call).contains("cats.effect.std.Dispatcher") && arg.canonicalText.split('.').last == e =>
        Runtime.Dispatcher(name, e)
    }
    if (effect != "IO") dispatcherFor(effect)
    else {
      val runtimes = values.filter((_, _, t) => t.exists(classFqn(_, call).contains("cats.effect.unsafe.IORuntime")))
      runtimes.find(_._2).map(_ => Runtime.Implicit)
        .orElse(Option.when(globalImported(call))(Runtime.Implicit))
        .orElse(runtimes.headOption.map(r => Runtime.Explicit(r._1)))
        .orElse(dispatcherFor("IO"))
        .orElse(Some(Runtime.ImportGlobal))
    }
  }

  private def globalImported(place: PsiElement): Boolean =
    place.getContainingFile.getText.linesIterator.exists { l =>
      val t = l.trim
      t.startsWith("import cats.effect.unsafe.implicits.") && (t.endsWith("global") || t.endsWith("_") || t.endsWith("*"))
    }

  /** `Action` → `Action.async`, `Action(parse.json)` → `Action.async(parse.json)`; None when already `async`. */
  private def newInvoked(call: ScMethodCall, name: String): Option[String] =
    if (name == "async") None
    else
      call.getInvokedExpr match {
        case ref: ScReferenceExpression                      => Some(s"${ref.getText}.async")
        case inner: ScMethodCall =>
          inner.getInvokedExpr match {
            case ref: ScReferenceExpression if ref.refName != "async" => Some(s"${ref.getText}.async${inner.args.getText}")
            case _                                                    => None
          }
        case _ => None
      }
}
