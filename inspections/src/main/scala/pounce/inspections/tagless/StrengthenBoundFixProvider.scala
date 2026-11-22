package pounce.inspections.tagless

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiFile, SmartPointerManager, SmartPsiElementPointer}
import org.jetbrains.plugins.scala.annotator.UnresolvedReferenceFixProvider
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScInfixExpr, ScPostfixExpr, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.types.Ty
import pounce.core.{Bounds, Imports, InstanceOracle, Libraries, Typeclasses}
import pounce.inspections.util.{BoundEditor, Typing}

import scala.util.control.NonFatal

/**
 * B8 (b). `fa.flatMap(...)` on a tagless `F[A]` whose bounds are too weak (`F[_]: Functor`): the Scala plugin reports
 * "Cannot resolve symbol flatMap" because the syntax conversion needs `FlatMap[F]`. We add quick fixes to that error:
 * "Strengthen bound to Monad" (editing the context bound / implicit parameter of the declaring method or class), and
 * "Import cats.syntax.all._" when the bound is fine but the syntax import is missing.
 *
 * Implemented as an `UnresolvedReferenceFixProvider` (Scala plugin extension point) rather than a separate
 * inspection, so we add the fix to the plugin's own error instead of a second, competing highlight.
 */
final class StrengthenBoundFixProvider extends UnresolvedReferenceFixProvider {

  import StrengthenBoundFixProvider.*

  override def fixesFor(reference: ScReference): Seq[IntentionAction] = reference match {
    case ref: ScReferenceExpression =>
      receiverOf(ref) match {
        case Some(qual) =>
          try fixes(ref, qual)
          catch { case NonFatal(_) => Nil }
        case None => Nil
      }
    case _ => Nil
  }

  private def fixes(ref: ScReferenceExpression, qual: ScExpression): Seq[IntentionAction] = {
    if (!Libraries.hasCats(ref)) return Nil
    val tagless = taglessFixes(ref, qual)
    if (tagless.nonEmpty) tagless else properFixes(ref, qual)
  }

  /**
   * `a compare b`, `a > b`, `a === b` on a plain value whose type has the instance (`Order[Contract]` in its companion,
   * `Eq[String]`, ...) but no syntax import: offer the import. Only when the instance is known to exist, so the fix
   * really makes the error go away. Lifts that need no instance (`"empty".invalidNel`, `().validNel`) always qualify.
   */
  private def properFixes(ref: ScReferenceExpression, qual: ScExpression): Seq[IntentionAction] = {
    val candidates = properOps(ref.refName)
    if (candidates.isEmpty) return Nil
    val oracle = InstanceOracle(ref)
    val result = for {
      t        <- Typing.typeOf(qual)
      ty       <- Typing.ty(t, ref)
      (op, _)  <- candidates.find { case (_, tc) => tc.forall(oracle.has(_, ty) == Answer.Yes) }
      s        <- op.syntax
      if !Imports.providers(s).exists(Imports.isRootInScope(_, ref))
    } yield new AddSyntaxImportAction(ref, Typing.importRootFor(s, ref))
    result.toSeq
  }

  private def taglessFixes(ref: ScReferenceExpression, qual: ScExpression): Seq[IntentionAction] = {
    val result = for {
      t        <- Typing.typeOf(qual).toSeq
      f        <- Typing.rigidHead(t).toSeq
      required <- requiredTypeclass(ref.refName).toSeq
      tp       <- BoundEditor.typeParam(ref, f).toSeq
    } yield {
      val current = Bounds.forParam(ref, f)
      if (!Typeclasses.satisfies(current, required)) {
        val declaredNames = BoundEditor.declared(tp).map(_.typeclass)
        Typeclasses.strengtheningCandidates(declaredNames, required).take(2).map(tc => new StrengthenBoundAction(tp, tc, f, current.nonEmpty))
      } else {
        syntaxOf(ref.refName).filterNot(s => Imports.providers(s).exists(Imports.isRootInScope(_, ref))).map { s =>
          new AddSyntaxImportAction(ref, Typing.importRootFor(s, ref))
        }.toSeq
      }
    }
    result.flatten
  }
}

object StrengthenBoundFixProvider {

  private lazy val syntaxOps = Catalog.bundled.ops.filter(op => op.kind == OpKind.Syntax)

  /** Extension methods on a value: syntax ops plus lifts (`.invalidNel`, `.asRight`, `.some`), all needing an import. */
  private lazy val receiverOps =
    Catalog.bundled.ops.filter(op => (op.kind == OpKind.Syntax || op.kind == OpKind.Lift) && op.syntax.isDefined)

  /**
   * The value an operation is called on: `a` in `a.compare(b)`, and also in `a compare b` / `a > b` (an infix
   * operation reference has no `qualifier`) and `a pmin` (postfix).
   */
  def receiverOf(ref: ScReferenceExpression): Option[ScExpression] =
    ref.qualifier.orElse(ref.getParent match {
      case i: ScInfixExpr if i.operation eq ref   => Some(i.left)
      case p: ScPostfixExpr if p.operation eq ref => Some(p.operand)
      case _                                      => None
    })

  /**
   * Syntax ops on a proper receiver `A` with the typeclass `A` needs, weakest first: `compare` (Order), `>`
   * (PartialOrder), `===` (Eq). `None` for ops that work on any value (`invalidNel`, `asRight`, `some`); those come
   * first, since nothing has to be checked for them.
   */
  def properOps(method: String): Seq[(Op, Option[String])] =
    receiverOps
      .filter(_.name == method)
      .flatMap { op =>
        op.receiver.collect { case Ty.Var(v) => v }.map(v => op -> op.requires.collectFirst { case (`v`, tc) => tc })
      }
      .sortBy { case (_, tc) => tc.fold(-1)(Typeclasses.depth) }

  /** The typeclass a syntax method needs for its receiver `F[A]` (`flatMap` → FlatMap), weakest first. */
  def requiredTypeclass(method: String): Option[String] =
    syntaxOps
      .filter(_.name == method)
      .flatMap { op =>
        val recvVar = op.receiver.collect {
          case Ty.App(fn, _)                   => fn.name
          case Ty.Con(name, _ :: _, _, _)      => name
        }
        recvVar.flatMap(v => op.requires.collectFirst { case (`v`, tc) => tc })
      }
      .sortBy(Typeclasses.depth)
      .headOption

  def syntaxOf(method: String): Option[String] = syntaxOps.find(_.name == method).flatMap(_.syntax)

  def strengthenText(tc: String): String = s"Strengthen bound to $tc"

  final class StrengthenBoundAction(tp: ScTypeParam, tc: String, typeParam: String, hasBounds: Boolean) extends BaseIntentionAction {
    private val pointer: SmartPsiElementPointer[ScTypeParam] = SmartPointerManager.createPointer(tp)

    setText(if (hasBounds) strengthenText(tc) else s"Add context bound $typeParam[_]: $tc")

    override def getFamilyName: String = "Pounce: strengthen typeclass bound"

    override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = pointer.getElement != null

    override def invoke(project: Project, editor: Editor, file: PsiFile): Unit =
      Option(pointer.getElement).foreach(BoundEditor.strengthen(_, tc))
  }

  final class AddSyntaxImportAction(ref: ScReferenceExpression, root: String) extends BaseIntentionAction {
    private val pointer = SmartPointerManager.createPointer(ref)

    setText(s"Import ${Imports.importText(root, ref)}")

    override def getFamilyName: String = "Pounce: import cats syntax"

    override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = pointer.getElement != null

    override def invoke(project: Project, editor: Editor, file: PsiFile): Unit =
      Option(pointer.getElement).foreach(r => Imports.ensureWildcardImport(root, r))
  }
}
