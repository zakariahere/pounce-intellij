package pounce.inspections.tagless

import com.intellij.codeInspection.{LocalQuickFix, ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ImplicitArgumentsOwner
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt}
import pounce.core.types.ScTypeBridge
import pounce.core.{Bounds, Typeclasses}
import pounce.inspections.util.{BoundEditor, Typing}
import pounce.inspections.{EditFix, PounceInspection}

import scala.util.control.NonFatal

/**
 * B11. Explains a missing typeclass instance. The Scala plugin says "No implicit arguments of type: Monad[F]"; for a
 * tagless type parameter we say which bounds `F` has and offer "Add context bound F[_]: Monad" / "Strengthen bound
 * to Monad" on the declaring method or class. A few common concrete cases get a hint too (`IORuntime`,
 * `ExecutionContext`).
 */
final class MissingInstanceInspection extends PounceInspection {

  import MissingInstanceInspection.*

  override protected def visit(element: PsiElement, holder: ProblemsHolder, isOnTheFly: Boolean): Unit = element match {
    case e: (ScExpression & ImplicitArgumentsOwner) if !reportedByParent(e) =>
      missing(e).distinctBy(t => Typing.show(t, e)).foreach(t => explain(e, t, holder))
    case _ =>
  }

  private def missing(e: ScExpression & ImplicitArgumentsOwner): Seq[ScType] =
    try e.findImplicitArguments.flatMap(_.args).filter(_.isNotFoundImplicitParameter).flatMap(_.implicitSearchState.map(_.tp))
    catch { case NonFatal(_) => Nil }

  /** `foo[F](x)`: the plugin may attach the same implicit clause to `foo[F]` and to the call; report it once. */
  private def reportedByParent(e: ScExpression): Boolean = e.getParent match {
    case call: ScMethodCall if call.getInvokedExpr eq e => missing(call).nonEmpty
    case _                                              => false
  }

  private def explain(e: ScExpression, t: ScType, holder: ProblemsHolder): Unit = {
    val target = anchor(e)
    decompose(t, e) match {
      case Some((tc, f)) =>
        BoundEditor.typeParam(e, f) match {
          case Some(tp) =>
            val current = Bounds.forParam(e, f).toSeq.sorted
            val fixes   = boundFixes(tp, f, tc, current)
            holder.registerProblem(target, taglessMessage(tc, f, current), ProblemHighlightType.WEAK_WARNING, fixes*)
          case None =>
        }
      case None =>
        val shown = Typing.show(t, e)
        Typing.headFqn(t, e).flatMap(hints.get).foreach { case (hint, importPath) =>
          holder.registerProblem(target, concreteMessage(shown, hint), ProblemHighlightType.WEAK_WARNING, importPath.toSeq.map(importFix(e, _))*)
        }
    }
  }

  private def boundFixes(tp: ScTypeParam, f: String, tc: String, current: Seq[String]): Seq[LocalQuickFix] =
    if (current.isEmpty) Seq(new EditFix[ScTypeParam](addHint(f, tc), tp)((p, _) => BoundEditor.strengthen(p, tc)))
    else
      Typeclasses.strengtheningCandidates(BoundEditor.declared(tp).map(_.typeclass), tc).take(2).map { cand =>
        new EditFix[ScTypeParam](strengthenHint(cand), tp)((p, _) => BoundEditor.strengthen(p, cand))
      }

  /** `import cats.effect.unsafe.implicits.global` (a member import, not a wildcard). */
  private def importFix(e: ScExpression, path: String): LocalQuickFix =
    new EditFix[ScExpression](s"Import $path", e)((x, _) => BoundEditor.ensureMemberImport(path, x))

  /** Highlight the method name when there is one (like the Scala plugin), else the whole expression. */
  private def anchor(e: ScExpression): PsiElement = e match {
    case r: ScReferenceExpression => r.nameId
    case _                        => e
  }
}

object MissingInstanceInspection {

  /** Concrete implicit types we can say something useful about: FQN → (hint, member import to add). */
  private val hints: Map[String, (String, Option[String])] = Map(
    "cats.effect.unsafe.IORuntime" -> ("running an IO needs an IORuntime: extend IOApp, or import the global runtime at the edge of the world", Some("cats.effect.unsafe.implicits.global")),
    "scala.concurrent.ExecutionContext" -> ("inject an ExecutionContext (Play: @Inject()(implicit ec: ExecutionContext)) or import one", None)
  )

  def taglessMessage(tc: String, f: String, bounds: Seq[String]): String =
    if (bounds.isEmpty) s"No $tc[$f] instance: $f has no typeclass bounds here"
    else s"No $tc[$f] instance: $f is only bounded by ${bounds.mkString(", ")}, which doesn't provide $tc"

  def concreteMessage(tpe: String, hint: String): String = s"No implicit $tpe: $hint"

  def addHint(f: String, tc: String): String = s"Add context bound $f[_]: $tc"

  def strengthenHint(tc: String): String = s"Strengthen bound to $tc"

  /** `Monad[F]` with a known cats typeclass and a type parameter `F` → (Monad, F). */
  private def decompose(t: ScType, place: PsiElement): Option[(String, String)] =
    try {
      given Context = Context(place)
      t.removeAliasDefinitions() match {
        case ParameterizedType(designator, args) =>
          args.lastOption match {
            case Some(tpt: TypeParameterType) =>
              val (name, _) = ScTypeBridge.nameAndFqn(designator)
              Typeclasses.canonical(name).map(_ -> tpt.name)
            case _ => None
          }
        case _ => None
      }
    } catch { case NonFatal(_) => None }
}
