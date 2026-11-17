package pounce.visual.gutter

import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.codeInsight.navigation.NavigationGutterIconBuilder
import com.intellij.icons.AllIcons
import com.intellij.openapi.editor.markup.GutterIconRenderer
import com.intellij.openapi.util.NotNullLazyValue
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import pounce.core.Libraries
import pounce.core.settings.PounceSettings
import pounce.visual.util.VisualPsi

import java.util
import javax.swing.Icon
import scala.jdk.CollectionConverters.*

/**
 * D4 effect gutters, on the identifier of the call (leaf elements only, as the platform recommends):
 *  - fork points: `start`, `background`, `parTraverse*`, `parSequence*`, `parMapN`, `parTupled`, `race`, `both`,
 *    `Supervisor#supervise`, fs2 `parEvalMap*` / `concurrently` / `parJoin*` → "Runs on a new fiber";
 *  - blocking regions: `IO.blocking`, `IO.interruptible(Many)`, `Sync[F].blocking` → "Runs on the blocking pool";
 *  - resources: `Resource.make(acquire)(release)` / `makeCase` / `makeFull` / `fromAutoCloseable` and `.use`, with
 *    navigation between acquisition and release (targets computed lazily, on click).
 *
 * Only calls that resolve into cats / cats-effect / fs2 are marked.
 */
final class EffectLineMarkerProvider extends LineMarkerProvider {

  import EffectLineMarkerProvider.*

  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[? <: PsiElement] = {
    val node = element.getNode
    if (node == null || node.getElementType != ScalaTokenTypes.tIDENTIFIER) return null
    val name = element.getText
    val category =
      if (ForkNames(name)) Some(Category.Fork)
      else if (BlockingNames(name)) Some(Category.Blocking)
      else if (ResourceNames(name)) Some(Category.Resource)
      else None
    category.filter(enabled).flatMap { c =>
      element.getParent match {
        case ref: ScReferenceExpression if ref.nameId == element && Libraries.hasCats(element) => marker(c, ref, element)
        case _                                                                               => None
      }
    }.orNull
  }

  private def enabled(c: Category): Boolean = {
    val s = PounceSettings()
    c match {
      case Category.Fork     => s.forkGutters
      case Category.Blocking => s.blockingGutters
      case Category.Resource => s.resourceGutters
    }
  }
}

object EffectLineMarkerProvider {

  enum Category {
    case Fork, Blocking, Resource
  }

  val ForkNames: Set[String] = Set(
    "start", "background", "parTraverse", "parTraverse_", "parTraverseN", "parTraverseN_", "parSequence", "parSequence_",
    "parSequenceN", "parMapN", "parTupled", "parFlatTraverse", "parReplicateA", "race", "racePair", "raceOutcome", "both",
    "bothOutcome", "supervise", "parEvalMap", "parEvalMapUnordered", "concurrently", "parJoin", "parJoinUnbounded"
  )

  val BlockingNames: Set[String] = Set("blocking", "interruptible", "interruptibleMany")

  val ResourceNames: Set[String] = Set("make", "makeCase", "makeFull", "fromAutoCloseable", "use")

  val ForkTooltip     = "Runs on a new fiber"
  val BlockingTooltip = "Runs on the blocking thread pool"

  def marker(c: Category, ref: ScReferenceExpression, leaf: PsiElement): Option[LineMarkerInfo[? <: PsiElement]] = {
    val owner = VisualPsi.resolvedOwnerFqn(ref).getOrElse("")
    if (!VisualPsi.isTypelevel(owner)) return None
    val name = ref.refName
    c match {
      case Category.Fork =>
        Some(simple(leaf, AllIcons.Vcs.Branch, s"$ForkTooltip (.$name)"))
      case Category.Blocking =>
        Some(simple(leaf, AllIcons.Debugger.ThreadFrozen, s"$BlockingTooltip (.$name)"))
      case Category.Resource if owner.contains("Resource") => resourceMarker(ref, leaf)
      case Category.Resource                               => None
    }
  }

  private def simple(leaf: PsiElement, icon: Icon, tooltip: String): LineMarkerInfo[PsiElement] =
    new LineMarkerInfo[PsiElement](
      leaf,
      leaf.getTextRange,
      icon,
      (_: PsiElement) => tooltip,
      null,
      GutterIconRenderer.Alignment.LEFT,
      () => tooltip
    )

  /** Argument lists applied after `ref`, outermost last: `Resource.make(acq)(rel)` → Seq(Seq(acq), Seq(rel)). */
  def argumentLists(ref: ScReferenceExpression): Seq[Seq[ScExpression]] = {
    val lists = Seq.newBuilder[Seq[ScExpression]]
    var current: PsiElement = ref
    var parent              = ref.getParent
    var continue            = true
    while (continue && parent != null) {
      parent match {
        case g: ScGenericCall if g.referencedExpr == current => current = g; parent = g.getParent
        case m: ScMethodCall if m.getInvokedExpr == current  => lists += m.argumentExpressions; current = m; parent = m.getParent
        case _                                               => continue = false
      }
    }
    lists.result()
  }

  final case class AcquireRelease(acquire: Option[ScExpression], release: Option[ScExpression])

  /** Acquire / release of a `Resource.make*` or `fromAutoCloseable` call whose method name is `ref`. */
  def acquireRelease(ref: ScReferenceExpression): Option[AcquireRelease] = {
    val lists = argumentLists(ref)
    ref.refName match {
      case "make" | "makeCase" | "makeFull" =>
        Some(AcquireRelease(lists.headOption.flatMap(_.headOption), lists.lift(1).flatMap(_.headOption)))
      case "fromAutoCloseable" => Some(AcquireRelease(lists.headOption.flatMap(_.headOption), None))
      case _                   => None
    }
  }

  /** For `res.use(...)`: the `Resource.make*` call that `res` is defined by (a val / def in the project). */
  def definitionOfUsed(use: ScReferenceExpression): Option[ScReferenceExpression] = {
    def makeIn(e: PsiElement): Option[ScReferenceExpression] =
      e.depthFirst()
        .collect { case r: ScReferenceExpression if Set("make", "makeCase", "makeFull", "fromAutoCloseable")(r.refName) => r }
        .nextOption()
    use.qualifier.flatMap {
      case q: ScReferenceExpression =>
        VisualPsi.safe(q.resolve()).flatMap {
          case p: ScReferencePattern =>
            p.nameContext match {
              case d: ScPatternDefinition => d.expr.flatMap(makeIn)
              case _                      => None
            }
          case f: ScFunctionDefinition => f.body.flatMap(makeIn)
          case _                       => None
        }
      case call: ScExpression => makeIn(call)
    }
  }

  private def resourceMarker(ref: ScReferenceExpression, leaf: PsiElement): Option[LineMarkerInfo[? <: PsiElement]] =
    ref.refName match {
      case "use" =>
        val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]](() =>
          definitionOfUsed(ref).flatMap(acquireRelease).toSeq.flatMap(ar => ar.acquire.toSeq ++ ar.release).asJava
        )
        Some(
          NavigationGutterIconBuilder
            .create(AllIcons.Nodes.Related)
            .setTargets(targets)
            .setTooltipText("Resource is acquired here and released when use completes")
            .setPopupTitle("Acquire / Release")
            .setEmptyPopupText("The resource is not defined by Resource.make in this project")
            .createLineMarkerInfo(leaf)
        )
      case "fromAutoCloseable" =>
        val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]](() =>
          acquireRelease(ref).flatMap(_.acquire).toSeq.asJava
        )
        Some(
          NavigationGutterIconBuilder
            .create(AllIcons.Nodes.Related)
            .setTargets(targets)
            .setTooltipText("Resource: acquired by the given effect, released with close()")
            .setPopupTitle("Acquire")
            .createLineMarkerInfo(leaf)
        )
      case _ =>
        val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]](() =>
          acquireRelease(ref).toSeq.flatMap(ar => ar.acquire.toSeq ++ ar.release).asJava
        )
        Some(
          NavigationGutterIconBuilder
            .create(AllIcons.Nodes.Related)
            .setTargets(targets)
            .setTooltipText("Resource: navigate between acquire and release")
            .setPopupTitle("Acquire / Release")
            .createLineMarkerInfo(leaf)
        )
    }
}
