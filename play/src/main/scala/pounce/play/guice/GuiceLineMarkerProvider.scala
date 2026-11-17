package pounce.play.guice

import com.intellij.codeInsight.daemon.{RelatedItemLineMarkerInfo, RelatedItemLineMarkerProvider}
import com.intellij.codeInsight.navigation.NavigationGutterIconBuilder
import com.intellij.icons.AllIcons
import com.intellij.openapi.util.NotNullLazyValue
import com.intellij.psi.{JavaPsiFacade, PsiElement}
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter

import java.util
import scala.jdk.CollectionConverters.*

/**
 * Spring-style wiring gutters for Guice:
 *  - on an `@Inject()` constructor parameter: jump to its binding(s) (`bind(...).to(...)`, `bind[A].to[B]`,
 *    `@Provides`, `@ImplementedBy`);
 *  - on a binding (`bind` of a bind chain, a `@Provides` method name): jump to every injection point of the bound
 *    type. Targets are computed lazily, when the gutter is clicked.
 */
final class GuiceLineMarkerProvider extends RelatedItemLineMarkerProvider {

  override def getName: String = "Guice bindings and injection points"

  override def getIcon: javax.swing.Icon = AllIcons.Nodes.Plugin

  override protected def collectNavigationMarkers(
    element: PsiElement,
    result: util.Collection[? >: RelatedItemLineMarkerInfo[?]]
  ): Unit =
    if (element.getNode != null && element.getNode.getElementType == ScalaTokenTypes.tIDENTIFIER && GuiceInjection.enabled(element)) {
      val project = element.getProject
      if (!GuiceInjection.isCompileTimeDi(project)) element.getParent match {
        case param: ScClassParameter if param.nameId == element =>
          GuiceInjection.injectionPoint(param).foreach { point =>
            val bindings = GuiceInjection.bindingsOf(point.injected)
            if (bindings.nonEmpty) {
              val tooltip = bindings.map(b => describe(b)).distinct.mkString("<br>")
              result.add(
                NavigationGutterIconBuilder
                  .create(AllIcons.Nodes.Plugin)
                  .setTargets(bindings.map(_.element).distinct.asJava)
                  .setTooltipText(s"<html>Guice binding for ${point.injected.getName}:<br>$tooltip</html>")
                  .setPopupTitle(s"Bindings of ${point.injected.getName}")
                  .createLineMarkerInfo(element)
              )
            }
          }

        case ref: ScReferenceExpression if ref.nameId == element && ref.refName == "bind" =>
          bindingAt(element).foreach(b => result.add(injectionPointsMarker(element, b)))

        case fn: ScFunctionDefinition if fn.nameId == element =>
          bindingAt(element).foreach(b => result.add(injectionPointsMarker(element, b)))

        case _ =>
      }
    }

  private def bindingAt(anchor: PsiElement): Option[GuiceBinding] =
    anchor.getContainingFile match {
      case f: ScalaFile => GuiceIndex.fileBindings(f).bindings.find(_.anchor == anchor)
      case _            => None
    }

  private def describe(b: GuiceBinding): String = {
    val target = b.targetFqn.filter(_ != b.boundFqn).map(t => s" → ${t.substring(t.lastIndexOf('.') + 1)}").getOrElse("")
    val where  = Option(b.element.getContainingFile).map(_.getName).getOrElse("")
    s"${b.kind.label}$target ($where)"
  }

  private def injectionPointsMarker(anchor: PsiElement, binding: GuiceBinding): RelatedItemLineMarkerInfo[PsiElement] = {
    val project = anchor.getProject
    val fqn     = binding.boundFqn
    val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]] { () =>
      Option(JavaPsiFacade.getInstance(project).findClass(fqn, GlobalSearchScope.allScope(project)))
        .toSeq
        .flatMap(GuiceInjection.injectionPointsOf)
        .map(p => p.param: PsiElement)
        .asJava
    }
    NavigationGutterIconBuilder
      .create(AllIcons.Gutter.ImplementedMethod)
      .setTargets(targets)
      .setTooltipText(s"Navigate to injection points of ${binding.boundName}")
      .setPopupTitle(s"Injection points of ${binding.boundName}")
      .setEmptyPopupText(s"No @Inject constructor parameter of type ${binding.boundName} found")
      .createLineMarkerInfo(anchor)
  }
}
