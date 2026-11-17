package pounce.visual.gutter

import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.codeInsight.navigation.NavigationGutterIconBuilder
import com.intellij.icons.AllIcons
import com.intellij.ide.util.PsiElementListCellRenderer
import com.intellij.openapi.util.{Computable, NotNullLazyValue}
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.{ClassInheritorsSearch, ReferencesSearch}
import com.intellij.psi.{PsiClass, PsiDocumentManager, PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructorInvocation
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScMethodCall, ScNewTemplateDefinition, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.*
import pounce.core.Libraries
import pounce.core.settings.PounceSettings
import pounce.visual.util.VisualPsi

import java.util
import scala.jdk.CollectionConverters.*

/**
 * D4 tagless wiring, like Spring's bean gutters:
 *  - on `trait UserRepo[F[_]]` (a tagless algebra): its implementations and the places they are constructed;
 *  - on `class DoobieUserRepo[F[_]: Sync] extends UserRepo[F]`: the algebra and the places it is constructed
 *    (`new DoobieUserRepo[IO](xa)`, `DoobieUserRepo[IO](xa)`, `DoobieUserRepo.make[IO](xa)`).
 *
 * Searches run only when the icon is clicked.
 */
object TaglessWiring {

  /** A trait / abstract class with an `F[_]` parameter and at least one method returning `F[...]`. */
  def isAlgebra(td: ScTypeDefinition): Boolean = td match {
    case _: ScTrait | (_: ScClass) if isAbstract(td) =>
      val hk = td.typeParameters.filter(_.typeParameters.nonEmpty).map(_.name).toSet
      hk.nonEmpty && td.functions.exists(f => f.returnTypeElement.exists(rt => hk.exists(n => rt.getText.startsWith(n + "["))))
    case _ => false
  }

  private def isAbstract(td: ScTypeDefinition): Boolean = td match {
    case _: ScTrait => true
    case c: ScClass => c.hasAbstractModifier
    case _          => false
  }

  /** Tagless algebras directly extended by `td`. */
  def algebrasOf(td: ScTemplateDefinition): Seq[ScTypeDefinition] =
    VisualPsi.safe(td.supers).toSeq.flatten.collect { case a: ScTypeDefinition if isAlgebra(a) => a }

  def implementations(algebra: PsiClass): Seq[PsiClass] = {
    val scope = GlobalSearchScope.projectScope(algebra.getProject)
    ClassInheritorsSearch.search(algebra, scope, true).findAll().asScala.toSeq.filter {
      case t: ScTrait => false
      case c: ScClass => !c.hasAbstractModifier
      case _          => true
    }
  }

  /**
   * Where `cls` is constructed: `new Cls(...)` (not `extends Cls`), plus calls on its companion (`Cls(...)`,
   * `Cls.make(...)`) outside the companion itself.
   */
  def constructionSites(cls: PsiClass): Seq[PsiElement] = cls match {
    case obj: ScObject => // an object interpreter is "wired" wherever it is referenced
      ReferencesSearch.search(obj, GlobalSearchScope.projectScope(obj.getProject)).findAll().asScala.toSeq
        .map(_.getElement)
        .filterNot(obj.isAncestorOf)
        .sortBy(e => (e.getContainingFile.getName, e.getTextRange.getStartOffset))
    case _ => classConstructionSites(cls)
  }

  private def classConstructionSites(cls: PsiClass): Seq[PsiElement] = {
    val scope = GlobalSearchScope.projectScope(cls.getProject)
    val viaNew =ReferencesSearch.search(cls, scope).findAll().asScala.toSeq.flatMap { ref =>
      val e = ref.getElement
      e.parentOfType[ScConstructorInvocation].filter { ci =>
        ci.parentOfType[ScTemplateDefinition].exists(_.isInstanceOf[ScNewTemplateDefinition]) &&
          ci.typeElement.getTextRange.contains(e.getTextRange)
      }.flatMap(_.parentOfType[ScNewTemplateDefinition])
    }
    val companion = cls match {
      case td: ScTypeDefinition => ScalaPsiUtil.getCompanionModule(td)
      case _                    => None
    }
    val viaCompanion = companion.toSeq.flatMap { obj =>
      ReferencesSearch.search(obj, scope).findAll().asScala.toSeq.flatMap { ref =>
        ref.getElement match {
          case r: ScReferenceExpression if !obj.isAncestorOf(r) =>
            r.getParent match {
              case m: ScMethodCall if m.getInvokedExpr == r                                    => Some(m) // Cls(...)
              case q: ScReferenceExpression if q.qualifier.contains(r) && isCall(q)             => q.getParent.asOptionOf[ScMethodCall].orElse(Some(q))
              case _                                                                           => None
            }
          case _ => None
        }
      }
    }
    (viaNew ++ viaCompanion).distinct.sortBy(e => (e.getContainingFile.getName, e.getTextRange.getStartOffset))
  }

  private def isCall(q: ScReferenceExpression): Boolean = q.getParent match {
    case m: ScMethodCall => m.getInvokedExpr == q
    case _               => false
  }

  /** "AppModule.scala:12  new DoobieUserRepo[IO](xa)" in the navigation popup. */
  final class SiteRenderer extends PsiElementListCellRenderer[PsiElement] {
    override def getElementText(element: PsiElement): String = element match {
      case c: PsiClass => c.getName
      case other       => VisualPsi.shorten(other.getText.replaceAll("\\s+", " "), 70)
    }

    override protected def getContainerText(element: PsiElement, name: String): String = {
      val file = element.getContainingFile
      val line = Option(PsiDocumentManager.getInstance(element.getProject).getDocument(file))
        .map(_.getLineNumber(element.getTextRange.getStartOffset) + 1)
      val member = element.parentOfType[ScMember].flatMap {
        case n: PsiNamedElement => Option(n.getName)
        case _                  => None
      }
      (member.map(_ + " in ").getOrElse("") + file.getName + line.map(":" + _).getOrElse("")).trim
    }
  }
}

final class TaglessWiringLineMarkerProvider extends LineMarkerProvider {

  import TaglessWiring.*

  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[? <: PsiElement] = {
    val node = element.getNode
    if (node == null || node.getElementType != ScalaTokenTypes.tIDENTIFIER || !PounceSettings().wiringGutters) return null
    element.getParent match {
      case td: ScTypeDefinition if td.nameId == element && Libraries.hasCats(element) =>
        if (isAlgebra(td)) algebraMarker(td, element)
        else
          td match {
            case _: ScClass | _: ScObject =>
              val algebras = algebrasOf(td)
              if (algebras.isEmpty) null else implementationMarker(td, algebras, element)
            case _ => null
          }
      case _ => null
    }
  }

  private def renderer: Computable[PsiElementListCellRenderer[?]] = () => new SiteRenderer

  private def algebraMarker(td: ScTypeDefinition, leaf: PsiElement): LineMarkerInfo[PsiElement] = {
    val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]] { () =>
      val impls = implementations(td)
      (impls ++ impls.flatMap(constructionSites)).asJava
    }
    NavigationGutterIconBuilder
      .create(AllIcons.Nodes.Plugin)
      .setTargets(targets)
      .setTooltipText(s"Tagless algebra ${td.name}: implementations and where they are constructed")
      .setPopupTitle(s"${td.name}: implementations and wiring")
      .setEmptyPopupText(s"No implementations of ${td.name} in the project")
      .setCellRenderer(renderer)
      .createLineMarkerInfo(leaf)
  }

  private def implementationMarker(td: ScTypeDefinition, algebras: Seq[ScTypeDefinition], leaf: PsiElement): LineMarkerInfo[PsiElement] = {
    val targets = NotNullLazyValue.`lazy`[util.Collection[? <: PsiElement]](() => (algebras ++ constructionSites(td)).asJava)
    val names   = algebras.map(_.name).mkString(", ")
    NavigationGutterIconBuilder
      .create(AllIcons.Nodes.Plugin)
      .setTargets(targets)
      .setTooltipText(s"${td.name} implements $names: navigate to the algebra and to where it is constructed")
      .setPopupTitle(s"${td.name}: algebra and wiring")
      .setCellRenderer(renderer)
      .createLineMarkerInfo(leaf)
  }
}
