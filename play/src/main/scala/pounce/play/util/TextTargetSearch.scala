package pounce.play.util

import com.intellij.find.findUsages.{FindUsagesHandler, FindUsagesHandlerFactory}
import com.intellij.openapi.application.QueryExecutorBase
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.search.{PsiSearchHelper, SearchScope, UsageSearchContext}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiReference}
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral

import scala.collection.mutable

/** Finds the Scala string literals whose (config / message key) references point at a [[TextTarget]]. */
object TextTargetUsages {

  /** The most selective identifier-like word of a key (the IdIndex splits `app.db.url` into `app`, `db`, `url`). */
  def searchWord(key: String): Option[String] =
    key.split("[^A-Za-z0-9_]+").filter(_.nonEmpty).maxByOption(_.length)

  def process(target: TextTarget, scope: SearchScope, consumer: PsiReference => Boolean): Boolean =
    searchWord(target.name) match {
      case None => true
      case Some(word) =>
        val seen = mutable.HashSet.empty[ScStringLiteral]
        PsiSearchHelper
          .getInstance(target.getProject)
          .processElementsWithWord(
            (element: PsiElement, _: Int) =>
              Option(PsiTreeUtil.getParentOfType(element, classOf[ScStringLiteral], false)) match {
                case Some(lit) if seen.add(lit) =>
                  lit.getReferences.iterator.filter(_.isReferenceTo(target)).forall(consumer)
                case _ => true
              },
            scope,
            word,
            UsageSearchContext.IN_STRINGS,
            true
          )
    }

  def literals(target: TextTarget, scope: SearchScope): Seq[PsiElement] = {
    val out = Seq.newBuilder[PsiElement]
    process(target, scope, ref => { out += ref.getElement; true })
    out.result().distinct
  }
}

/** Find Usages / ReferencesSearch for config and message keys. */
final class TextTargetReferencesSearcher
    extends QueryExecutorBase[PsiReference, ReferencesSearch.SearchParameters](true) {
  override def processQuery(params: ReferencesSearch.SearchParameters, consumer: Processor[? >: PsiReference]): Unit =
    params.getElementToSearch match {
      case t: TextTarget => TextTargetUsages.process(t, params.getEffectiveSearchScope, r => consumer.process(r))
      case _             =>
    }
}

final class TextTargetFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  override def canFindUsages(element: PsiElement): Boolean = element.isInstanceOf[TextTarget]
  override def createFindUsagesHandler(element: PsiElement, forHighlightUsages: Boolean): FindUsagesHandler =
    new FindUsagesHandler(element) {}
}
