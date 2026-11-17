package pounce.completion

import com.intellij.codeInsight.completion.{InsertHandler, InsertionContext, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder, LookupElementDecorator, LookupElementPresentation}
import com.intellij.icons.AllIcons
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.intellij.ui.JBColor
import pounce.core.Imports
import pounce.core.catalog.Op

/** Builds Pounce lookup elements and applies them (text, caret, imports). */
object LensLookup {

  def create(item: LensItem): LookupElement = {
    var b = LookupElementBuilder
      .create(item, item.lookupString)
      .withPresentableText(item.lookupString)
      .bold()
      .withIcon(AllIcons.Nodes.Method)
      .withTailText(item.paramsText, false)
      .appendTailText("  " + item.constraintsText, true)
      .withTypeText(item.typeText, true)
      .withInsertHandler(new LensInsertHandler(item))
    if (item.isUnknown) b = b.withItemTextForeground(JBColor.GRAY)
    PrioritizedLookupElement.withPriority(b, item.priority)
  }

  /**
   * Wraps a lookup element the Scala plugin produced for a curated member (`IO.blocking`, `io.attempt`): moves it up
   * and shows the catalog's one-line summary. Insertion stays the Scala plugin's.
   */
  def decorate(element: LookupElement, op: Op): LookupElement =
    PrioritizedLookupElement.withPriority(new CuratedLookupElement(element, op), 3.0 + op.rank / 100.0)

  final class CuratedLookupElement(delegate: LookupElement, val op: Op) extends LookupElementDecorator[LookupElement](delegate) {
    override def renderElement(presentation: LookupElementPresentation): Unit = {
      super.renderElement(presentation)
      presentation.setItemTextBold(true)
      val summary = op.doc.takeWhile(_ != '.')
      if (summary.nonEmpty && summary.length <= 60) presentation.appendTailText(s"  $summary", true)
    }
  }

  /**
   * Wraps the Scala plugin's element for cats syntax that is already imported (`ids.traverse` under
   * `import cats.syntax.all._`). Pounce does not add a duplicate item then, so this element carries Pounce's
   * presentation (compact parameters + ` Traverse[List] · cats`) and its quick documentation. Insertion stays the
   * Scala plugin's: nothing needs importing.
   */
  def explain(element: LookupElement, item: LensItem): LookupElement = new ExplainedLookupElement(element, item)

  final class ExplainedLookupElement(delegate: LookupElement, val item: LensItem) extends LookupElementDecorator[LookupElement](delegate) {
    override def renderElement(presentation: LookupElementPresentation): Unit = {
      super.renderElement(presentation)
      // The Scala plugin's tail is the full signature (`[G[_], B](f: Long => G[B])(implicit ev$1: Applicative[G])`),
      // which the popup truncates, so the explanation appended after it would never be visible.
      presentation.setTailText(item.paramsText, false)
      presentation.appendTailText("  " + item.constraintsText, true)
    }
  }

  /** The Pounce item behind a lookup element Pounce created, if it is one (not an explained Scala-plugin element). */
  def itemOf(element: LookupElement): Option[LensItem] =
    element.getObject match {
      case i: LensItem => Some(i)
      case _           => None
    }

  /** The Pounce item explaining a Scala-plugin element for already-imported cats syntax, if any. */
  def explainedOf(element: LookupElement): Option[LensItem] = decorators(element).collectFirst {
    case x: ExplainedLookupElement => x.item
  }

  /** The curated op behind a decorated Scala-plugin element, if any. */
  def curatedOf(element: LookupElement): Option[Op] = decorators(element).collectFirst {
    case c: CuratedLookupElement => c.op
  }

  /** The element and every element it decorates, outermost first. */
  private def decorators(element: LookupElement): Iterator[LookupElement] =
    Iterator.iterate(element) {
      case d: LookupElementDecorator[?] => d.getDelegate
      case _                            => null
    }.takeWhile(_ != null)

  /** Adds the imports an item needs. Must run in a write action with a committed document. */
  def addImports(item: LensItem, place: PsiElement): Unit = {
    if (item.syntaxImport)
      (item.op :: item.moreOps).filter(_.syntax.isDefined).foreach { op =>
        if (!Imports.isProvided(op, place)) Imports.ensureImportFor(op, place)
      }
    item.classImports.foreach(Owners.ensureImported(_, place))
  }
}

/** Replaces the inserted lookup string with the item's text, positions the caret, adds the imports. */
final class LensInsertHandler(item: LensItem) extends InsertHandler[LookupElement] {

  override def handleInsert(context: InsertionContext, element: LookupElement): Unit = {
    val document = context.getDocument
    val project  = context.getProject
    var start    = context.getStartOffset
    val tail     = context.getTailOffset
    var text     = item.insertText
    var caret    = item.caret
    if (item.infix) {
      // `fa.*>` → `fa *> |`: drop the dot (and whitespace) before the operator
      val chars = document.getCharsSequence
      var i     = start - 1
      while (i >= 0 && chars.charAt(i).isWhitespace) i -= 1
      if (i >= 0 && chars.charAt(i) == '.') {
        start = i
        text = " " + text
        caret += 1
      }
    }
    document.replaceString(start, tail, text)
    if (text.contains('(') && context.getCompletionChar == '(') context.setAddCompletionChar(false)
    val caretMarker = document.createRangeMarker(start + caret, start + caret)
    val psiDocs     = PsiDocumentManager.getInstance(project)
    if (item.syntaxImport || item.classImports.nonEmpty) {
      psiDocs.commitDocument(document)
      val place = context.getFile.findElementAt(math.min(start + 1, math.max(0, document.getTextLength - 1)))
      if (place != null) LensLookup.addImports(item, place)
      psiDocs.doPostponedOperationsAndUnblockDocument(document)
    }
    context.getEditor.getCaretModel.moveToOffset(caretMarker.getStartOffset)
    caretMarker.dispose()
  }
}
