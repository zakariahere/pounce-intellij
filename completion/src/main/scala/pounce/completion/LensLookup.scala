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

  /** The Pounce item behind a lookup element (through any decorators), if it is one. */
  def itemOf(element: LookupElement): Option[LensItem] =
    element.getObject match {
      case i: LensItem => Some(i)
      case _           => None
    }

  /** The curated op behind a decorated Scala-plugin element, if any. */
  def curatedOf(element: LookupElement): Option[Op] = {
    var e: LookupElement = element
    while (e != null) {
      e match {
        case c: CuratedLookupElement => return Some(c.op)
        case d: LookupElementDecorator[?] => e = d.getDelegate
        case _ => e = null
      }
    }
    None
  }

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
