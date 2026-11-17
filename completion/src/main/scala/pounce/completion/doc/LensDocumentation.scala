package pounce.completion.doc

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.lang.documentation.DocumentationMarkup.*
import com.intellij.model.Pointer
import com.intellij.openapi.util.text.StringUtil
import com.intellij.platform.backend.documentation.{DocumentationResult, DocumentationTarget, LookupElementDocumentationTargetProvider}
import com.intellij.platform.backend.presentation.TargetPresentation
import com.intellij.psi.PsiFile
import pounce.completion.{ItemSource, LensItem, LensLookup}
import pounce.core.Imports
import pounce.core.catalog.{Op, OpKind}

import scala.util.control.NonFatal

/**
 * A7: quick documentation (Ctrl+Q / the lookup's doc popup) for Pounce lookup items and for the Scala plugin items
 * Pounce curated: signature, constraints, the catalog doc, example, see-also, the import that will be added, and
 * "why it is offered here" (which instance was found for which type, which bound in scope gave it).
 */
final class LensDocumentation extends LookupElementDocumentationTargetProvider {

  override def documentationTarget(file: PsiFile, element: LookupElement, offset: Int): DocumentationTarget =
    try {
      LensLookup.itemOf(element) match {
        case Some(item) => new LensDocTarget(item.lookupString, LensDocumentation.html(item, importNote(item, file, offset)))
        case None =>
          LensLookup.curatedOf(element) match {
            case Some(op) => new LensDocTarget(op.name, LensDocumentation.html(op))
            case None     => null
          }
      }
    } catch {
      case e: com.intellij.openapi.progress.ProcessCanceledException => throw e
      case NonFatal(_)                                               => null
    }

  /** `import cats.syntax.all._ (added on insert)`, or `None` when nothing needs importing. */
  private def importNote(item: LensItem, file: PsiFile, offset: Int): Option[String] =
    if (!item.syntaxImport) None
    else
      Option(file.findElementAt(math.max(0, offset - 1))).flatMap { place =>
        val missing = (item.op :: item.moreOps).filter(op => op.syntax.isDefined && !Imports.isProvided(op, place))
        if (missing.isEmpty) None
        else {
          val style = Imports.effectiveStyle(place)
          val roots = missing.flatMap(_.syntax).map(Imports.rootFor(_, style)).distinct
          Some(roots.map(Imports.importText(_, place)).map("import " + _).mkString(", "))
        }
      }
}

/** Plain data (no PSI), so a hard pointer is fine. */
final class LensDocTarget(title: String, val html: String) extends DocumentationTarget {

  override def createPointer(): Pointer[? <: DocumentationTarget] = Pointer.hardPointer(this)

  override def computePresentation(): TargetPresentation = TargetPresentation.builder(title).presentation()

  override def computeDocumentation(): DocumentationResult = DocumentationResult.documentation(html)
}

object LensDocumentation {

  private def esc(s: String): String = StringUtil.escapeXmlEntities(s)

  private def section(title: String, body: String): String =
    s"$SECTION_HEADER_START${esc(title)}$SECTION_SEPARATOR$body$SECTION_END"

  /** Documentation of a Pounce lookup item, with the "why here" lines and the import note. */
  def html(item: LensItem, importNote: Option[String] = None): String = {
    val op = item.op
    val here = item.source match {
      case ItemSource.Smart     => s"${item.insertText}: ${item.typeText}"
      case ItemSource.Companion => s"${item.receiverText}.${op.name}${item.paramsText}: ${item.typeText}"
      case ItemSource.Extension => s"${item.receiverText}.${op.name}${item.paramsText}: ${item.typeText}"
    }
    val instances = op.requires.map { case (v, tc) =>
      item.fit.map(_.subst).getOrElse(item.subst).get(v).map(t => s"$tc[${t.show}]").getOrElse(s"$tc[$v]")
    }
    val sections = List(
      Some(section("Here", s"<code>${esc(here)}</code>")),
      Option.when(instances.nonEmpty)(section("Requires", esc(instances.mkString(", ")))),
      Option.when(item.why.nonEmpty)(section("Why here", item.why.map(esc).mkString("<br/>"))),
      Option.when(item.isUnknown)(section("Note", "Pounce could not verify the instances: the call may not compile.")),
      importNote.map(n => section("Import", s"<code>${esc(n)}</code> (added on insert)"))
    ).flatten
    page(op, sections)
  }

  /** Documentation of a curated member offered by the Scala plugin (`IO.blocking`, `io.attempt`). */
  def html(op: Op): String = page(op, Nil)

  private def page(op: Op, extra: List[String]): String = {
    val definition = s"$DEFINITION_START${esc(op.signature)}$DEFINITION_END"
    val content    = if (op.doc.isEmpty) "" else s"$CONTENT_START${esc(op.doc)}$CONTENT_END"
    val common = List(
      Option.when(op.requires.nonEmpty)(section("Constraints", esc(op.constraintsText))),
      Option.when(op.example.nonEmpty)(section("Example", s"<pre><code>${esc(op.example)}</code></pre>")),
      Option.when(op.seeAlso.nonEmpty)(section("See also", esc(op.seeAlso.mkString(", ")))),
      Some(section("Library", esc(op.library.displayName + op.syntax.map(s => s" · syntax: $s").getOrElse("") + kindNote(op))))
    ).flatten
    definition + content + SECTIONS_START + (extra ++ common).mkString + SECTIONS_END
  }

  private def kindNote(op: Op): String = op.kind match {
    case OpKind.Lift      => " · lift"
    case OpKind.Tuple     => " · tuple syntax"
    case OpKind.Companion => " · constructor"
    case OpKind.Method    => " · method"
    case OpKind.Syntax    => ""
  }
}
