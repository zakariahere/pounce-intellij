package pounce.play.config

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.{LookupElementBuilder, LookupElement}
import com.intellij.icons.AllIcons
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.play.util.PlayPsi

/**
 * Completes config paths inside `config.get[T]("…")`, `getOptional`, `has`, `underlying.getString("…")`,
 * `ConfigFactory.load().getX("…")`, `ConfigSource.default.at("…")`. Keys come from the project's conf files and
 * every library `reference.conf`; the lookup shows the value and where it is defined.
 */
final class ConfKeyCompletionContributor extends CompletionContributor {

  override def fillCompletionVariants(parameters: CompletionParameters, result: CompletionResultSet): Unit =
    ConfKeyCompletionContributor.literalAt(parameters.getPosition).foreach { lit =>
      if (ConfCalls.enabled(lit)) {
        ConfCalls.use(lit).foreach { use =>
          val typed    = ConfKeyCompletionContributor.typedPrefix(lit, parameters.getOffset)
          val snapshot = ConfIndex.snapshot(parameters.getOriginalFile.getProject)
          val base = use.prefix match {
            case ConfPrefix.Known(p) => Some(p + ".")
            case _                   => None
          }
          val rs = result.withPrefixMatcher(typed)
          snapshot.allEntries.foreach { entry =>
            val relative = base match {
              case Some(b) => if (entry.path.startsWith(b)) Some(entry.path.substring(b.length)) else None
              case None    => Some(entry.path)
            }
            relative.foreach(r => rs.addElement(ConfKeyCompletionContributor.lookup(r, entry)))
          }
        }
      }
    }
}

object ConfKeyCompletionContributor {

  def literalAt(position: PsiElement): Option[ScStringLiteral] =
    Option(position).flatMap(p => PlayPsi.plainLiteral(p).orElse(Option(p.getParent).flatMap(PlayPsi.plainLiteral)))

  /** Literal content between the opening quote and the caret. */
  def typedPrefix(lit: ScStringLiteral, caret: Int): String = {
    val content = lit.contentRange
    val end     = math.max(content.getStartOffset, math.min(caret, content.getEndOffset))
    lit.getContainingFile.getText.substring(content.getStartOffset, end)
  }

  def lookup(text: String, entry: ConfEntry): LookupElement = {
    val tail = entry.valueType match {
      case "object" => ""
      case _        => s" = ${entry.value}"
    }
    val element = LookupElementBuilder
      .create(entry, text)
      .withPresentableText(text)
      .withTailText(tail, true)
      .withTypeText(entry.fileLabel, true)
      .withIcon(if (entry.valueType == "object") AllIcons.Nodes.Folder else AllIcons.Nodes.Property)
    PrioritizedLookupElement.withPriority(element, if (entry.library) 0.0 else 10.0)
  }
}
