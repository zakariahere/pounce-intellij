package pounce.play.i18n

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.LookupElementBuilder
import com.intellij.icons.AllIcons
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.*
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.play.config.ConfKeyCompletionContributor
import pounce.play.util.{PlayPsi, TextTarget}

/** Ctrl+click on a message key → its line in `conf/messages` (popup with every language file defining it). */
final class MessageKeyReferenceContributor extends PsiReferenceContributor {
  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit =
    registrar.registerReferenceProvider(
      PlatformPatterns.psiElement(classOf[ScStringLiteral]),
      new PsiReferenceProvider {
        override def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] =
          PlayPsi.plainLiteral(element) match {
            case Some(lit) if lit.getValue.nonEmpty && MessagesCalls.enabled(lit) && MessagesCalls.use(lit).isDefined =>
              Array(new MessageKeyReference(lit))
            case _ => PsiReference.EMPTY_ARRAY
          }
      }
    )
}

final class MessageKeyReference(lit: ScStringLiteral)
    extends PsiPolyVariantReferenceBase[ScStringLiteral](lit, PlayPsi.contentRange(lit), true) {

  def key: String = lit.getValue

  def targets: Seq[TextTarget] =
    MessagesIndex.snapshot(lit.getProject).definitions(key).map((f, e) => MessageKeyReference.target(lit.getProject, f, e))

  override def multiResolve(incompleteCode: Boolean): Array[ResolveResult] =
    targets.map(t => new PsiElementResolveResult(t): ResolveResult).toArray

  /** The default `conf/messages` entry (what users see when no translation applies). */
  override def resolve(): PsiElement = targets.headOption.orNull

  override def isReferenceTo(element: PsiElement): Boolean = element match {
    case t: TextTarget => t.kind == MessageKeyReference.Kind && t.name == key
    case _             => false
  }

  override def handleElementRename(newElementName: String): PsiElement = lit
}

object MessageKeyReference {
  val Kind = "message key"

  def target(project: com.intellij.openapi.project.Project, file: MessagesFile, entry: MessageEntry): TextTarget =
    new TextTarget(project, file.file, entry.line, entry.key, entry.key, Kind, entry.value)
}

/** Completes message keys inside `messages("…")`, `Messages("…")`, `messagesApi("…")`, ... */
final class MessageKeyCompletionContributor extends CompletionContributor {
  override def fillCompletionVariants(parameters: CompletionParameters, result: CompletionResultSet): Unit =
    ConfKeyCompletionContributor.literalAt(parameters.getPosition).foreach { lit =>
      if (MessagesCalls.enabled(lit) && MessagesCalls.use(lit).isDefined) {
        val typed    = ConfKeyCompletionContributor.typedPrefix(lit, parameters.getOffset)
        val snapshot = MessagesIndex.snapshot(parameters.getOriginalFile.getProject)
        val rs       = result.withPrefixMatcher(typed)
        val seen     = scala.collection.mutable.HashSet.empty[String]
        for {
          file  <- snapshot.all
          entry <- file.entries
          if seen.add(entry.key)
        } {
          val element = LookupElementBuilder
            .create(entry.key)
            .withTailText(s" = ${if (entry.value.length > 50) entry.value.take(47) + "…" else entry.value}", true)
            .withTypeText(file.label, true)
            .withIcon(AllIcons.Nodes.Property)
          rs.addElement(PrioritizedLookupElement.withPriority(element, if (file.library) 0.0 else 10.0))
        }
      }
    }
}
