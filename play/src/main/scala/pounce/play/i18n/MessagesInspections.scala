package pounce.play.i18n

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.codeInspection.*
import com.intellij.openapi.fileTypes.PlainTextFileType
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElement, PsiElementVisitor, PsiFile, PsiManager}
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.play.util.{PlayPsi, TextFiles}

private object MessagesInspectionSupport {
  def visitor(check: (ScStringLiteral, MessageKeyUse) => Unit): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = element match {
        case lit: ScStringLiteral =>
          PlayPsi.plainLiteral(lit).filter(_.getValue.nonEmpty).foreach { _ =>
            if (MessagesCalls.enabled(lit)) MessagesCalls.use(lit).filterNot(_.optional).foreach(check(lit, _))
          }
        case _ =>
      }
    }
}

/**
 * "Missing message key": `messages("k")` where `k` is not in the default `conf/messages` (nor in a library's
 * `messages.default`, which is where Play's own `error.required` etc. live). Projects without a default messages file
 * are checked against all their `messages.xx` files. Projects without any messages file are not checked.
 */
final class MissingMessageKeyInspection extends LocalInspectionTool {
  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    MessagesInspectionSupport.visitor { (lit, use) =>
      val snapshot = MessagesIndex.snapshot(lit.getProject)
      if (snapshot.hasProjectFiles) {
        val key = use.key
        val defaults = snapshot.defaults
        val missing =
          if (defaults.nonEmpty) !snapshot.definedByDefault(key)
          else !snapshot.definedAnywhere(key)
        if (missing) {
          val target = defaults.headOption.orElse(snapshot.translations.headOption)
          val where  = target.map(_.label).getOrElse("messages")
          val fixes = target.toArray.map { f =>
            new AddMessageKeyQuickFix(key, AddMessageKeyQuickFix.stubValue(key), f.file, "Add message key to messages file"): LocalQuickFix
          }
          holder.registerProblem(lit, PlayPsi.contentRange(lit), s"Message key '$key' is not defined in $where", fixes*)
        }
      }
    }
}

/**
 * "Missing translation": a key defined in the default `conf/messages` but absent from a `conf/messages.xx` file of
 * the project. Keys that only exist in a library's `messages.default` are not reported.
 */
final class MissingTranslationInspection extends LocalInspectionTool {
  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    MessagesInspectionSupport.visitor { (lit, use) =>
      val snapshot = MessagesIndex.snapshot(lit.getProject)
      val key      = use.key
      snapshot.defaults.flatMap(_.byKey.get(key)).headOption.foreach { default =>
        val missing = snapshot.translations.filterNot(_.byKey.contains(key))
        if (missing.nonEmpty) {
          val fixes = missing.map { f =>
            new AddMessageKeyQuickFix(key, default.value, f.file, "Add missing translation"): LocalQuickFix
          }
          holder.registerProblem(
            lit,
            s"Message key '$key' has no translation in ${missing.map(_.label).mkString(", ")}",
            ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
            PlayPsi.contentRange(lit),
            fixes*
          )
        }
      }
    }
}

/** Appends `key=value` to a messages file. */
final class AddMessageKeyQuickFix(key: String, value: String, file: VirtualFile, family: String) extends LocalQuickFix {
  override def getFamilyName: String = family
  override def getName: String       = s"Add '$key' to ${file.getName}"

  private def block: String = s"$key=$value"

  override def getElementToMakeWritable(currentFile: PsiFile): PsiElement =
    Option(PsiManager.getInstance(currentFile.getProject).findFile(file)).getOrElse(currentFile)

  override def applyFix(project: Project, descriptor: ProblemDescriptor): Unit =
    if (file.isValid) TextFiles.append(project, file, block)

  override def generatePreview(project: Project, previewDescriptor: ProblemDescriptor): IntentionPreviewInfo = {
    val (before, after) = TextFiles.appendedText(file, block)
    new IntentionPreviewInfo.CustomDiff(PlainTextFileType.INSTANCE, file.getName, before, after)
  }
}

object AddMessageKeyQuickFix {

  /** `user.greeting` → `TODO user.greeting`: visible in the UI until someone writes the real text. */
  def stubValue(key: String): String = s"TODO $key"
}
