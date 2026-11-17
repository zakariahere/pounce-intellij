package pounce.play.config

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.codeInspection.*
import com.intellij.openapi.fileTypes.PlainTextFileType
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElement, PsiElementVisitor, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.play.util.{PlayPsi, TextFiles}

/**
 * "Unknown config key": a required read (`get[T]`, `getString`, `at`, ...) of a path that no project conf file and no
 * library `reference.conf` defines. Stays silent when it can't be sure: optional reads (`getOptional`, `has`,
 * `hasPath`, `get[Option[T]]`), paths below a `${?ENV}` substitution, relative configs with an unknown prefix,
 * guarded reads (`if (config.has("k")) ...`, `Try(...)`), unresolvable includes, and projects without a conf file.
 */
final class UnknownConfigKeyInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = element match {
        case lit: ScStringLiteral => check(lit, holder)
        case _                    =>
      }
    }

  private def check(lit: ScStringLiteral, holder: ProblemsHolder): Unit =
    PlayPsi.plainLiteral(lit).filter(_.getValue.nonEmpty).foreach { _ =>
      if (ConfCalls.enabled(lit)) {
        ConfCalls.use(lit).filterNot(_.optional).foreach { use =>
          use.absolutePath.foreach { path =>
            val snapshot = ConfIndex.snapshot(lit.getProject)
            val unknown =
              snapshot.hasProjectConf && !snapshot.incomplete && !snapshot.contains(path) &&
                !snapshot.isUnderDynamic(path) && !UnknownConfigKeyInspection.guarded(lit, use.key)
            if (unknown) {
              val fixes = snapshot.mainConf.toArray.map { conf =>
                new AddConfKeyQuickFix(path, AddConfKeyQuickFix.placeholder(use), conf): LocalQuickFix
              }
              holder.registerProblem(
                lit,
                PlayPsi.contentRange(lit),
                s"Unknown config key '$path': not defined in application.conf or any library reference.conf",
                fixes*
              )
            }
          }
        }
      }
    }
}

object UnknownConfigKeyInspection {

  private val GuardMethods = Set("has", "hasPath", "hasPathOrNull", "getOptional")

  /** Inside `Try(...)`, or the same key is checked with `has` / `hasPath` / `getOptional` elsewhere in the file. */
  def guarded(lit: ScStringLiteral, key: String): Boolean = {
    import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScMethodCall, ScReferenceExpression}
    val inTry = Iterator.iterate[PsiElement](lit.getParent)(_.getParent).takeWhile(e => e != null && !e.isInstanceOf[PsiFile]).exists {
      case call: ScMethodCall =>
        call.getInvokedExpr match {
          case r: ScReferenceExpression => r.refName == "Try"
          case _                        => false
        }
      case _ => false
    }
    inTry || {
      val literals = com.intellij.psi.util.PsiTreeUtil.findChildrenOfType(lit.getContainingFile, classOf[ScStringLiteral])
      literals.stream().anyMatch { other =>
        other != lit && other.getValue == key && (other.getParent match {
          case args: org.jetbrains.plugins.scala.lang.psi.api.expr.ScArgumentExprList =>
            args.getParent match {
              case call: ScMethodCall =>
                call.getInvokedExpr match {
                  case r: ScReferenceExpression => GuardMethods.contains(r.refName)
                  case g: org.jetbrains.plugins.scala.lang.psi.api.expr.ScGenericCall =>
                    g.referencedExpr match {
                      case r: ScReferenceExpression => GuardMethods.contains(r.refName)
                      case _                        => false
                    }
                  case _ => false
                }
              case _ => false
            }
          case _ => false
        })
      }
    }
  }
}

/** Appends `path = <placeholder>` to the project's application.conf. */
final class AddConfKeyQuickFix(path: String, placeholder: String, conf: VirtualFile) extends LocalQuickFix {

  override def getFamilyName: String = "Add key to application.conf"
  override def getName: String       = s"Add '$path' to ${conf.getName}"

  private def block: String = s"$path = $placeholder"

  override def startInWriteAction(): Boolean = true

  override def getElementToMakeWritable(currentFile: PsiFile): PsiElement =
    Option(com.intellij.psi.PsiManager.getInstance(currentFile.getProject).findFile(conf)).getOrElse(currentFile)

  override def applyFix(project: Project, descriptor: ProblemDescriptor): Unit =
    if (conf.isValid) TextFiles.append(project, conf, block)

  override def generatePreview(project: Project, previewDescriptor: ProblemDescriptor): IntentionPreviewInfo = {
    val (before, after) = TextFiles.appendedText(conf, block)
    new IntentionPreviewInfo.CustomDiff(PlainTextFileType.INSTANCE, conf.getName, before, after)
  }
}

object AddConfKeyQuickFix {

  /** A value of the type the call reads, so the app starts after the fix (`getInt` → 0, `getString` → ""). */
  def placeholder(use: ConfKeyUse): String = {
    val name = use.call.name
    val typeArg = use.call.typeArgs.headOption.map(_.getText.trim).getOrElse("")
    def byType(t: String): String = t match {
      case "Int" | "Long" | "Double" | "Float" | "Short" | "Number" => "0"
      case "Boolean"                                                => "false"
      case "Configuration" | "Config"                               => "{}"
      case t if t.startsWith("Seq[") || t.startsWith("List[")       => "[]"
      case "FiniteDuration" | "Duration"                            => "1s"
      case _                                                        => "\"\""
    }
    name match {
      case "get" | "getAndValidate"                                    => byType(typeArg)
      case "at" | "getConfig" | "getObject"                            => "{}"
      case "getInt" | "getLong" | "getDouble" | "getNumber"            => "0"
      case "getBoolean"                                                => "false"
      case "getDuration" | "getMillis" | "getNanos" | "getMilliseconds" | "getNanoseconds" => "1s"
      case "getBytes" | "getMemorySize"                                => "1M"
      case n if n.endsWith("List")                                     => "[]"
      case _                                                           => "\"\""
    }
  }
}
