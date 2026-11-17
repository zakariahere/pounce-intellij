package pounce.play.util

import com.intellij.icons.AllIcons
import com.intellij.navigation.ItemPresentation
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.impl.FakePsiElement
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}

import javax.swing.Icon

/**
 * A navigation target inside a text file Pounce does not have a PSI tree for (`application.conf`, `reference.conf`
 * inside a jar, `conf/messages`): a named position (file + 1-based line + token). Resolving a reference to it makes
 * Ctrl+click, Ctrl+B and Find Usages work without the HOCON plugin.
 *
 * @param kind  `config key` / `message key` (shown in popups)
 * @param name  the full key
 * @param token the text to place the caret on (the last key segment)
 */
final class TextTarget(
  project: Project,
  val file: VirtualFile,
  val line: Int,
  val name: String,
  val token: String,
  val kind: String,
  val detail: String
) extends FakePsiElement {

  private def psiFile: PsiFile = if (file.isValid) PsiManager.getInstance(project).findFile(file) else null

  lazy val offset: Int =
    TextFiles.text(file).map(t => if (line > 0) TextFiles.wordOffsetOnLine(t, line, token) else 0).getOrElse(0)

  override def getParent: PsiElement     = psiFile
  override def getContainingFile: PsiFile = psiFile
  override def getProject: Project        = project
  override def isValid: Boolean           = file.isValid && !project.isDisposed
  override def getName: String            = name
  override def getText: String            = name
  override def getTextOffset: Int         = offset
  override def getTextRange: TextRange    = TextRange.from(offset, token.length)
  override def getNavigationElement: PsiElement = this
  override def canNavigate: Boolean       = file.isValid
  override def canNavigateToSource: Boolean = file.isValid
  override def navigate(requestFocus: Boolean): Unit =
    new OpenFileDescriptor(project, file, offset).navigate(requestFocus)

  override def getPresentableText: String = name
  override def getLocationString: String  = if (line > 0) s"${file.getName}:$line" else file.getName
  override def getIcon(open: Boolean): Icon = AllIcons.Nodes.Property
  override def getPresentation: ItemPresentation = this
  override def toString: String = s"$kind $name (${file.getName}:$line)"

  override def equals(other: Any): Boolean = other match {
    case t: TextTarget => t.file == file && t.name == name && t.line == line
    case _             => false
  }
  override def hashCode: Int = (file, name, line).hashCode
}
