package pounce.play.util

import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.ModificationTracker
import com.intellij.openapi.vfs.{VirtualFile, VirtualFileManager}
import com.intellij.psi.PsiDocumentManager

import java.util.regex.Pattern
import scala.util.control.NonFatal

/**
 * Text-level access to non-Scala project files (`application.conf`, `conf/messages*`). These are plain text unless
 * the HOCON plugin is installed, so Pounce works on their text (unsaved edits included) rather than on a PSI tree.
 */
object TextFiles {

  def text(vf: VirtualFile): Option[String] =
    if (vf == null || !vf.isValid || vf.isDirectory) None
    else
      Option(FileDocumentManager.getInstance.getCachedDocument(vf)) match {
        case Some(doc) => Some(doc.getImmutableCharSequence.toString)
        case None =>
          try Some(com.intellij.openapi.fileEditor.impl.LoadTextUtil.loadText(vf).toString)
          catch {
            case e: com.intellij.openapi.diagnostic.ControlFlowException => throw e
            case NonFatal(_)                                             => None
          }
      }

  /** Changes on every edit of the file, saved or not. */
  def stamp(vf: VirtualFile): Long =
    if (vf == null || !vf.isValid) -1L
    else
      Option(FileDocumentManager.getInstance.getCachedDocument(vf))
        .map(_.getModificationStamp)
        .getOrElse(vf.getModificationStamp)

  /** Invalidates a cache when any of `files` is edited or files are created / deleted / moved anywhere. */
  final class FilesTracker(files: Seq[VirtualFile]) extends ModificationTracker {
    override def getModificationCount: Long =
      files.foldLeft(VirtualFileManager.VFS_STRUCTURE_MODIFICATIONS.getModificationCount)((acc, f) => acc * 31 + stamp(f))
  }

  /** Offset of the start of a 1-based line, clamped to the text. */
  def lineStart(text: String, line: Int): Int = {
    var offset = 0
    var current = 1
    while (current < line && offset < text.length) {
      val nl = text.indexOf('\n', offset)
      if (nl < 0) offset = text.length
      else {
        offset = nl + 1
        current += 1
      }
    }
    math.min(offset, text.length)
  }

  def lineOf(text: String, offset: Int): Int = {
    var line = 1
    var i    = 0
    val end  = math.min(offset, text.length)
    while (i < end) {
      if (text.charAt(i) == '\n') line += 1
      i += 1
    }
    line
  }

  /** Offset of `word` as a whole token on a 1-based line (falls back to the first non-blank char of the line). */
  def wordOffsetOnLine(text: String, line: Int, word: String): Int = {
    val start = lineStart(text, line)
    val end   = { val nl = text.indexOf('\n', start); if (nl < 0) text.length else nl }
    val lineText = text.substring(start, end)
    val m = Pattern.compile("(?<![\\w-])\"?" + Pattern.quote(word) + "\"?(?![\\w-])").matcher(lineText)
    if (m.find()) start + m.start() + (if (lineText.charAt(m.start()) == '"') 1 else 0)
    else start + lineText.indexWhere(c => !c.isWhitespace).max(0)
  }

  /** Appends `block` to the end of a file (adding a separating newline when needed), in the caller's write action. */
  def append(project: Project, vf: VirtualFile, block: String): Boolean =
    Option(FileDocumentManager.getInstance.getDocument(vf)) match {
      case Some(doc) =>
        val current = doc.getCharsSequence
        val sep     = if (current.length == 0 || current.charAt(current.length - 1) == '\n') "" else "\n"
        doc.insertString(doc.getTextLength, sep + block + (if (block.endsWith("\n")) "" else "\n"))
        PsiDocumentManager.getInstance(project).commitDocument(doc)
        true
      case None => false
    }

  def appendedText(vf: VirtualFile, block: String): (String, String) = {
    val before = text(vf).getOrElse("")
    val sep    = if (before.isEmpty || before.endsWith("\n")) "" else "\n"
    (before, before + sep + block + (if (block.endsWith("\n")) "" else "\n"))
  }
}
