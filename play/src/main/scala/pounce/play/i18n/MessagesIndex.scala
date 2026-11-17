package pounce.play.i18n

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootModificationTracker
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.search.{FilenameIndex, ProjectScope}
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager}
import pounce.play.util.TextFiles

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** One `key=value` line of a Play messages file (`line` is 1-based). */
final case class MessageEntry(key: String, value: String, line: Int)

/**
 * A Play messages file: `conf/messages` (default, `lang` = None), `conf/messages.fr`, `conf/messages.en-US`, or a
 * library's `messages.default` (Play's own `error.required`, `constraint.min`, ...).
 */
final case class MessagesFile(file: VirtualFile, lang: Option[String], library: Boolean, entries: Seq[MessageEntry]) {
  lazy val byKey: Map[String, MessageEntry] = entries.map(e => e.key -> e).toMap
  def label: String = file.getName
  def isDefault: Boolean = lang.isEmpty && !library
}

final class MessagesSnapshot(val project: Seq[MessagesFile], val library: Seq[MessagesFile]) {
  def defaults: Seq[MessagesFile]     = project.filter(_.isDefault)
  def translations: Seq[MessagesFile] = project.filter(_.lang.nonEmpty)
  def hasProjectFiles: Boolean        = project.nonEmpty
  def all: Seq[MessagesFile]          = defaults ++ translations ++ library

  def definitions(key: String): Seq[(MessagesFile, MessageEntry)] =
    all.flatMap(f => f.byKey.get(key).map(f -> _))

  /** Known to Play's fallback chain for the default language: project default file(s) or a library default. */
  def definedByDefault(key: String): Boolean =
    defaults.exists(_.byKey.contains(key)) || library.exists(_.byKey.contains(key))

  def definedAnywhere(key: String): Boolean = all.exists(_.byKey.contains(key))

  def isMessagesFile(vf: VirtualFile): Boolean = project.exists(_.file == vf)
}

object MessagesIndex {

  /** `messages.fr`, `messages.en-US`, `messages.zh_Hant`; not `messages.scala`, `messages.html`, ... */
  private val LangSuffix = "^[A-Za-z]{2,3}([-_][A-Za-z0-9]{2,8})*$".r

  def langOf(fileName: String): Option[Option[String]] =
    if (fileName == "messages") Some(None)
    else if (fileName.startsWith("messages.")) {
      val suffix = fileName.stripPrefix("messages.")
      if (suffix == "default") Some(None)
      else if (LangSuffix.matches(suffix) && !Set("scala", "java", "html", "txt", "md", "kt", "js", "ts", "conf", "xml", "json", "yml").contains(suffix))
        Some(Some(suffix))
      else None
    } else None

  def snapshot(project: Project): MessagesSnapshot = new MessagesSnapshot(projectFiles(project), libraryFiles(project))

  private def projectFiles(project: Project): Seq[MessagesFile] =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val files = computeProjectFiles(project)
        CachedValueProvider.Result.create(
          files,
          new TextFiles.FilesTracker(files.map(_.file)),
          ProjectRootModificationTracker.getInstance(project)
        )
      }
    )

  private def computeProjectFiles(project: Project): Seq[MessagesFile] = {
    val scope = ProjectScope.getContentScope(project)
    // conf directories: where `messages` or `application.conf` live
    val dirs = (FilenameIndex.getVirtualFilesByName("messages", scope).asScala ++
      FilenameIndex.getVirtualFilesByName("application.conf", scope).asScala).flatMap(f => Option(f.getParent)).toSeq.distinct
    val files = for {
      dir  <- dirs
      file <- dir.getChildren.toSeq
      if !file.isDirectory
      lang <- langOf(file.getName)
    } yield MessagesFile(file, lang, library = false, parse(file))
    files.distinctBy(_.file).sortBy(f => (f.lang.isDefined, f.file.getPath))
  }

  private def libraryFiles(project: Project): Seq[MessagesFile] =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val scope = ProjectScope.getLibrariesScope(project)
        val files = FilenameIndex.getVirtualFilesByName("messages.default", scope).asScala.toSeq.sortBy(_.getPath)
        CachedValueProvider.Result.create(
          files.map(f => MessagesFile(f, None, library = true, parse(f))),
          ProjectRootModificationTracker.getInstance(project)
        )
      }
    )

  def parse(vf: VirtualFile): Seq[MessageEntry] = TextFiles.text(vf).map(parseText).getOrElse(Nil)

  /**
   * Play's messages format: `key = value` (or `key: value`), `#` comments, a trailing `\` continues the value on the
   * next line. Later definitions of a key win.
   */
  def parseText(text: String): Seq[MessageEntry] = {
    val out   = mutable.LinkedHashMap.empty[String, MessageEntry]
    val lines = text.split("\n", -1)
    var i     = 0
    while (i < lines.length) {
      val raw     = lines(i).stripSuffix("\r")
      val trimmed = raw.trim
      val lineNo  = i + 1
      i += 1
      if (trimmed.nonEmpty && !trimmed.startsWith("#") && !trimmed.startsWith("!")) {
        val sep = trimmed.indexWhere(c => c == '=' || c == ':')
        if (sep > 0) {
          val key   = trimmed.substring(0, sep).trim
          var value = trimmed.substring(sep + 1).trim
          while (value.endsWith("\\") && !value.endsWith("\\\\") && i < lines.length) {
            value = value.dropRight(1) + lines(i).stripSuffix("\r").trim
            i += 1
          }
          if (key.nonEmpty && !key.exists(_.isWhitespace)) out(key) = MessageEntry(key, value, lineNo)
        }
      }
    }
    out.values.toSeq
  }
}
