package pounce.play.config

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootModificationTracker
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.search.{FilenameIndex, GlobalSearchScope, ProjectScope}
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager}
import com.typesafe.config.*
import pounce.play.util.TextFiles

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * One definition of a HOCON path.
 *
 * @param dynamic the value is an unresolved substitution / concatenation (`${?ENV}`, `${base} {...}`, `+=`): keys
 *                below it may exist at runtime, so "unknown key" checks stay silent under it
 */
final case class ConfEntry(
  path: String,
  value: String,
  valueType: String,
  file: VirtualFile,
  line: Int,
  library: Boolean,
  dynamic: Boolean
) {
  def segments: List[String] = ConfPaths.split(path)
  def lastSegment: String    = segments.lastOption.getOrElse(path)

  /** `application.conf` or `reference.conf (play_2.13-3.0.9.jar)`. */
  def fileLabel: String =
    if (!library) file.getName
    else {
      val jar = Option(com.intellij.openapi.vfs.VfsUtilCore.getVirtualFileForJar(file)).map(_.getName)
      jar.fold(file.getName)(j => s"${file.getName} ($j)")
    }
}

object ConfPaths {
  def split(path: String): List[String] =
    try ConfigUtil.splitPath(path).asScala.toList
    catch { case NonFatal(_) => path.split('.').toList }

  def join(segments: Seq[String]): String =
    try ConfigUtil.joinPath(segments.asJava)
    catch { case NonFatal(_) => segments.mkString(".") }

  /** `a.b.c` → `a`, `a.b`, `a.b.c`. */
  def prefixes(path: String): List[String] = {
    val segs = split(path)
    segs.indices.map(i => join(segs.take(i + 1))).toList
  }
}

/** All config keys known for a project: its own conf files (+ includes) and every library `reference.conf`. */
final class ConfSnapshot(val projectPart: ConfIndex.Part, val libraryPart: ConfIndex.Part) {

  def hasProjectConf: Boolean = projectPart.roots.nonEmpty

  /** Some project include could not be resolved, or a conf file does not parse: keys may be missing from the model. */
  def incomplete: Boolean = projectPart.incomplete

  def definitions(path: String): Seq[ConfEntry] =
    projectPart.byPath.getOrElse(path, Nil) ++ libraryPart.byPath.getOrElse(path, Nil)

  def contains(path: String): Boolean = projectPart.byPath.contains(path) || libraryPart.byPath.contains(path)

  /** The path itself or one of its parents is defined by a substitution: anything can be below it. */
  def isUnderDynamic(path: String): Boolean =
    ConfPaths.prefixes(path).exists(p => definitions(p).exists(_.dynamic))

  /** One entry per path (project definition wins), project paths first. */
  def allEntries: Seq[ConfEntry] = {
    val seen = mutable.HashSet.empty[String]
    (projectPart.entries ++ libraryPart.entries).filter(e => seen.add(e.path))
  }

  def projectFiles: Seq[VirtualFile] = projectPart.files

  /** The `application.conf` to add keys to: the one under a `conf/` directory first. */
  def mainConf: Option[VirtualFile] = {
    val apps = projectPart.roots.filter(_.getName == "application.conf")
    apps.find(f => Option(f.getParent).exists(_.getName == "conf")).orElse(apps.headOption)
  }

  def entriesAt(file: VirtualFile, line: Int): Seq[ConfEntry] =
    projectPart.entries.filter(e => e.file == file && e.line == line)
}

object ConfIndex {

  final case class Part(entries: Seq[ConfEntry], roots: Seq[VirtualFile], files: Seq[VirtualFile], incomplete: Boolean) {
    lazy val byPath: Map[String, Seq[ConfEntry]] = entries.groupBy(_.path)
  }

  private val EmptyPart = Part(Nil, Nil, Nil, incomplete = false)

  def snapshot(project: Project): ConfSnapshot = new ConfSnapshot(projectPart(project), libraryPart(project))

  /** Files that make up the project's configuration (used to recognise conf files in navigation handlers). */
  def isProjectConfFile(project: Project, vf: VirtualFile): Boolean = projectPart(project).files.contains(vf)

  // ---- project conf files: re-parsed only when one of them (or the file tree) changes --------------------------

  private def projectPart(project: Project): Part =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val part = computeProjectPart(project)
        CachedValueProvider.Result.create(
          part,
          new TextFiles.FilesTracker(part.files),
          ProjectRootModificationTracker.getInstance(project)
        )
      }
    )

  private def computeProjectPart(project: Project): Part = {
    val scope = ProjectScope.getContentScope(project)
    val apps  = FilenameIndex.getVirtualFilesByName("application.conf", scope).asScala.toSeq
    val variants = apps.flatMap { app =>
      Option(app.getParent).toSeq.flatMap(_.getChildren.toSeq).filter { f =>
        val n = f.getName
        !f.isDirectory && n != "application.conf" && n.startsWith("application.") && n.endsWith(".conf")
      }
    }
    val references = FilenameIndex.getVirtualFilesByName("reference.conf", scope).asScala.toSeq
    val roots      = (apps ++ variants ++ references).distinct.sortBy(_.getPath)
    if (roots.isEmpty) EmptyPart
    else {
      val parser  = new Parser(project, library = false)
      val entries = roots.flatMap(parser.entriesOf)
      Part(entries, roots, parser.filesRead.toSeq, parser.incomplete)
    }
  }

  // ---- library reference.conf files: re-parsed only when libraries change ---------------------------------------

  private def libraryPart(project: Project): Part =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () =>
        CachedValueProvider.Result.create(computeLibraryPart(project), ProjectRootModificationTracker.getInstance(project))
    )

  private def computeLibraryPart(project: Project): Part = {
    val scope = ProjectScope.getLibrariesScope(project)
    val files = Seq("reference.conf", "reference-overrides.conf")
      .flatMap(n => FilenameIndex.getVirtualFilesByName(n, scope).asScala)
      .distinct
      .sortBy(_.getPath)
    val parser = new Parser(project, library = true)
    Part(files.flatMap(parser.entriesOf), files, parser.filesRead.toSeq, incomplete = false)
  }

  // ---- parsing ---------------------------------------------------------------------------------------------------

  /** Last successfully parsed entries per file URL: used while the user is mid-edit and the file does not parse. */
  private val LastGood: Key[ConcurrentHashMap[String, Seq[ConfEntry]]] = Key.create("pounce.play.conf.lastGood")

  private def lastGood(project: Project): ConcurrentHashMap[String, Seq[ConfEntry]] = {
    val existing = project.getUserData(LastGood)
    if (existing != null) existing
    else {
      val created = new ConcurrentHashMap[String, Seq[ConfEntry]]()
      project.putUserData(LastGood, created)
      created
    }
  }

  private final class Parser(project: Project, library: Boolean) {
    val filesRead: mutable.LinkedHashSet[VirtualFile] = mutable.LinkedHashSet.empty
    var incomplete: Boolean                           = false
    private val urls                                  = mutable.LinkedHashMap.empty[String, VirtualFile]

    def entriesOf(root: VirtualFile): Seq[ConfEntry] = {
      val obj = parseFile(root, depth = 0)
      obj match {
        case Some(o) =>
          val out = Seq.newBuilder[ConfEntry]
          walk(o, Nil, root, out)
          val result = out.result()
          lastGood(project).put(root.getUrl, result)
          result
        case None =>
          incomplete = true
          Option(lastGood(project).get(root.getUrl)).getOrElse(Nil)
      }
    }

    private def parseFile(vf: VirtualFile, depth: Int): Option[ConfigObject] =
      if (depth > 8 || vf == null) None
      else {
        filesRead += vf
        urls(vf.getUrl) = vf
        TextFiles.text(vf).flatMap { text =>
          val options = ConfigParseOptions.defaults
            .setOriginDescription(vf.getUrl)
            .setSyntax(syntaxOf(vf))
            .setAllowMissing(true)
            .setIncluder(new Includer(vf, depth))
          try Some(ConfigFactory.parseString(text, options).root())
          catch {
            case e: com.intellij.openapi.diagnostic.ControlFlowException => throw e
            case NonFatal(_)                                             => None
          }
        }
      }

    private def syntaxOf(vf: VirtualFile): ConfigSyntax = vf.getExtension match {
      case "json"       => ConfigSyntax.JSON
      case "properties" => ConfigSyntax.PROPERTIES
      case _            => ConfigSyntax.CONF
    }

    private def walk(obj: ConfigObject, prefix: List[String], current: VirtualFile, out: mutable.Builder[ConfEntry, Seq[ConfEntry]]): Unit = {
      val keys =
        try obj.keySet().asScala.toList.sorted
        catch { case NonFatal(_) => Nil }
      for (key <- keys) {
        val value = try Option(obj.get(key)) catch { case NonFatal(_) => None }
        value.foreach { v =>
          val segments    = prefix :+ key
          val (tpe, dyn0) = typeOf(v)
          var dynamic     = dyn0
          v match {
            case child: ConfigObject if !dynamic =>
              try child.keySet()
              catch { case NonFatal(_) => dynamic = true }
            case _ =>
          }
          val (file, line) = originOf(v.origin(), current)
          out += ConfEntry(ConfPaths.join(segments), render(v, tpe), tpe, file, line, library, dynamic)
          v match {
            case child: ConfigObject if !dynamic => walk(child, segments, current, out)
            case _                               =>
          }
        }
      }
    }

    private def typeOf(v: ConfigValue): (String, Boolean) =
      try (v.valueType().name().toLowerCase, false)
      catch { case NonFatal(_) => ("substitution", true) }

    private def render(v: ConfigValue, tpe: String): String = tpe match {
      case "object" => "{…}"
      case _ =>
        val text =
          try v.render(ConfigRenderOptions.concise())
          catch { case NonFatal(_) => "${…}" }
        if (text.length > 60) text.take(57) + "…" else text
    }

    /** Origins carry the file URL we passed as origin description (`url: line`, or `merge of url: 1,url: 2`). */
    private def originOf(origin: ConfigOrigin, current: VirtualFile): (VirtualFile, Int) =
      if (origin == null) (current, -1)
      else {
        val desc = Option(origin.description()).getOrElse("").stripPrefix("merge of ")
        val file = urls.iterator
          .filter((url, _) => desc.startsWith(url))
          .maxByOption(_._1.length)
          .map(_._2)
          .getOrElse(current)
        (file, origin.lineNumber())
      }

    private final class Includer(from: VirtualFile, depth: Int)
        extends ConfigIncluder
        with ConfigIncluderFile
        with ConfigIncluderURL
        with ConfigIncluderClasspath {

      override def withFallback(fallback: ConfigIncluder): ConfigIncluder = this

      /** `include "x"`: relative to the including file first, then the classpath. */
      override def include(context: ConfigIncludeContext, what: String): ConfigObject =
        relative(what).orElse(classpath(what)).getOrElse(missing())

      override def includeFile(context: ConfigIncludeContext, what: java.io.File): ConfigObject =
        if (!what.isAbsolute) relative(what.getPath.replace('\\', '/')).getOrElse(missing())
        else
          Option(LocalFileSystem.getInstance.findFileByIoFile(what))
            .flatMap(parseFile(_, depth + 1))
            .getOrElse(missing())

      override def includeURL(context: ConfigIncludeContext, what: java.net.URL): ConfigObject = missing()

      override def includeResources(context: ConfigIncludeContext, what: String): ConfigObject =
        classpath(what).getOrElse(missing())

      private def candidates(what: String): Seq[String] =
        if (what.contains('.') && what.lastIndexOf('.') > what.lastIndexOf('/')) Seq(what)
        else Seq(s"$what.conf", s"$what.json", s"$what.properties")

      private def relative(what: String): Option[ConfigObject] = {
        val dir = from.getParent
        if (dir == null) None
        else
          candidates(what).iterator
            .flatMap(c => Option(dir.findFileByRelativePath(c)))
            .nextOption()
            .flatMap(parseFile(_, depth + 1))
      }

      private def classpath(what: String): Option[ConfigObject] = {
        val scope = GlobalSearchScope.allScope(project)
        candidates(what.stripPrefix("/")).iterator.flatMap { c =>
          val name = c.substring(c.lastIndexOf('/') + 1)
          FilenameIndex.getVirtualFilesByName(name, scope).asScala.filter(_.getPath.endsWith(c))
        }.nextOption().flatMap(parseFile(_, depth + 1))
      }

      private def missing(): ConfigObject = {
        if (!library) incomplete = true
        ConfigFactory.empty().root()
      }
    }
  }
}
