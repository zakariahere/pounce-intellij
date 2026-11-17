package pounce

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.openapi.extensions.PluginId
import junit.framework.Assert.*
import pounce.core.settings.PounceSettings
import pounce.testing.PounceFixtureTestBase

import java.io.File
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*

/**
 * Runs against the fully merged plugin (the artifact users install). Guards plugin.xml and every pillar's
 * `META-INF/pounce-<pillar>.xml`: a typo'd class name or a broken include fails here, not in a user's IDE.
 */
class PluginWiringTest extends PounceFixtureTestBase {
  import PluginWiringTest.*

  def testPluginLoadsWithAllPillars(): Unit = {
    val plugin = PluginManagerCore.getPlugin(PluginId.getId(pounce.PluginId))
    assertNotNull("Pounce plugin not loaded", plugin)
    assertTrue("Pounce plugin disabled", PluginManagerCore.isLoaded(plugin.getPluginId))
  }

  def testCoreServicesAvailable(): Unit =
    assertNotNull(PounceSettings())

  /** Every extension implementation declared by Pounce must be loadable. */
  def testAllPounceExtensionClassesResolve(): Unit = allExtensionClassesResolve()

  /**
   * The installed artifact is `lib/pounce.jar` under `plugin.path`, not the test classpath, which always has every
   * pillar compiled in. sbt-idea-plugin's incremental packager has produced a stale jar before (descriptors present,
   * pillar classes missing), so check the jar itself.
   */
  def testPackagedJarContainsEveryExtensionClass(): Unit = packagedJarContainsEveryExtensionClass()

  private def allExtensionClassesResolve(): Unit = {
    val loader = getClass.getClassLoader
    val missing = for {
      xml <- Descriptors
      url <- Option(loader.getResource(xml)).toSeq
      text = scala.io.Source.fromURL(url, "UTF-8").mkString
      fqn <- classesNamedIn(text)
      if scala.util.Try(Class.forName(fqn, false, loader)).isFailure
    } yield s"$xml: $fqn"
    assertTrue(s"Unloadable extension classes:\n${missing.mkString("\n")}", missing.isEmpty)
  }

  private def packagedJarContainsEveryExtensionClass(): Unit = {
    val pluginPath = Option(System.getProperty("plugin.path")).getOrElse("")
    val candidates = pluginPath.split(",").toSeq.flatMap(_.split(File.pathSeparator)).filter(_.nonEmpty)
    val jarFile    = candidates.map(p => new File(p, "lib/pounce.jar")).find(_.isFile)
    assertTrue(s"no lib/pounce.jar under plugin.path=$pluginPath", jarFile.isDefined)

    // Compare against the descriptors in the source tree, never the jar against itself: a stale jar from an older
    // build is internally consistent (empty pillar descriptors, no pillar classes) and would pass. The classloader is
    // no reference either: sbt-idea-plugin puts the packaged jar itself on the test classpath instead of main classes.
    val sources = sourceDescriptors()
    def compiled(xml: String): String = new String(java.nio.file.Files.readAllBytes(sources(xml).toPath), "UTF-8")

    val jar = new JarFile(jarFile.get)
    try {
      val entries = jar.entries().asScala.map(_.getName).toSet
      def packaged(xml: String): String = new String(jar.getInputStream(jar.getEntry(xml)).readAllBytes(), "UTF-8")

      assertEquals(s"descriptors in ${jarFile.get}", Descriptors.toSet, Descriptors.filter(entries.contains).toSet)
      // plugin.xml is patched (version, IDE range) while packaging; pillar descriptors are copied verbatim.
      val stale = Descriptors.filterNot(_ == "META-INF/plugin.xml").filter(xml => packaged(xml) != compiled(xml))
      assertTrue(s"Stale descriptors in ${jarFile.get} (rebuild the artifact): ${stale.mkString(", ")}", stale.isEmpty)
      val missing = for {
        xml <- Descriptors
        fqn <- classesNamedIn(compiled(xml))
        if !entries.contains(fqn.replace('.', '/') + ".class")
      } yield s"$xml: $fqn"
      assertTrue(s"Classes named in descriptors but missing from ${jarFile.get}:\n${missing.mkString("\n")}", missing.isEmpty)
    } finally jar.close()
  }
}

object PluginWiringTest {

  val Descriptors: Seq[String] =
    "META-INF/plugin.xml" +: Seq("completion", "inspections", "visual", "play", "runtime").map(p => s"META-INF/pounce-$p.xml")

  private val ClassAttr =
    """(?:implementation|implementationClass|instance|serviceImplementation|class|factoryClass|providerClass)="(pounce\.[^"]+)"""".r

  def classesNamedIn(xml: String): Seq[String] = ClassAttr.findAllMatchIn(xml).map(_.group(1)).toSeq

  /** Descriptor path in the jar → its source file (`core/` holds plugin.xml, `<pillar>/` its own descriptor). */
  def sourceDescriptors(): Map[String, File] = {
    val start = new File(System.getProperty("user.dir")).getAbsoluteFile
    val root = Iterator
      .iterate(start)(_.getParentFile)
      .takeWhile(_ != null)
      .find(d => new File(d, "core/src/main/resources/META-INF/plugin.xml").isFile)
      .getOrElse(throw new AssertionError(s"repository root not found above $start"))
    Descriptors.map { xml =>
      val module = xml match {
        case "META-INF/plugin.xml" => "core"
        case other                 => other.stripPrefix("META-INF/pounce-").stripSuffix(".xml")
      }
      xml -> new File(root, s"$module/src/main/resources/$xml")
    }.toMap
  }
}
