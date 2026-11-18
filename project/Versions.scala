import org.jetbrains.sbtidea.Keys.*
import org.jetbrains.sbtidea.download.BuildInfo
import org.jetbrains.sbtidea.download.idea.IntellijVersionUtils
import org.jetbrains.sbtidea.IntellijPlugin
import sbt.*

object Versions {
  /** Must match the Scala 3 runtime bundled by the Scala plugin (scala3-library_3.jar in its lib/ folder). */
  val scala3: String = "3.7.4"

  /** IntelliJ IDEA 2026.2.1 */
  val intellijVersion: String      = "262.9437.185"
  val intellijHumanVersion: String = "2026.2"
  val sinceBuild: String           = "262.9437"
  val untilBuild: String           = "262.*"

  /** Last Scala plugin release compatible with 262.9437 (2026.2.20+ need 262.10315). */
  val scalaPluginVersion: String = "2026.2.18"

  /**
   * Local builds are snapshots. The release workflow (.github/workflows/release.yml) sets POUNCE_VERSION from the git
   * tag: `v0.2.0` builds 0.2.0.
   */
  val pluginVersion: String = sys.env.get("POUNCE_VERSION").filter(_.trim.nonEmpty).getOrElse("0.1.0-SNAPSHOT")

  lazy val (
    intellijVersion_ForManagedIntellijDependencies,
    intellijRepository_ForManagedIntellijDependencies
  ) = {
    val location = IntellijVersionUtils.detectArtifactLocation(BuildInfo(intellijVersion, IntelliJPlatform.Idea), ".zip")
    (location.artifactVersion, location.repository)
  }
}

object Dependencies {
  val junit: Seq[ModuleID] = Seq(
    "junit"          % "junit"           % "4.13.2" % Test,
    "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
    "org.opentest4j" % "opentest4j"      % "1.3.0"  % Test
  )

  private def platform(name: String) =
    "com.jetbrains.intellij.platform" % name % Versions.intellijVersion_ForManagedIntellijDependencies
  private def java(name: String) =
    "com.jetbrains.intellij.java" % name % Versions.intellijVersion_ForManagedIntellijDependencies

  val intellijTestFrameworkAll: Seq[ModuleID] = Seq(
    platform("test-framework-core"),
    platform("test-framework-common"),
    platform("test-framework"),
    java("java-test-framework-shared"),
    java("java-test-framework-backend"),
    java("java-test-framework"),
    platform("debugger-test-framework"),
    platform("uast-test-framework"),
    // runtime deps of test-framework-core that the error-reporting path needs (NoClassDefFoundError otherwise)
    platform("test-framework-team-city"),
    "com.jetbrains.intellij.codeowners" % "codeowners-runtime-resolver" % Versions.intellijVersion_ForManagedIntellijDependencies
  ).map(_.notTransitive() % Test)

  /** Typesafe Config: parses application.conf / reference.conf for config-key completion & navigation. */
  val typesafeConfig: ModuleID = "com.typesafe" % "config" % "1.4.3"

  /** In 262 most platform features are content modules of the IDE itself, not separate plugins. */
  val intellijPlugins: Seq[IntellijPlugin] = Seq(
    // 262 split these platform parts into bundled plugins; Java (and therefore Scala) won't load without them.
    "intellij.structureView.plugin",
    "intellij.testRunner.plugin",
    "intellij.bookmarks.plugin",
    "intellij.todo.plugin",
    "intellij.libraries.misc.plugin", // lucene, needed by spellchecker -> java.impl
    "intellij.structuralSearch.plugin",
    "com.intellij.properties", // Play messages files
    "com.intellij.java",
    "intellij.java.aetherDependencyResolver.plugin",
    "JUnit",
    s"org.intellij.scala:${Versions.scalaPluginVersion}"
  ).map(_.toPlugin)
}
