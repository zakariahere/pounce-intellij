import org.jetbrains.sbtidea.packaging.PackagingMethod
import org.jetbrains.sbtidea.verifier.FailureLevel
import Versions.*

ThisBuild / intellijPluginName := "pounce"
ThisBuild / intellijBuild      := intellijVersion
ThisBuild / intellijPlatform   := IntelliJPlatform.Idea

ThisBuild / autoRemoveOldCachedIntelliJSDK := true
ThisBuild / autoRemoveOldCachedDownloads   := true
Global / intellijAttachSources             := true

addCommandAlias("fmt", "scalafmtAll")
addCommandAlias("check", "scalafmtCheckAll")

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-Wconf:msg=is deprecated:s" // Scala plugin APIs move fast; deprecations are tracked separately
)

/** Stamps version and IDE range into plugin.xml (applied in core, where plugin.xml lives). */
lazy val pluginXmlPatch = patchPluginXml := pluginXmlOptions { xml =>
  xml.version    = version.value
  xml.sinceBuild = Versions.sinceBuild
  xml.untilBuild = Versions.untilBuild
}

/** The plugin artifact: plugin.xml + all pillars merged into one jar. */
lazy val root =
  newProject("pounce", file("."))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      // sbt-idea-plugin only patches plugin.xml in the packaging project's own products, and ours is in core.
      packageArtifact := packageArtifact.dependsOn(core / doPatchPluginXml).value,
      // Release tasks act on the merged plugin only, not on each pillar's partial test packaging.
      packageArtifactZip / aggregate := false,
      runPluginVerifier / aggregate  := false,
      // Fail on real breakage. Deprecated / experimental / internal usages are reported, not fatal: most are
      // compiler-generated mixin forwarders for Java default methods (e.g. Configurable.isModified), not our calls.
      pluginVerifierOptions := pluginVerifierOptions.value.copy(failureLevels = Set(
        FailureLevel.COMPATIBILITY_PROBLEMS,
        FailureLevel.INVALID_PLUGIN,
        FailureLevel.MISSING_DEPENDENCIES,
        FailureLevel.OVERRIDE_ONLY_API_USAGES,
        FailureLevel.NON_EXTENDABLE_API_USAGES
      ))
    )
    // root tests run against the fully merged plugin (plugin.path = target/plugin/pounce): the wiring regression net
    .dependsOn(core % "compile->compile;test->test", completion, inspections, visual, play, runtime)
    .dependsOn(scalaPluginTestkit % "test->test")
    .aggregate(core, completion, inspections, visual, play, runtime)

/** Shared engine: library detection, type language, typeclass hierarchy, instance oracle, catalog, imports, settings. */
lazy val core =
  newProject("core", file("core"))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      pluginXmlPatch,
      // Patch the copied plugin.xml, never before the copy: on a clean build copyResources would otherwise overwrite
      // the stamped file with the raw one (`<version>replaced-by-build</version>` in the released zip).
      doPatchPluginXml := doPatchPluginXml.dependsOn(Compile / copyResources).value,
      packageMethod := PackagingMethod.MergeIntoParent(),
      libraryDependencies += Dependencies.typesafeConfig,
      packageLibraryMappings += "com.typesafe" % "config" % ".*" -> Some("lib/typesafe-config.jar")
    )
    .dependsOn(scalaPluginTestkit % "test->test")

/** A: typeclass-aware completion, postfix templates, quick documentation. */
lazy val completion = pillar("completion")

/** B + C: effect-safety inspections and simplifications. */
lazy val inspections = pillar("inspections")

/** D: inlay hints, gutters, Typeclass Explorer, Combinator Finder, endpoints. */
lazy val visual = pillar("visual")

/** E: Play 3 config keys, Guice navigation, i18n messages, bridge helpers. */
lazy val play = pillar("play")

/** F + G: debugger support, fiber dumps, live templates, compiler-backed completion spike. */
lazy val runtime = pillar("runtime")

def pillar(name: String): Project =
  newProject(name, file(name))
    .enablePlugins(SbtIdeaPlugin)
    .settings(packageMethod := PackagingMethod.MergeIntoParent())
    .dependsOn(core % "compile->compile;test->test", scalaPluginTestkit % "test->test")

// Vendored copy of the IntelliJ Scala plugin test framework - see scala-plugin-testkit/ORIGIN.md.
lazy val scalaPluginTestkit =
  newProject("scala-plugin-testkit", file("scala-plugin-testkit"))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      packageMethod := PackagingMethod.Skip(),
      scalacOptions -= "-Werror"
    )

def newProject(projectName: String, base: File): Project =
  Project(projectName, base).settings(
    name         := projectName,
    scalaVersion := scala3,
    version      := pluginVersion,
    resolvers += Versions.intellijRepository_ForManagedIntellijDependencies,
    libraryDependencies ++= Dependencies.junit,
    libraryDependencies ++= Dependencies.intellijTestFrameworkAll,
    intellijPlugins := Dependencies.intellijPlugins,

    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q")
  )
