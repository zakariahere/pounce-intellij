package pounce.play

import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import pounce.testing.{PounceFixtureTestBase, PounceInspectionTestBase}
import com.intellij.codeInspection.LocalInspectionTool

import scala.reflect.ClassTag

/** Real Play 3 jars (non-transitive Ivy loads, so every needed jar is listed). */
object PlayTestLibs {
  val PlayVersion  = "3.0.9"
  val PekkoVersion = "1.0.3"

  def loaders(using ScalaVersion): Seq[LibraryLoader] =
    Seq(
      IvyManagedLoader(
        "org.playframework"     %% "play"               % PlayVersion,
        "org.playframework"     %% "play-configuration" % PlayVersion,
        "org.playframework"     %% "play-streams"       % PlayVersion,
        "org.apache.pekko"      %% "pekko-actor"        % PekkoVersion,
        "org.apache.pekko"      %% "pekko-stream"       % PekkoVersion,
        "com.typesafe"           % "config"             % "1.4.3",
        "javax.inject"           % "javax.inject"       % "1",
        "com.google.inject"      % "guice"              % "6.0.0",
        "net.codingwell"        %% "scala-guice"        % "6.0.0",
        "com.github.pureconfig" %% "pureconfig-core"    % "0.17.9",
        "org.typelevel"         %% "cats-effect-kernel" % "3.6.3",
        "org.typelevel"         %% "cats-effect-std"    % "3.6.3"
      )
    )
}

abstract class PlayFixtureTestBase extends PounceFixtureTestBase {
  override protected def extraLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    PlayTestLibs.loaders
  }
}

abstract class PlayInspectionTestBase[T <: LocalInspectionTool: ClassTag] extends PounceInspectionTestBase[T] {
  override protected def extraLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    PlayTestLibs.loaders
  }
}
