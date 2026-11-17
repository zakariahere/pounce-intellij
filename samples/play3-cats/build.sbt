// Pounce showcase: Scala 2.13 + Play 3 + cats-effect. Open this folder in an IDE that has Pounce installed
// (or `sbt runIDE` from the plugin repo) and follow samples/play3-cats/README.md.
ThisBuild / scalaVersion := "2.13.18"
ThisBuild / organization := "dev.pounce.samples"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := "pounce-sample-play3-cats",
    libraryDependencies ++= Seq(
      guice,
      "org.typelevel"          %% "cats-core"          % "2.13.0",
      "org.typelevel"          %% "cats-effect"        % "3.6.3",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test,
      "org.typelevel"          %% "munit-cats-effect"  % "2.2.1" % Test
    ),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
  )
