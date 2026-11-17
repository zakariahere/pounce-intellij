package pounce.runtime.tests

import com.intellij.execution.actions.{ConfigurationContext, RunConfigurationProducer}
import com.intellij.execution.lineMarker.RunLineMarkerContributor
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.{ScalaLanguage, ScalaVersion}
import org.jetbrains.sbt.runner.SbtRunConfiguration
import org.junit.Assert.*
import pounce.runtime.settings.PounceRuntimeSettings
import pounce.testing.PounceFixtureTestBase

import scala.jdk.CollectionConverters.*

/** Real weaver 0.13.1 + munit-cats-effect 2.2.1 jars (non-transitive Ivy loads: every needed jar is listed). */
abstract class TestFrameworksFixtureBase extends PounceFixtureTestBase {
  override protected def extraLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    Seq(
      IvyManagedLoader(
        "org.typelevel" %% "cats-kernel"        % catsVersion,
        "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion,
        "org.typelevel" %% "cats-effect-std"    % catsEffectVersion,
        "co.fs2"        %% "fs2-core"           % "3.14.0",
        "org.typelevel" %% "weaver-core"        % "0.13.1",
        "org.typelevel" %% "weaver-cats-core"   % "0.13.1",
        "org.typelevel" %% "weaver-cats"        % "0.13.1",
        "org.typelevel" %% "weaver-framework"   % "0.13.1",
        "org.scalameta" %% "munit"              % "1.3.6",
        "org.scalameta" %% "munit-diff"         % "1.3.6",
        "org.typelevel" %% "munit-cats-effect"  % "2.2.1"
      )
    )
  }

  protected def leafAt(marker: String, delta: Int = 0): PsiElement = {
    val offset = getFile.getText.indexOf(marker)
    assertTrue(s"no `$marker` in the file", offset >= 0)
    getFile.findElementAt(offset + delta)
  }
}

/** F2: weaver run gutters and the sbt `testOnly ... -- -o` configurations behind them. */
class WeaverGutterTest extends TestFrameworksFixtureBase {

  private val Suite =
    """package com.acme
      |
      |import cats.effect._
      |import weaver._
      |
      |object UserSuite extends SimpleIOSuite {
      |
      |  test("finds a user") {
      |    IO(expect(1 == 1))
      |  }
      |
      |  pureTest("pure \"quoted\" check") {
      |    expect(true)
      |  }
      |
      |  loggedTest("logged one") { log =>
      |    log.info("hi").as(success)
      |  }
      |
      |  test(s"interpolated ${1 + 1}") {
      |    IO(success)
      |  }
      |
      |  def helper(): Unit = {
      |    test("nested in a method") { IO(success) }
      |    ()
      |  }
      |}
      |
      |object Outer {
      |  object InnerSuite extends SimpleIOSuite {
      |    test("inner test") { IO(success) }
      |  }
      |}
      |
      |trait SharedTests extends SimpleIOSuite {
      |  test("shared test") { IO(success) }
      |}
      |
      |object NotWeaver {
      |  def test(name: String)(body: => Unit): Unit = ()
      |  test("plain method") { () }
      |}
      |""".stripMargin

  private def contributor = new WeaverRunLineMarkerContributor

  private def info(marker: String, delta: Int = 0): Option[RunLineMarkerContributor.Info] =
    Option(contributor.getInfo(leafAt(marker, delta)))

  private def target(marker: String, delta: Int = 0): WeaverSupport.Target =
    WeaverSupport.targetAtLeaf(leafAt(marker, delta)).getOrElse(fail(s"no weaver target at `$marker`").asInstanceOf[Nothing])

  def testContributorIsRegisteredForScala(): Unit =
    assertTrue(RunLineMarkerContributor.EXTENSION.allForLanguage(ScalaLanguage.INSTANCE).asScala.exists(_.isInstanceOf[WeaverRunLineMarkerContributor]))

  def testWhatOtherContributorsShowOnWeaverCode(): Unit = {
    configureFromFileText(Suite)
    val others = RunLineMarkerContributor.EXTENSION
      .allForLanguage(ScalaLanguage.INSTANCE)
      .asScala
      .filterNot(_.isInstanceOf[WeaverRunLineMarkerContributor])
    def from(marker: String) = others.filter(c => c.getInfo(leafAt(marker)) != null).map(_.getClass.getSimpleName)
    println(s"[pounce-f2] other gutters on a weaver suite name: ${from("UserSuite")}; on a weaver test: ${from("test(\"finds")}")
    assertTrue("only Pounce marks weaver tests", from("test(\"finds").isEmpty)
  }

  def testGutterOnEveryStaticallyNamedTest(): Unit = {
    configureFromFileText(Suite)
    assertEquals(Some("finds a user"), target("test(\"finds").testName)
    assertEquals(Some("pure \"quoted\" check"), target("pureTest(").testName)
    assertEquals(Some("logged one"), target("loggedTest(").testName)
    Seq("test(\"finds", "pureTest(", "loggedTest(").foreach(m => assertTrue(m, info(m).isDefined))
    assertEquals("Run weaver test \"finds a user\"", info("test(\"finds").get.tooltipProvider.apply(leafAt("test(\"finds")))
  }

  def testGutterOnSuiteName(): Unit = {
    configureFromFileText(Suite)
    val suite = target("UserSuite")
    assertEquals(None, suite.testName)
    assertEquals("com.acme.UserSuite", suite.selector)
    assertTrue(info("UserSuite").isDefined)
  }

  def testNoGutterWhereTheTestCannotBeRunOnItsOwn(): Unit = {
    configureFromFileText(Suite)
    // dynamic name, call nested in a method, trait (not instantiable by the framework), a non-weaver `test`
    Seq("test(s\"interpolated", "test(\"nested", "test(\"shared", "SharedTests", "test(\"plain", "NotWeaver", "helper")
      .foreach(m => assertEquals(m, None, info(m)))
    // only identifiers carry markers (run line markers are computed per leaf)
    assertEquals(None, info("\"finds a user\""))
  }

  def testNestedSuiteNames(): Unit = {
    configureFromFileText(Suite)
    val inner = target("test(\"inner")
    assertEquals("com.acme.Outer.InnerSuite", inner.selector)
    // weaver's suiteName is the JVM name without `$`
    assertEquals("com.acme.OuterInnerSuite", inner.suiteName)
    assertEquals(
      "testOnly com.acme.Outer.InnerSuite -- -o \"com.acme.OuterInnerSuite.inner test\"",
      WeaverSupport.sbtCommand(inner)
    )
  }

  def testSbtCommands(): Unit = {
    configureFromFileText(Suite)
    assertEquals(
      "testOnly com.acme.UserSuite -- -o \"com.acme.UserSuite.finds a user\"",
      WeaverSupport.sbtCommand(target("test(\"finds"))
    )
    assertEquals(
      "testOnly com.acme.UserSuite -- -o \"com.acme.UserSuite.pure \\\"quoted\\\" check\"",
      WeaverSupport.sbtCommand(target("pureTest("))
    )
    assertEquals("core/testOnly com.acme.UserSuite", WeaverSupport.sbtCommand(target("UserSuite"), "core/"))
  }

  def testGutterFollowsTheSetting(): Unit = {
    configureFromFileText(Suite)
    val s = PounceRuntimeSettings()
    s.weaverGutters = false
    try assertEquals(None, info("test(\"finds"))
    finally s.weaverGutters = true
  }

  def testTaggedTestNamesKeepTheirName(): Unit = {
    configureFromFileText(
      """import cats.effect._
        |import weaver._
        |object TaggedSuite extends SimpleIOSuite {
        |  test("focused".only) { IO(success) }
        |  test("skipped".ignore) { IO(success) }
        |}
        |""".stripMargin
    )
    assertEquals(Some("focused"), target("test(\"focused").testName)
    assertEquals(Some("skipped"), target("test(\"skipped").testName)
    assertEquals("testOnly TaggedSuite -- -o \"TaggedSuite.focused\"", WeaverSupport.sbtCommand(target("test(\"focused")))
  }

  // ------------------------------------------------------------------ run configuration producer

  private def producer: WeaverSbtTestConfigurationProducer =
    RunConfigurationProducer.getInstance(classOf[WeaverSbtTestConfigurationProducer])

  def testProducerCreatesSbtTestOnlyConfiguration(): Unit = {
    configureFromFileText(Suite)
    val context = new ConfigurationContext(leafAt("test(\"finds"))
    val created = Option(producer.createConfigurationFromContext(context)).getOrElse(fail("no configuration").asInstanceOf[Nothing])
    val config  = created.getConfiguration.asInstanceOf[SbtRunConfiguration]
    assertEquals("testOnly com.acme.UserSuite -- -o \"com.acme.UserSuite.finds a user\"", config.commands)
    assertEquals("weaver: UserSuite.finds a user", config.getName)
    assertTrue(producer.isConfigurationFromContext(config, context))
    assertFalse(producer.isConfigurationFromContext(config, new ConfigurationContext(leafAt("loggedTest("))))
  }

  def testProducerFromInsideTheTestBody(): Unit = {
    configureFromFileText(Suite)
    val context = new ConfigurationContext(leafAt("expect(1 == 1)"))
    val created = Option(producer.createConfigurationFromContext(context))
    // the body is not the test call itself: the suite (the enclosing object) is not picked up from inside a body
    created.foreach(c => assertTrue(c.getConfiguration.asInstanceOf[SbtRunConfiguration].commands.startsWith("testOnly com.acme.UserSuite")))
  }

  def testProducerIgnoresNonWeaverCode(): Unit = {
    configureFromFileText(Suite)
    assertNull(producer.createConfigurationFromContext(new ConfigurationContext(leafAt("test(\"plain"))))
    assertNull(producer.createConfigurationFromContext(new ConfigurationContext(leafAt("test(s\"interpolated"))))
  }

  def testOurConfigurationIsPreferredForWeaverTests(): Unit = {
    configureFromFileText(Suite)
    val context = new ConfigurationContext(leafAt("test(\"finds"))
    val all     = Option(context.getConfigurationsFromContext).map(_.asScala.toList).getOrElse(Nil)
    println(s"[pounce-f2] configurations from context at a weaver test: ${all.map(c => c.getConfiguration.getClass.getSimpleName + ": " + c.getConfiguration.getName)}")
    assertTrue(all.nonEmpty)
    assertTrue(all.head.getConfiguration.isInstanceOf[SbtRunConfiguration])
  }
}

/** F2: munit-cats-effect suites are left to the Scala plugin's MUnit support. */
class MunitCatsEffectGutterTest extends TestFrameworksFixtureBase {

  def testOurContributorStaysOutOfMunitSuites(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |import munit.CatsEffectSuite
        |
        |class UserSpec extends CatsEffectSuite {
        |  test("munit test") {
        |    IO(1).assertEquals(1)
        |  }
        |}
        |""".stripMargin
    )
    val leaf = leafAt("test(\"munit")
    assertNull(new WeaverRunLineMarkerContributor().getInfo(leaf))
    assertNull(new WeaverRunLineMarkerContributor().getInfo(leafAt("UserSpec")))
    // report what the Scala plugin itself offers here (the MUnit integration is a content module of the Scala plugin)
    val others = RunLineMarkerContributor.EXTENSION
      .allForLanguage(ScalaLanguage.INSTANCE)
      .asScala
      .filterNot(_.isInstanceOf[WeaverRunLineMarkerContributor])
    val onTest  = others.filter(c => c.getInfo(leaf) != null).map(_.getClass.getName)
    val onSuite = others.filter(c => c.getInfo(leafAt("UserSpec")) != null).map(_.getClass.getName)
    println(s"[pounce-f2] Scala-language run line marker contributors: ${others.map(_.getClass.getName).mkString(", ")}")
    println(s"[pounce-f2] munit-cats-effect test(\"...\") gutter from: ${onTest.mkString(", ")}; suite gutter from: ${onSuite.mkString(", ")}")
  }
}
