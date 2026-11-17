package pounce.play

import org.junit.Assert.*
import pounce.play.config.UnknownConfigKeyInspection

/** E1 (c): "Unknown config key" — positives, negatives and the quick fix. */
class ConfigInspectionTest extends PlayInspectionTestBase[UnknownConfigKeyInspection] {

  override protected val description = "Unknown config key"

  override protected def descriptionMatches(s: String): Boolean = s != null && s.startsWith(description)

  private val AppConf =
    """app {
      |  name = "pounce"
      |  db.url = "jdbc:postgresql://localhost/app"
      |  secrets = ${?APP_SECRETS}
      |}
      |""".stripMargin

  private def addConf(text: String = AppConf): Unit = myFixture.addFileToProject("conf/application.conf", text)

  private def controller(body: String): String =
    s"""import play.api.Configuration
       |import com.typesafe.config.{Config, ConfigFactory}
       |import javax.inject.Inject
       |import scala.util.Try
       |class UserController @Inject() (config: Configuration) {
       |  $body
       |}
       |""".stripMargin

  def testUnknownKeyIsReported(): Unit = {
    addConf()
    checkTextHasError(controller(s"""def a = config.get[String]("${START}app.nmae$END")"""))
  }

  def testUnknownKeyViaUnderlyingAndConfigFactory(): Unit = {
    addConf()
    checkTextHasError(
      controller(
        s"""def a = config.underlying.getString("${START}app.dbb.url$END")
           |  def b = ConfigFactory.load().getInt("${START}app.port$END")""".stripMargin
      )
    )
  }

  def testUnknownRelativeKeyWithKnownPrefix(): Unit = {
    addConf()
    checkTextHasError(controller(s"""def a = config.underlying.getConfig("app.db").getString("${START}user$END")"""))
  }

  def testKnownProjectAndLibraryKeysAreFine(): Unit = {
    addConf()
    checkTextHasNoErrors(
      controller(
        """def a = config.get[String]("app.name")
          |  def b = config.underlying.getString("app.db.url")
          |  def c = config.get[String]("play.http.secret.key")
          |  def d = config.get[Configuration]("app")
          |  def e = config.underlying.getConfig("app.db").getString("url")
          |  def f = ConfigFactory.load().getString("pekko.actor.provider")""".stripMargin
      )
    )
  }

  def testOptionalReadsAreNotReported(): Unit = {
    addConf()
    checkTextHasNoErrors(
      controller(
        """def a = config.getOptional[String]("app.missing")
          |  def b = config.has("app.missing2")
          |  def c = config.get[Option[String]]("app.missing3")
          |  def d = config.underlying.hasPath("app.missing4")""".stripMargin
      )
    )
  }

  def testKeysBelowEnvSubstitutionAreNotReported(): Unit = {
    addConf()
    checkTextHasNoErrors(controller("""def a = config.get[String]("app.secrets.apiKey")"""))
  }

  def testUnknownPrefixIsNotReported(): Unit = {
    addConf()
    checkTextHasNoErrors(
      controller(
        """def a(cfg: Config) = cfg.getString("anything.at.all")
          |  def b(c: Configuration) = c.get[String]("relative.key")""".stripMargin
      )
    )
  }

  def testGuardedReadsAreNotReported(): Unit = {
    addConf()
    checkTextHasNoErrors(
      controller(
        """def a = if (config.has("feature.flag")) config.get[Boolean]("feature.flag") else false
          |  def b = Try(config.underlying.getString("legacy.key")).toOption""".stripMargin
      )
    )
  }

  def testNoConfFileNoWarnings(): Unit =
    checkTextHasNoErrors(controller("""def a = config.get[String]("anything")"""))

  def testUnresolvableIncludeDisablesWarnings(): Unit = {
    addConf(AppConf + "include \"local-overrides.conf\"\n")
    checkTextHasNoErrors(controller("""def a = config.get[String]("app.nmae")"""))
  }

  def testUnrelatedStringsAreIgnored(): Unit = {
    addConf()
    checkTextHasNoErrors(controller("""def a = println("app.nmae")"""))
  }

  def testQuickFixAppendsKeyToApplicationConf(): Unit = {
    addConf()
    val fixes = doFindQuickFixes(controller(s"""def a = config.get[Int]("${START}app.port$END")"""), "Add 'app.port' to application.conf")
    assertEquals(1, fixes.size)
    myFixture.launchAction(fixes.head)
    val conf = myFixture.findFileInTempDir("conf/application.conf")
    val text = com.intellij.openapi.fileEditor.FileDocumentManager.getInstance.getDocument(conf).getText
    assertTrue(text, text.endsWith("app.port = 0\n"))
    // fixed: no more warning
    assertTrue(myFixture.doHighlighting().stream().noneMatch(h => descriptionMatches(h.getDescription)))
  }
}
