package pounce.play

import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.junit.Assert.*
import pounce.play.util.TextTarget

import scala.jdk.CollectionConverters.*

/** E1: completion, navigation and reverse navigation of config keys. */
class ConfigKeysTest extends PlayFixtureTestBase {

  private val AppConf =
    """# app settings
      |app {
      |  name = "pounce"
      |  db.url = "jdbc:postgresql://localhost/app"
      |  timeout = 5s
      |}
      |include "secrets.conf"
      |""".stripMargin

  private def addConf(): Unit = {
    myFixture.addFileToProject("conf/application.conf", AppConf)
    myFixture.addFileToProject("conf/secrets.conf", "secrets.apiKey = ${?API_KEY}\n")
    myFixture.addFileToProject("conf/application.prod.conf", "include \"application.conf\"\nprod.only = true\n")
  }

  private def controller(body: String): String =
    s"""import play.api.Configuration
       |import com.typesafe.config.{Config, ConfigFactory}
       |import javax.inject.Inject
       |class UserController @Inject() (config: Configuration) {
       |  $body
       |}
       |""".stripMargin

  private def complete(body: String): Seq[String] = {
    configureFromFileText(controller(body))
    myFixture.completeBasic()
    Option(myFixture.getLookupElementStrings).map(_.asScala.toSeq).getOrElse(Nil)
  }

  def testCompletionOffersProjectIncludedAndLibraryKeys(): Unit = {
    addConf()
    val items = complete("""def a = config.get[String]("<caret>")""")
    assertTrue(items.toString, items.contains("app.name"))
    assertTrue(items.toString, items.contains("app.db.url"))
    assertTrue("included file", items.contains("secrets.apiKey"))
    assertTrue("env variant", items.contains("prod.only"))
    assertTrue("Play reference.conf", items.contains("play.http.secret.key"))
    assertTrue("Pekko reference.conf", items.contains("pekko.actor.provider"))
    // project keys first
    assertTrue(items.indexOf("app.name") < items.indexOf("play.http.secret.key"))
  }

  def testCompletionPresentationShowsValueAndOrigin(): Unit = {
    addConf()
    complete("""def a = config.get[String]("app.<caret>")""")
    val element      = myFixture.getLookupElements.find(_.getLookupString == "app.name").get
    val presentation = new com.intellij.codeInsight.lookup.LookupElementPresentation
    element.renderElement(presentation)
    assertEquals(" = \"pounce\"", presentation.getTailText)
    assertEquals("application.conf", presentation.getTypeText)
  }

  def testCompletionLibraryKeyShowsJar(): Unit = {
    addConf()
    complete("""def a = config.get[String]("play.http.secret.k<caret>")""")
    val lookup = myFixture.getLookupElements
    if (lookup == null) myFixture.checkResult(controller("""def a = config.get[String]("play.http.secret.key")"""))
    else {
      val element      = lookup.find(_.getLookupString == "play.http.secret.key").get
      val presentation = new com.intellij.codeInsight.lookup.LookupElementPresentation
      element.renderElement(presentation)
      assertTrue(presentation.getTypeText, presentation.getTypeText.startsWith("reference.conf (play_2.13"))
    }
  }

  def testCompletionInsertsFullKey(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = config.get[String]("app.db.u<caret>")"""))
    myFixture.completeBasic()
    myFixture.checkResult(controller("""def a = config.get[String]("app.db.url")"""))
  }

  def testCompletionRelativeToGetConfig(): Unit = {
    addConf()
    val items = complete("""def a = config.underlying.getConfig("app").getString("<caret>")""")
    assertTrue(items.toString, items.contains("name"))
    assertTrue(items.toString, items.contains("db.url"))
    assertFalse(items.toString, items.contains("app.name"))
    assertFalse(items.toString, items.contains("play.http.secret.key"))
  }

  def testCompletionInTypesafeConfigAndConfigFactory(): Unit = {
    addConf()
    assertTrue(complete("""def a = ConfigFactory.load().getString("<caret>")""").contains("app.name"))
  }

  def testCompletionInValDefinedSubConfig(): Unit = {
    addConf()
    val items = complete(
      """private val db = config.get[Configuration]("app.db")
        |  def a = db.get[String]("<caret>")""".stripMargin
    )
    assertTrue(items.toString, items.contains("url"))
  }

  def testCompletionInPureconfigAt(): Unit = {
    addConf()
    assertTrue(complete("""def a = pureconfig.ConfigSource.default.at("<caret>")""").contains("app.db"))
  }

  def testNoCompletionInUnrelatedStrings(): Unit = {
    addConf()
    val items = complete("""def a = println("<caret>")""")
    assertFalse(items.toString, items.contains("app.name"))
  }

  def testNavigationToApplicationConfLine(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = config.get[String]("app.na<caret>me")"""))
    myFixture.getElementAtCaret match {
      case t: TextTarget =>
        assertEquals("application.conf", t.file.getName)
        assertEquals(3, t.line)
        assertEquals("app.name", t.name)
        val text = t.getContainingFile.getText
        assertTrue(text.substring(t.getTextOffset).startsWith("name = \"pounce\""))
      case other => fail(s"unexpected target $other")
    }
  }

  def testNavigationToIncludedFile(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = config.underlying.getString("secrets.api<caret>Key")"""))
    val target = myFixture.getElementAtCaret.asInstanceOf[TextTarget]
    assertEquals("secrets.conf", target.file.getName)
    assertEquals(1, target.line)
  }

  def testNavigationToLibraryReferenceConf(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = config.get[String]("play.http.secret.k<caret>ey")"""))
    val target = myFixture.getElementAtCaret.asInstanceOf[TextTarget]
    assertEquals("reference.conf", target.file.getName)
    assertTrue(target.file.getPath, target.file.getPath.contains("play_2.13"))
    assertTrue(target.line > 0)
  }

  def testNavigationRelativeKey(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = config.underlying.getConfig("app.db").getString("u<caret>rl")"""))
    val target = myFixture.getElementAtCaret.asInstanceOf[TextTarget]
    assertEquals("app.db.url", target.name)
    assertEquals(4, target.line)
  }

  def testNoReferenceOnUnrelatedLiteral(): Unit = {
    addConf()
    configureFromFileText(controller("""def a = println("app.na<caret>me")"""))
    val refs = getFile.findElementAt(getEditor.getCaretModel.getOffset).getParent.getReferences
    assertFalse(refs.toSeq.toString, refs.exists(_.isInstanceOf[pounce.play.config.ConfKeyReference]))
  }

  def testFindUsagesAndGotoFromConfFile(): Unit = {
    addConf()
    configureFromFileText(
      controller(
        """def a = config.get[String]("app.name")
          |  def b = config.underlying.getString("app.name")
          |  def c = config.get[String]("app.db.url")""".stripMargin
      )
    )
    val target = getFile.findReferenceAt(getFile.getText.indexOf("app.name") + 1).resolve().asInstanceOf[TextTarget]
    val usages = ReferencesSearch.search(target, GlobalSearchScope.projectScope(getProject)).findAll().asScala
    assertEquals(2, usages.size)

    // Ctrl+B on `name` in application.conf lists the Scala reads
    val conf = myFixture.findFileInTempDir("conf/application.conf")
    myFixture.configureFromExistingVirtualFile(conf)
    val offset  = myFixture.getFile.getText.indexOf("name =") + 1
    val targets = new KeyUsagesGotoHandler().getGotoDeclarationTargets(myFixture.getFile.findElementAt(offset), offset, myFixture.getEditor)
    assertNotNull(targets)
    assertEquals(2, targets.length)
    assertTrue(targets.forall(_.getText == "\"app.name\""))
  }
}
