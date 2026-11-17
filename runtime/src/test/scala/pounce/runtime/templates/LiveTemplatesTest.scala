package pounce.runtime.templates

import com.intellij.codeInsight.template.TemplateActionContext
import com.intellij.codeInsight.template.impl.{TemplateManagerImpl, TemplateSettings}
import com.intellij.openapi.actionSystem.IdeActions
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.fileTypes.PlainTextFileType
import org.junit.Assert.*
import pounce.testing.PounceFixtureTestBase

import scala.jdk.CollectionConverters.*

/** F4: Cats Effect / weaver / munit-cats-effect live templates. */
class LiveTemplatesTest extends PounceFixtureTestBase {

  private val Group = "Pounce (Cats Effect)"

  /** Types the abbreviation at the caret, expands it with Tab and accepts every default. */
  private def expand(abbreviation: String, text: String = "<caret>"): String = {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable)
    configureFromFileText(text)
    myFixture.`type`(abbreviation)
    myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB)
    // finishing the template reformats the document: that must happen inside a command
    Option(TemplateManagerImpl.getTemplateState(getEditor)).foreach { state =>
      CommandProcessor.getInstance().executeCommand(getProject, () => state.gotoEnd(false), "Finish template", null)
    }
    getFile.getText
  }

  private def applicableHere(text: String): Set[String] = {
    configureFromFileText(text)
    TemplateManagerImpl
      .listApplicableTemplates(TemplateActionContext.expanding(getFile, getEditor))
      .asScala
      .filter(_.getGroupName == Group)
      .map(_.getKey)
      .toSet
  }

  private def assertContains(text: String, parts: String*): Unit =
    parts.foreach(p => assertTrue(s"expected `$p` in:\n$text", text.contains(p)))

  def testAllTemplatesAreRegistered(): Unit =
    Seq("ioapp", "ioappargs", "resmake", "algebra", "ioref", "weaversuite", "munitce").foreach { name =>
      assertNotNull(name, TemplateSettings.getInstance().getTemplate(name, Group))
    }

  def testIoApp(): Unit = {
    val text = expand("ioapp")
    assertContains(text, "import cats.effect.{IO, IOApp}", "extends IOApp.Simple {", "def run: IO[Unit] =", "IO.println(\"Hello, Cats Effect!\")")
    assertTrue(text, text.matches("(?s).*object \\w+ extends IOApp\\.Simple.*"))
  }

  def testIoAppWithArgs(): Unit = {
    val text = expand("ioappargs")
    assertContains(text, "import cats.effect.{ExitCode, IO, IOApp}", "extends IOApp {", "def run(args: List[String]): IO[ExitCode] =", ".as(ExitCode.Success)")
  }

  def testResourceMake(): Unit = {
    val text = expand("resmake", "import cats.effect._\nobject T {\n  val r = <caret>\n}\n")
    assertContains(text, "val r = Resource.make(IO(???))(r => IO.unit)")
  }

  def testAlgebra(): Unit = {
    val text = expand("algebra")
    assertTrue(text, text.matches("(?s)trait (\\w+)\\[F\\[_]] \\{.*}\\s*object \\1 \\{\\s*def apply\\[F\\[_]]\\(implicit ev: \\1\\[F]\\): \\1\\[F] = ev\\s*}\\s*"))
  }

  def testIoRef(): Unit = {
    val text = expand("ioref", "import cats.effect._\nobject T {\n  val counter = <caret>\n}\n")
    assertContains(text, "val counter = Ref.of[IO, Int](0)")
  }

  def testWeaverSuite(): Unit = {
    val text = expand("weaversuite")
    assertContains(text, "import weaver.SimpleIOSuite", "extends SimpleIOSuite {", "test(\"does something\") {", "pureTest(\"pure expectation\") {", "expect(1 + 1 == 2)")
  }

  def testMunitCatsEffectSuite(): Unit = {
    val text = expand("munitce")
    assertContains(text, "import munit.CatsEffectSuite", "extends CatsEffectSuite {", "test(\"does something\") {", "IO.pure(1).assertEquals(1)")
  }

  def testApplicableInCodeOnly(): Unit = {
    val all = Set("ioapp", "ioappargs", "resmake", "algebra", "ioref", "weaversuite", "munitce")
    assertEquals(all, applicableHere("object T {\n  <caret>\n}\n"))
    assertEquals(Set.empty, applicableHere("object T {\n  // <caret>\n}\n"))
    assertEquals(Set.empty, applicableHere("object T {\n  val s = \"<caret>\"\n}\n"))
  }

  def testNoExpansionOutsideScala(): Unit = {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable)
    myFixture.configureByText(PlainTextFileType.INSTANCE, "<caret>")
    myFixture.`type`("ioapp")
    myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB)
    assertFalse(myFixture.getFile.getText.contains("IOApp"))
  }
}
