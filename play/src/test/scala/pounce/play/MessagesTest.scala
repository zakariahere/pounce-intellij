package pounce.play

import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.junit.Assert.*
import pounce.play.i18n.{MessageKeyReference, MessagesIndex, MissingMessageKeyInspection, MissingTranslationInspection}
import pounce.play.util.TextTarget

import scala.jdk.CollectionConverters.*

private object MessagesFixtures {
  val Default: String =
    """# default messages
      |user.greeting=Hello {0}
      |user.farewell = Goodbye
      |long.text=first part \
      |  second part
      |""".stripMargin
  val French: String = "user.greeting=Bonjour {0}\n"

  def controller(body: String): String =
    s"""import play.api.i18n._
       |import play.api.mvc._
       |import javax.inject.Inject
       |class HomeController @Inject() (val controllerComponents: ControllerComponents, messagesApi: MessagesApi)
       |    extends BaseController with I18nSupport {
       |  $body
       |}
       |""".stripMargin
}

/** E3: completion, navigation and reverse navigation of i18n message keys. */
class MessagesTest extends PlayFixtureTestBase {
  import MessagesFixtures.*

  private def addMessages(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    myFixture.addFileToProject("conf/messages.fr", French)
  }

  def testParser(): Unit = {
    val entries = MessagesIndex.parseText(Default)
    assertEquals(Seq("user.greeting", "user.farewell", "long.text"), entries.map(_.key))
    assertEquals("Goodbye", entries(1).value)
    assertEquals(3, entries(1).line)
    assertEquals("first part second part", entries(2).value)
    assertEquals(None, MessagesIndex.langOf("messages.scala"))
    assertEquals(Some(Some("fr")), MessagesIndex.langOf("messages.fr"))
    assertEquals(Some(Some("en-US")), MessagesIndex.langOf("messages.en-US"))
  }

  def testCompletionInAllCallShapes(): Unit = {
    addMessages()
    for (call <- Seq(
           """def a(implicit m: Messages) = Messages("<caret>")""",
           """def a(implicit l: Lang) = messagesApi("<caret>")""",
           """def a(m: Messages) = m("<caret>")""",
           """def a(implicit request: MessagesRequestHeader) = request.messages("<caret>")""",
           """def a(r: RequestHeader) = messagesApi.preferred(r)("<caret>")"""
         )) {
      configureFromFileText(controller(call))
      myFixture.completeBasic()
      val items = Option(myFixture.getLookupElementStrings).map(_.asScala.toSeq).getOrElse(Nil)
      assertTrue(s"$call: $items", items.contains("user.greeting"))
      assertTrue(s"$call: $items", items.contains("user.farewell"))
      assertTrue(s"Play's messages.default: $items", items.contains("error.required"))
    }
  }

  /** `MessagesAbstractController`: `Action` yields a `MessagesRequest`, whose `messages` is a parameterless `Messages`. */
  def testReferenceInsideMessagesAction(): Unit = {
    addMessages()
    configureFromFileText(
      """import play.api.mvc._
        |import javax.inject.Inject
        |class HomeController @Inject() (mcc: MessagesControllerComponents) extends MessagesAbstractController(mcc) {
        |  def a = Action { implicit request => Ok(request.messages("user.gree<caret>ting")) }
        |}
        |""".stripMargin
    )
    val target = myFixture.getElementAtCaret.asInstanceOf[TextTarget]
    assertEquals("user.greeting", target.name)
  }

  /**
   * With `I18nSupport`, `request.messages` takes an implicit `MessagesApi`, so `request.messages("k")` passes "k" as
   * that argument (it does not compile): not a message key.
   */
  def testI18nSupportRequestMessagesWithArgumentIsNotAKey(): Unit = {
    addMessages()
    configureFromFileText(controller("""def a = Action { implicit request => Ok(request.messages("user.gree<caret>ting")) }"""))
    val refs = getFile.findElementAt(getEditor.getCaretModel.getOffset).getParent.getReferences
    assertFalse(refs.exists(_.isInstanceOf[MessageKeyReference]))
  }

  def testNoCompletionInUnrelatedString(): Unit = {
    addMessages()
    configureFromFileText(controller("""def a = println("<caret>")"""))
    myFixture.completeBasic()
    val items = Option(myFixture.getLookupElementStrings).map(_.asScala.toSeq).getOrElse(Nil)
    assertFalse(items.contains("user.greeting"))
  }

  def testNavigationToDefaultMessagesWithTranslationsAsAlternatives(): Unit = {
    addMessages()
    configureFromFileText(controller("""def a(implicit m: Messages) = Messages("user.gree<caret>ting")"""))
    val target = myFixture.getElementAtCaret.asInstanceOf[TextTarget]
    assertEquals("messages", target.file.getName)
    assertEquals(2, target.line)
    val ref = getFile.findReferenceAt(getEditor.getCaretModel.getOffset).asInstanceOf[MessageKeyReference]
    assertEquals(Seq("messages", "messages.fr"), ref.multiResolve(false).toSeq.map(_.getElement.asInstanceOf[TextTarget].file.getName))
  }

  def testReverseNavigationFromMessagesFile(): Unit = {
    addMessages()
    configureFromFileText(
      controller(
        """def a(implicit m: Messages) = Messages("user.farewell")
          |  def b(implicit l: Lang) = messagesApi("user.farewell")""".stripMargin
      )
    )
    val target = getFile.findReferenceAt(getFile.getText.indexOf("user.farewell") + 2).resolve().asInstanceOf[TextTarget]
    assertEquals(2, ReferencesSearch.search(target, GlobalSearchScope.projectScope(getProject)).findAll().size)

    myFixture.configureFromExistingVirtualFile(myFixture.findFileInTempDir("conf/messages"))
    val offset  = myFixture.getFile.getText.indexOf("user.farewell") + 1
    val targets = new KeyUsagesGotoHandler().getGotoDeclarationTargets(myFixture.getFile.findElementAt(offset), offset, myFixture.getEditor)
    assertEquals(2, targets.length)
  }
}

/** E3: "Missing message key". */
class MissingMessageKeyTest extends PlayInspectionTestBase[MissingMessageKeyInspection] {
  import MessagesFixtures.*

  override protected val description = "Message key"
  override protected def descriptionMatches(s: String): Boolean = s != null && s.contains("is not defined in")

  def testMissingKeyIsReported(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    checkTextHasError(controller(s"""def a(implicit m: Messages) = Messages("${START}user.unknown$END")"""))
  }

  def testKnownProjectAndPlayKeysAreFine(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    checkTextHasNoErrors(
      controller(
        """def a(implicit m: Messages) = Messages("user.greeting", "Bob")
          |  def b(implicit l: Lang) = messagesApi("error.required")
          |  def c(implicit l: Lang) = messagesApi.isDefinedAt("not.there")
          |  def d = println("not.a.key")""".stripMargin
      )
    )
  }

  def testProjectWithoutMessagesFilesIsNotChecked(): Unit =
    checkTextHasNoErrors(controller("""def a(implicit m: Messages) = Messages("user.unknown")"""))

  def testQuickFixAddsStub(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    val fixes = doFindQuickFixes(controller(s"""def a(implicit m: Messages) = Messages("${START}user.new$END")"""), "Add 'user.new' to messages")
    myFixture.launchAction(fixes.head)
    val text = com.intellij.openapi.fileEditor.FileDocumentManager.getInstance.getDocument(myFixture.findFileInTempDir("conf/messages")).getText
    assertTrue(text, text.endsWith("user.new=TODO user.new\n"))
  }
}

/** E3: "Missing translation". */
class MissingTranslationTest extends PlayInspectionTestBase[MissingTranslationInspection] {
  import MessagesFixtures.*

  override protected val description = "Message key"
  override protected def descriptionMatches(s: String): Boolean = s != null && s.contains("has no translation in")

  def testMissingTranslationIsReported(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    myFixture.addFileToProject("conf/messages.fr", French)
    checkTextHasError(controller(s"""def a(implicit m: Messages) = Messages("${START}user.farewell$END")"""))
  }

  def testTranslatedKeyIsFine(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    myFixture.addFileToProject("conf/messages.fr", French)
    checkTextHasNoErrors(controller("""def a(implicit m: Messages) = Messages("user.greeting")"""))
  }

  def testLibraryOnlyKeyAndNoTranslationsAreFine(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    checkTextHasNoErrors(controller("""def a(implicit m: Messages) = Messages("user.farewell")"""))
  }

  def testQuickFixAddsTranslationStub(): Unit = {
    myFixture.addFileToProject("conf/messages", Default)
    myFixture.addFileToProject("conf/messages.fr", French)
    val fixes = doFindQuickFixes(controller(s"""def a(implicit m: Messages) = Messages("${START}user.farewell$END")"""), "Add 'user.farewell' to messages.fr")
    myFixture.launchAction(fixes.head)
    val text = com.intellij.openapi.fileEditor.FileDocumentManager.getInstance.getDocument(myFixture.findFileInTempDir("conf/messages.fr")).getText
    assertEquals("user.greeting=Bonjour {0}\nuser.farewell=Goodbye\n", text)
  }
}
