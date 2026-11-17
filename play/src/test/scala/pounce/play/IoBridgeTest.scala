package pounce.play

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInsight.template.impl.TemplateManagerImpl
import com.intellij.openapi.actionSystem.IdeActions
import org.junit.Assert.*

import scala.jdk.CollectionConverters.*

/** E4: the IO → Future intention for Play actions and the live templates. */
class IoBridgeTest extends PlayFixtureTestBase {

  private def controller(ctorExtra: String, action: String, imports: String = ""): String =
    s"""import cats.effect.IO
       |import cats.effect.std.Dispatcher
       |import cats.effect.unsafe.IORuntime
       |import play.api.mvc._
       |import javax.inject.Inject$imports
       |
       |class UserController @Inject() (val controllerComponents: ControllerComponents$ctorExtra) extends BaseController {
       |  def find(id: Long): IO[Result] = IO.pure(Ok(id.toString))
       |  $action
       |}
       |""".stripMargin

  private def bridge: Option[IntentionAction] =
    myFixture.getAvailableIntentions.asScala.find(_.getText.startsWith("Convert to Action.async"))

  private def convert(before: String, after: String, expectedText: Option[String] = None): Unit = {
    configureFromFileText(before)
    val action = bridge.getOrElse(fail("intention not available").asInstanceOf[Nothing])
    expectedText.foreach(t => assertEquals(t, action.getText))
    myFixture.launchAction(action)
    myFixture.checkResult(after)
  }

  def testActionWithImplicitRuntime(): Unit =
    convert(
      controller(")(implicit runtime: IORuntime", "def show(id: Long) = Act<caret>ion { implicit request =>\n    find(id)\n  }"),
      controller(")(implicit runtime: IORuntime", "def show(id: Long) = Action.async { implicit request =>\n    find(id).unsafeToFuture()\n  }"),
      Some("Convert to Action.async { io.unsafeToFuture() }")
    )

  def testActionWithoutRuntimeAddsGlobalImport(): Unit = {
    configureFromFileText(controller("", "def show(id: Long) = Action { find(id).map(<caret>identity) }"))
    val action = bridge.get
    assertEquals("Convert to Action.async { io.unsafeToFuture() } (adds import cats.effect.unsafe.implicits.global)", action.getText)
    myFixture.launchAction(action)
    val text = getFile.getText
    assertTrue(text, text.contains("import cats.effect.unsafe.implicits.global"))
    assertTrue(text, text.contains("def show(id: Long) = Action.async { find(id).map(identity).unsafeToFuture() }"))
  }

  def testExplicitRuntimeIsPassed(): Unit =
    convert(
      controller(", rt: IORuntime", "def show(id: Long) = Action { implicit request => fi<caret>nd(id) }"),
      controller(", rt: IORuntime", "def show(id: Long) = Action.async { implicit request => find(id).unsafeToFuture()(rt) }")
    )

  def testDispatcherIsUsed(): Unit =
    convert(
      controller(", dispatcher: Dispatcher[IO]", "def show(id: Long) = Action.async { implicit request => fi<caret>nd(id) }"),
      controller(", dispatcher: Dispatcher[IO]", "def show(id: Long) = Action.async { implicit request => dispatcher.unsafeToFuture(find(id)) }")
    )

  def testAlreadyAsyncWithBodyParser(): Unit =
    convert(
      controller(")(implicit runtime: IORuntime", "def create = Action(parse.json) { request => IO.pure(<caret>Created) }"),
      controller(")(implicit runtime: IORuntime", "def create = Action.async(parse.json) { request => IO.pure(Created).unsafeToFuture() }")
    )

  def testTaglessWithDispatcher(): Unit = {
    val before =
      """import cats.effect.std.Dispatcher
        |import play.api.mvc._
        |
        |class UserController[F[_]](val controllerComponents: ControllerComponents, dispatcher: Dispatcher[F], load: F[Result]) extends BaseController {
        |  def show = Action { lo<caret>ad }
        |}
        |""".stripMargin
    convert(before, before.replace("Action { lo<caret>ad }", "Action.async { dispatcher.unsafeToFuture(load) }"))
  }

  def testNotOfferedForPlainOrFutureActions(): Unit = {
    configureFromFileText(controller("", "def a = Action { Ok(\"x\") }\n  def b = Action.async { scala.concurrent.Future.successful(O<caret>k) }"))
    assertTrue(bridge.isEmpty)
    configureFromFileText(controller("", "def a = Act<caret>ion { Ok(\"x\") }"))
    assertTrue(bridge.isEmpty)
  }

  def testNotOfferedForTaglessWithoutDispatcher(): Unit = {
    configureFromFileText(
      """import play.api.mvc._
        |class C[F[_]](val controllerComponents: ControllerComponents, load: F[Result]) extends BaseController {
        |  def show = Action { lo<caret>ad }
        |}
        |""".stripMargin
    )
    assertTrue(bridge.isEmpty)
  }

  private def expand(abbreviation: String): String = {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable)
    configureFromFileText(
      """import play.api.mvc._
        |class C(val controllerComponents: ControllerComponents) extends BaseController {
        |  <caret>
        |}
        |""".stripMargin
    )
    myFixture.`type`(abbreviation)
    myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB)
    Option(TemplateManagerImpl.getTemplateState(getEditor)).foreach(_.gotoEnd(false))
    getFile.getText
  }

  def testIoActionLiveTemplate(): Unit = {
    val text = expand("ioaction")
    assertTrue(text, text.contains("def index = Action.async { implicit request =>"))
    assertTrue(text, text.contains("IO.pure(Ok)") && text.contains(".unsafeToFuture()"))
  }

  def testIoActionJsonLiveTemplate(): Unit = {
    val text = expand("ioactionjson")
    assertTrue(text, text.contains("def create = Action.async(parse.json) { implicit request =>"))
    assertTrue(text, text.contains("request.body.validate[T].fold("))
  }

  def testGuiceBindLiveTemplate(): Unit = {
    val text = expand("guicebind")
    assertTrue(text, text.contains("bind(classOf[Service]).to(classOf[ServiceImpl])"))
  }

  def testPlayModuleLiveTemplate(): Unit = {
    val text = expand("playmodule")
    assertTrue(text, text.contains("class Module extends AbstractModule {"))
    assertTrue(text, text.contains("override def configure(): Unit = {"))
  }
}
