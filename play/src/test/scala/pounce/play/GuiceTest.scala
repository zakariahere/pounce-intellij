package pounce.play

import com.intellij.codeInsight.daemon.{GutterMark, LineMarkerInfo, RelatedItemLineMarkerInfo}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.junit.Assert.*
import pounce.play.guice.{GuiceIndex, UnboundInjectionInspection}

import scala.jdk.CollectionConverters.*

private object GuiceFixtures {
  val Model: String =
    """import com.google.inject.{AbstractModule, ImplementedBy, Provides}
      |import javax.inject.{Inject, Provider, Singleton}
      |import play.api.{Configuration, Environment}
      |import play.api.mvc._
      |import org.apache.pekko.actor.ActorSystem
      |import scala.concurrent.ExecutionContext
      |
      |trait UserRepo
      |class UserRepoImpl @Inject() (config: Configuration) extends UserRepo
      |trait Mailer
      |class SmtpMailer extends Mailer
      |trait Clock
      |class SystemClock extends Clock
      |@ImplementedBy(classOf[RedisCache]) trait Cache
      |class RedisCache extends Cache
      |trait Audit
      |class AuditImpl extends Audit
      |trait Unbound
      |abstract class AbstractUnbound
      |class NeedsArgs(x: Int)
      |class NoArgs
      |
      |class Module extends AbstractModule {
      |  override def configure(): Unit = {
      |    bind(classOf[UserRepo]).to(classOf[UserRepoImpl])
      |  }
      |  @Provides def mailer(): Mailer = new SmtpMailer
      |}
      |
      |class PlayModule extends play.api.inject.Module {
      |  def bindings(env: Environment, conf: Configuration) = Seq(play.api.inject.bind[Clock].to[SystemClock])
      |}
      |
      |class ScalaGuiceModule extends AbstractModule with net.codingwell.scalaguice.ScalaModule {
      |  override def configure(): Unit = bind[Audit].to[AuditImpl].asEagerSingleton()
      |}
      |""".stripMargin
}

/** E2: binding index and gutters. */
class GuiceTest extends PlayFixtureTestBase {
  import GuiceFixtures.*

  private def gutters(fragment: String): Seq[GutterMark] =
    myFixture.findAllGutters().asScala.toSeq.filter(g => Option(g.getTooltipText).exists(_.contains(fragment)))

  private def targetsOf(g: GutterMark): Seq[PsiElement] = g match {
    case r: LineMarkerInfo.LineMarkerGutterIconRenderer[?] =>
      r.getLineMarkerInfo match {
        case info: RelatedItemLineMarkerInfo[?] => info.createGotoRelatedItems.asScala.toSeq.map(_.getElement)
        case _                                  => Nil
      }
    case _ => Nil
  }

  private val Controller =
    """
      |class UserController @Inject() (
      |  repo: UserRepo,
      |  mailer: Provider[Mailer],
      |  clock: Clock,
      |  cache: Cache,
      |  audit: Audit,
      |  cc: ControllerComponents,
      |  system: ActorSystem
      |)(implicit ec: ExecutionContext) extends AbstractController(cc)
      |""".stripMargin

  def testIndexFindsEveryBindingStyle(): Unit = {
    configureFromFileText(Model + Controller)
    val snapshot = GuiceIndex.snapshot(getProject)
    def kinds(fqn: String) = snapshot.bindingsFor(fqn).map(b => (b.kind.label, b.targetFqn))
    assertEquals(Seq(("bind", Some("UserRepoImpl"))), kinds("UserRepo"))
    assertEquals(Seq(("@Provides", None)), kinds("Mailer"))
    assertEquals(Seq(("bind", Some("SystemClock"))), kinds("Clock"))
    assertEquals(Seq(("bind", Some("AuditImpl"))), kinds("Audit"))
    assertFalse(snapshot.isBound("Unbound"))
  }

  def testInjectionPointGuttersNavigateToBindings(): Unit = {
    configureFromFileText(Model + Controller)
    val repo = gutters("Guice binding for UserRepo")
    assertEquals(1, repo.size)
    assertTrue(targetsOf(repo.head).head.getText.startsWith("bind(classOf[UserRepo]).to(classOf[UserRepoImpl])"))

    val mailer = gutters("Guice binding for Mailer")
    assertEquals("Provider[Mailer] needs Mailer's binding", 1, mailer.size)
    assertTrue(targetsOf(mailer.head).head.getText.contains("def mailer(): Mailer"))

    assertEquals(1, gutters("Guice binding for Clock").size)
    assertEquals(1, gutters("Guice binding for Audit").size)
    val cache = gutters("Guice binding for Cache")
    assertEquals(1, cache.size)
    assertTrue(cache.head.getTooltipText.contains("@ImplementedBy"))

    // Play-bound types get no gutter
    assertTrue(gutters("Guice binding for ControllerComponents").isEmpty)
    assertTrue(gutters("Guice binding for ActorSystem").isEmpty)
  }

  def testBindingGuttersNavigateToInjectionPoints(): Unit = {
    configureFromFileText(Model + Controller + "class Other @Inject() (r: UserRepo)\nclass NotInjected(r: UserRepo)\n")
    val bindGutter = gutters("Navigate to injection points of UserRepo")
    assertEquals(1, bindGutter.size)
    val params = targetsOf(bindGutter.head).collect { case p: ScClassParameter => p.name }
    assertEquals(Set("repo", "r"), params.toSet)
    assertEquals("only @Inject constructors", 2, params.size)

    val provides = gutters("Navigate to injection points of Mailer")
    assertEquals(1, provides.size)
    assertEquals(Seq("mailer"), targetsOf(provides.head).collect { case p: ScClassParameter => p.name })

    assertEquals(1, gutters("Navigate to injection points of Clock").size)
    assertEquals(1, gutters("Navigate to injection points of Audit").size)
  }

  def testNoGuttersWithoutInject(): Unit = {
    configureFromFileText(Model + "class Plain(repo: UserRepo)\n")
    assertTrue(gutters("Guice binding for UserRepo").isEmpty)
  }
}

/** E2: "Unbound Guice injection". */
class UnboundInjectionTest extends PlayInspectionTestBase[UnboundInjectionInspection] {
  import GuiceFixtures.*

  override protected val description = "Guice"
  override protected def descriptionMatches(s: String): Boolean =
    s != null && (s.startsWith("No Guice binding found") || s.contains("has no @Inject constructor"))

  def testUnboundTraitAndAbstractClassAreReported(): Unit =
    checkTextHasError(
      Model + s"class C @Inject() (a: ${START}Unbound$END, b: ${START}AbstractUnbound$END, c: ${START}Provider[Unbound]$END)\n"
    )

  def testConcreteClassWithoutInjectableConstructorIsReported(): Unit =
    checkTextHasError(Model + s"class C @Inject() (a: ${START}NeedsArgs$END, b: NoArgs, c: UserRepoImpl)\n")

  def testBoundTypesAreFine(): Unit =
    checkTextHasNoErrors(
      Model + "class C @Inject() (a: UserRepo, b: Mailer, c: Clock, d: Cache, e: Audit, f: Provider[UserRepo])\n"
    )

  def testPlayPekkoGuiceTypesAreAllowListed(): Unit =
    checkTextHasNoErrors(
      Model +
        """class C @Inject() (
          |  cc: ControllerComponents,
          |  config: Configuration,
          |  env: Environment,
          |  system: ActorSystem,
          |  mat: org.apache.pekko.stream.Materializer,
          |  lifecycle: play.api.inject.ApplicationLifecycle,
          |  injector: com.google.inject.Injector,
          |  shutdown: org.apache.pekko.actor.CoordinatedShutdown,
          |  messages: play.api.i18n.MessagesApi
          |)(implicit ec: ExecutionContext)
          |""".stripMargin
    )

  def testCatsEffectDispatcherNeedsABinding(): Unit = {
    val code = Model.replace("import scala.concurrent.ExecutionContext", "import scala.concurrent.ExecutionContext\nimport cats.effect.IO\nimport cats.effect.std.Dispatcher")
    checkTextHasError(code + s"class C @Inject() (d: ${START}Dispatcher[IO]$END)\n")
  }

  def testDispatcherBoundViaProvidesIsFine(): Unit = {
    val code = Model
      .replace("import scala.concurrent.ExecutionContext", "import scala.concurrent.ExecutionContext\nimport cats.effect.IO\nimport cats.effect.std.Dispatcher")
      .replace("@Provides def mailer(): Mailer = new SmtpMailer", "@Provides def mailer(): Mailer = new SmtpMailer\n  @Provides def dispatcher(): Dispatcher[IO] = ???")
    checkTextHasNoErrors(code + "class C @Inject() (d: Dispatcher[IO])\n")
  }

  /** `install(new ThirdPartyModule)` style: a library class used inside a project module vouches for its whole jar. */
  def testLibraryJarReferencedFromAModuleIsPresumedBound(): Unit = {
    val code = Model
      .replace("import scala.concurrent.ExecutionContext", "import scala.concurrent.ExecutionContext\nimport cats.effect.IO\nimport cats.effect.std.{Dispatcher, Supervisor}")
      .replace("@Provides def mailer(): Mailer = new SmtpMailer", "@Provides def mailer(): Mailer = new SmtpMailer\n  @Provides def supervisor(): Supervisor[IO] = ???")
    checkTextHasNoErrors(code + "class C @Inject() (d: Dispatcher[IO])\n")
  }

  def testClassesWithoutInjectAreNotChecked(): Unit =
    checkTextHasNoErrors(Model + "class C(a: Unbound)\n")

  def testCompileTimeDiProjectsAreSkipped(): Unit =
    checkTextHasNoErrors(
      Model +
        """class AppComponents(ctx: play.api.ApplicationLoader.Context) extends play.api.BuiltInComponentsFromContext(ctx)
          |class C @Inject() (a: Unbound)
          |""".stripMargin
    )
}
