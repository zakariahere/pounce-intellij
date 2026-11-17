package pounce.inspections.effects

import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import pounce.inspections.MessageTestBase

// ---------------------------------------------------------------------------------------------------------------
// B2 nested effect
// ---------------------------------------------------------------------------------------------------------------

class NestedEffectInspectionTest extends MessageTestBase[NestedEffectInspection] {
  import NestedEffectInspection.*

  override protected def isOwnMessage(s: String): Boolean =
    s.startsWith("Nested effect") || s.startsWith("Effects inside") || s == foreachMessage

  def testMapToSameEffect(): Unit = assertHighlight(nestedMessage("IO[IO[User]]"), ce(s"io.${START}map(findUser)$END"))
  def testMapToSameEffectFix(): Unit =
    assertFix(nestedMessage("IO[IO[User]]"), flatMapHint, ce("io.map(findUser)"), ce("io.flatMap(findUser)"))

  def testListOfEffectsDiscarded(): Unit = assertHighlight(
    traverseMessage("List[IO[Unit]]"),
    ce(s"""ids.${START}map(i => IO.println(i))$END
          |IO.unit""".stripMargin)
  )
  def testListOfEffectsFix(): Unit = assertFix(
    traverseMessage("List[IO[Unit]]"),
    traverseHint,
    ce("""ids.map(i => IO.println(i))
         |IO.unit""".stripMargin),
    ce("""ids.traverse(i => IO.println(i))
         |IO.unit""".stripMargin)
  )

  def testForeachEffect(): Unit = assertHighlight(foreachMessage, ce(s"ids.${START}foreach(i => IO.println(i))$END\nIO.unit"))
  def testForeachFix(): Unit =
    assertFix(foreachMessage, traverseUnderscoreHint, ce("ids.foreach(i => IO.println(i))\nIO.unit"), ce("ids.traverse_(i => IO.println(i))\nIO.unit"))

  // ---- negatives ----
  def testPlainMap(): Unit           = assertNone(ce("io.map(_ + 1)"))
  def testExpectedNested(): Unit     = assertNone(ce("val nested: IO[IO[Int]] = io.map(IO.pure(_))\nnested"))
  def testFlattenedRightAway(): Unit = assertNone(ce("io.map(findUser).flatten"))
  def testListKeptAsValue(): Unit    = assertNone(ce("val effects = ids.map(findUser)\neffects.sequence"))
  def testForeachSideEffect(): Unit  = assertNone(ce("ids.foreach(println)\nIO.unit"))
}

// ---------------------------------------------------------------------------------------------------------------
// B3 blocking call inside delay
// ---------------------------------------------------------------------------------------------------------------

class BlockingInDelayInspectionTest extends MessageTestBase[BlockingInDelayInspection] {
  import BlockingInDelayInspection.*

  override protected def isOwnMessage(s: String): Boolean = s.startsWith("Blocking call (")

  private val sleepMsg = message("Thread.sleep/join", "IO(...)")

  def testThreadSleep(): Unit = assertHighlight(sleepMsg, ce(s"IO(Thread.${START}sleep$END(10))"))
  def testBlockingFix(): Unit = assertFix(sleepMsg, "Replace with IO.blocking", ce("IO(Thread.sleep(10))"), ce("IO.blocking(Thread.sleep(10))"))
  def testInterruptibleFix(): Unit =
    assertFix(sleepMsg, "Replace with IO.interruptible", ce("IO(Thread.sleep(10))"), ce("IO.interruptible(Thread.sleep(10))"))
  def testSleepFixAddsDurationImport(): Unit = assertFix(
    sleepMsg,
    sleepHint,
    ce("IO(Thread.sleep(10))"),
    ce("IO.sleep(10.millis)", imports = "import cats._\nimport cats.syntax.all._\nimport cats.effect._\nimport scala.concurrent.duration._")
  )

  def testNioFiles(): Unit = assertHighlight(
    message("java.nio.file.Files", "IO.delay"),
    ce(s"""IO.delay(java.nio.file.Files.${START}readString$END(java.nio.file.Paths.get("a")))""")
  )

  def testTaglessSourceFix(): Unit = assertFix(
    message("scala.io.Source", "Sync[F].delay"),
    "Replace with Sync[F].blocking",
    tagless("""Sync[F].delay(scala.io.Source.fromFile("a").mkString)"""),
    tagless("""Sync[F].blocking(scala.io.Source.fromFile("a").mkString)""")
  )

  def testConstructor(): Unit = assertHighlight(
    message("new FileInputStream", "IO(...)"),
    ce(s"""IO(${START}new java.io.FileInputStream("a")$END)""")
  )

  /** `java.sql` isn't in the light-test JDK, so a `java.base` API stands in for the JDBC entries. */
  def testCountDownLatch(): Unit = assertHighlight(
    message("CountDownLatch.await", "IO(...)"),
    ce(s"""def run(l: java.util.concurrent.CountDownLatch) = IO(l.${START}await$END())
          |run(new java.util.concurrent.CountDownLatch(1))""".stripMargin)
  )

  // ---- negatives ----
  def testAlreadyBlocking(): Unit  = assertNone(ce("IO.blocking(Thread.sleep(10))"))
  def testInMemoryStream(): Unit   = assertNone(ce("IO(new java.io.ByteArrayInputStream(Array[Byte](1)).read())"))
  def testPrintlnIsFine(): Unit    = assertNone(ce("""IO(println("hi"))"""))
  def testOutsideDelay(): Unit     = assertNone(ce("Thread.sleep(10)\nIO.unit"))
  def testLocalDefNotScanned(): Unit = assertNone(ce("IO { def later() = Thread.sleep(1); 1 }"))
}

// ---------------------------------------------------------------------------------------------------------------
// B4 unsafeRun outside the edge of the world (Play controllers use real Play 3 jars)
// ---------------------------------------------------------------------------------------------------------------

object PlayJars {
  def loaders(using ScalaVersion): Seq[LibraryLoader] = Seq(
    IvyManagedLoader(
      "org.playframework" %% "play"               % "3.0.9",
      "org.playframework" %% "play-configuration" % "3.0.9",
      "org.playframework" %% "play-streams"       % "3.0.9",
      "org.apache.pekko"  %% "pekko-actor"        % "1.0.3",
      "org.apache.pekko"  %% "pekko-stream"       % "1.0.3",
      "com.typesafe"       % "config"             % "1.4.3",
      "javax.inject"       % "javax.inject"       % "1",
      "com.google.inject"  % "guice"              % "6.0.0"
    )
  )
}

class UnsafeRunOutsideEdgeInspectionTest extends MessageTestBase[UnsafeRunOutsideEdgeInspection] {
  import UnsafeRunOutsideEdgeInspection.*

  override protected def moreLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    PlayJars.loaders
  }

  override protected def isOwnMessage(s: String): Boolean = s.contains("outside the edge of the world")

  private val Header =
    """import cats.effect._
      |import cats.effect.unsafe.implicits.global
      |""".stripMargin

  def testServiceMethod(): Unit = assertHighlight(
    message("unsafeRunSync"),
    Header + s"""object Svc {
                |  def total: Int = IO.pure(1).${START}unsafeRunSync$END()
                |}""".stripMargin
  )

  def testToFutureInService(): Unit = assertHighlight(
    message("unsafeToFuture"),
    Header + s"""object Svc {
                |  def total = IO.pure(1).${START}unsafeToFuture$END()
                |}""".stripMargin
  )

  /** `Action.async` is exempt wherever it is used, not only in allow-listed controller classes. */
  def testActionBuilderOutsideController(): Unit = assertNone(
    Header + """import javax.inject.Inject
               |import play.api.mvc._
               |
               |class Routes @Inject()(action: DefaultActionBuilder) {
               |  def show = action.async(IO.pure(Results.Ok("x")).unsafeToFuture())
               |}""".stripMargin
  )

  /** Outside the bridge call, in a class that isn't allow-listed: reported. */
  def testNonControllerHelper(): Unit = assertHighlight(
    message("unsafeRunSync"),
    Header + s"""import javax.inject.Inject
                |import play.api.mvc._
                |
                |class Routes @Inject()(action: DefaultActionBuilder) {
                |  private def count: Int = IO.pure(1).${START}unsafeRunSync$END()
                |  def show = action(Results.Ok(count.toString))
                |}""".stripMargin
  )

  // ---- negatives: the top-priority false positive is Play 3's Action.async(io.unsafeToFuture()) ----

  def testPlayActionAsync(): Unit = assertNone(
    Header + """import javax.inject.Inject
               |import play.api.mvc._
               |
               |class UserController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
               |  def show(id: Int): Action[AnyContent] = Action.async {
               |    IO.pure(Ok(s"user $id")).unsafeToFuture()
               |  }
               |  def withRequest: Action[AnyContent] = Action.async { request =>
               |    IO(Ok(request.path)).unsafeToFuture()
               |  }
               |  def json = Action.async(parse.json) { request =>
               |    IO.pure(Ok(request.body.toString)).unsafeToFuture()
               |  }
               |}""".stripMargin
  )

  def testPlayBaseController(): Unit = assertNone(
    Header + """import javax.inject.Inject
               |import play.api.mvc._
               |
               |class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
               |  def index = Action.async(IO.pure(Ok("home")).unsafeToFuture())
               |}""".stripMargin
  )

  /** Guice modules run once at startup: building a Ref-backed service in a `@Provides` method is wiring. */
  def testGuiceModuleProvider(): Unit = assertNone(
    Header + """import com.google.inject.{AbstractModule, Provides}
               |
               |class AppModule extends AbstractModule {
               |  @Provides def counter: Ref[IO, Int] = Ref.of[IO, Int](0).unsafeRunSync()
               |}""".stripMargin
  )

  def testPlayModule(): Unit = assertNone(
    Header + """import play.api.{Configuration, Environment}
               |import play.api.inject.{Binding, Module}
               |
               |class EffectsModule extends Module {
               |  private val seed: Int = IO.pure(1).unsafeRunSync()
               |  def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq.empty
               |}""".stripMargin
  )

  def testMainMethod(): Unit = assertNone(
    Header + """object Main {
               |  def main(args: Array[String]): Unit = IO.println("hi").unsafeRunSync()
               |}""".stripMargin
  )
}

// ---------------------------------------------------------------------------------------------------------------
// B5 eager Future
// ---------------------------------------------------------------------------------------------------------------

class EagerFutureInspectionTest extends MessageTestBase[EagerFutureInspection] {
  import EagerFutureInspection.*

  override protected def isOwnMessage(s: String): Boolean =
    s == pureMessage || s == ignoredFutureMessage || s.contains("is a Future created outside the effect")

  private def fut(body: String) = ce(
    s"""import scala.concurrent.{ExecutionContext, Future}
       |implicit val ec: ExecutionContext = ExecutionContext.global
       |def call(): Future[Int] = Future(1)
       |val ready = true
       |$body""".stripMargin
  )

  def testPure(): Unit = assertHighlight(pureMessage, fut(s"IO.fromFuture(${START}IO.pure(call())$END)"))
  def testPureFix(): Unit =
    assertFix(pureMessage, suspendHint, fut("IO.fromFuture(IO.pure(call()))"), fut("IO.fromFuture(IO(call()))"))

  def testValCreatedOutside(): Unit = assertHighlight(
    alreadyRunningMessage("f"),
    fut(s"""val f = call()
           |IO.fromFuture(IO(${START}f$END))""".stripMargin)
  )
  def testMoveInsideFix(): Unit = assertFix(
    alreadyRunningMessage("f"),
    moveInsideHint,
    fut("""val f = call()
          |IO.fromFuture(IO(f))""".stripMargin),
    fut("IO.fromFuture(IO(call()))")
  )

  def testIgnoredFuture(): Unit = assertHighlight(ignoredFutureMessage, fut(s"${START}IO(call())$END"))
  def testIgnoredFutureFix(): Unit = assertFix(ignoredFutureMessage, wrapHint, fut("IO(call())"), fut("IO.fromFuture(IO(call()))"))

  // ---- negatives ----
  def testSuspended(): Unit       = assertNone(fut("IO.fromFuture(IO(call()))"))
  def testByNameParam(): Unit     = assertNone(fut("def lift(f: => Future[Int]) = IO.fromFuture(IO(f))\nlift(call())"))
  def testExpectedFuture(): Unit  = assertNone(fut("val io2: IO[Future[Int]] = IO(call())\nio2"))
}

// ---------------------------------------------------------------------------------------------------------------
// B6 fiber leak
// ---------------------------------------------------------------------------------------------------------------

class FiberLeakInspectionTest extends MessageTestBase[FiberLeakInspection] {
  import FiberLeakInspection.*

  override protected def isOwnMessage(s: String): Boolean = s == message

  def testStartThen(): Unit = assertHighlight(message, ce(s"${START}io.start$END >> IO.println(1)"))
  def testStartThenFix(): Unit =
    assertFix(message, backgroundHint, ce("io.start >> IO.println(1)"), ce("io.background.surround(IO.println(1))"))
  def testStartVoid(): Unit = assertHighlight(message, ce(s"${START}io.start$END.void"))
  def testWildcardGenerator(): Unit = assertHighlight(
    message,
    ce(s"""for {
          |  _ <- ${START}io.start$END
          |  u <- findUser(1)
          |} yield u""".stripMargin)
  )

  // ---- negatives ----
  def testJoined(): Unit = assertNone(
    ce("""for {
         |  fib <- io.start
         |  r   <- fib.join
         |} yield r""".stripMargin)
  )
  def testBackground(): Unit = assertNone(ce("io.background.surround(IO.println(1))"))
}

// ---------------------------------------------------------------------------------------------------------------
// B7 resource escaping use
// ---------------------------------------------------------------------------------------------------------------

class ResourceEscapesUseInspectionTest extends MessageTestBase[ResourceEscapesUseInspection] {
  import ResourceEscapesUseInspection.*

  override protected def isOwnMessage(s: String): Boolean = s == useMessage || s == allocatedMessage

  private def res(body: String) = ce(
    s"""final class Conn { def close(): Unit = () }
       |val pool: Resource[IO, Conn] = Resource.make(IO(new Conn))(c => IO(c.close()))
       |$body""".stripMargin
  )

  def testUsePure(): Unit   = assertHighlight(useMessage, res(s"pool.use(${START}IO.pure$END)"))
  def testUseLambda(): Unit = assertHighlight(useMessage, res(s"pool.use(${START}c => IO.pure(c)$END)"))
  def testUseSyntax(): Unit = assertHighlight(useMessage, res(s"pool.use(${START}_.pure[IO]$END)"))
  def testAllocated(): Unit = assertHighlight(allocatedMessage, res(s"pool.allocated.${START}map(_._1)$END"))

  // ---- negatives ----
  def testUseProperly(): Unit = assertNone(res("pool.use(c => IO(c.hashCode))"))
  def testPlainData(): Unit   = assertNone(res("Resource.pure[IO, Int](1).use(IO.pure)"))
}

// ---------------------------------------------------------------------------------------------------------------
// B10 mixed effects in for-comprehension
// ---------------------------------------------------------------------------------------------------------------

class MixedEffectsInForInspectionTest extends MessageTestBase[MixedEffectsInForInspection] {
  import MixedEffectsInForInspection.*

  override protected def isOwnMessage(s: String): Boolean = s.startsWith("Mixed effects in for-comprehension")

  private def withOpt(body: String) = ce(s"def nick(u: User): Option[String] = None\n$body")

  def testOptionInIO(): Unit = assertHighlight(
    message("Option[String]", "IO"),
    withOpt(s"""for {
               |  u <- findUser(1)
               |  n <- ${START}nick(u)$END
               |} yield n""".stripMargin)
  )
  def testOptionInIOFix(): Unit = assertFix(
    message("Option[String]", "IO"),
    "Lift with IO.fromOption",
    withOpt("""for {
              |  u <- findUser(1)
              |  n <- nick(u)
              |} yield n""".stripMargin),
    withOpt("""for {
              |  u <- findUser(1)
              |  n <- IO.fromOption(nick(u))(new NoSuchElementException("nick(u) is empty"))
              |} yield n""".stripMargin)
  )
  def testEitherInIOFix(): Unit = assertFix(
    message("Either[Throwable, Int]", "IO"),
    "Lift with IO.fromEither",
    ce("""val parsed: Either[Throwable, Int] = Right(1)
         |for {
         |  u <- findUser(1)
         |  n <- parsed
         |} yield n + u.id""".stripMargin),
    ce("""val parsed: Either[Throwable, Int] = Right(1)
         |for {
         |  u <- findUser(1)
         |  n <- IO.fromEither(parsed)
         |} yield n + u.id""".stripMargin)
  )

  // ---- negatives ----
  def testSameEffect(): Unit = assertNone(
    ce("""for {
         |  u <- findUser(1)
         |  _ <- save(u)
         |} yield u""".stripMargin)
  )
  def testPureCollections(): Unit = assertNone(ce("for { a <- ids; b <- List(a, a) } yield b"))
}
