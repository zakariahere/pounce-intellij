package pounce.visual

import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.junit.Assert.*
import pounce.visual.endpoints.Endpoints

/** http4s / tapir code here is parsed only (no http4s / tapir jars needed: the scanners are syntactic). */
class EndpointsTest extends VisualTestBase {

  def testParseRoutesFile(): Unit = {
    val routes =
      """# Users
        |GET     /users/:id          controllers.UserController.show(id: Long)
        |+ nocsrf
        |POST    /users              @controllers.UserController.create
        |->      /api                api.Routes
        |
        |not a route
        |""".stripMargin
    val parsed = Endpoints.parseRoutes(routes)
    assertEquals(
      Seq(
        ("GET", "/users/:id", "controllers.UserController.show(id: Long)"),
        ("POST", "/users", "controllers.UserController.create"),
        ("->", "/api", "api.Routes")
      ),
      parsed.map(r => (r.method, r.path, r.action))
    )
    assertEquals(routes.indexOf("GET"), parsed.head.offset)
  }

  def testHttp4sPathRendering(): Unit = {
    assertEquals("/users/{id: Int}", Endpoints.http4sPath("""Root / "users" / IntVar(id)"""))
    assertEquals("/search", Endpoints.http4sPath("""Root / "search" :? Q(q)"""))
    assertEquals("/", Endpoints.http4sPath("Root"))
    assertEquals("/files/{name}", Endpoints.http4sPath("""Root / "files" / name"""))
  }

  def testSplitCasePattern(): Unit = {
    assertEquals(Some(("GET", """Root / "users" / IntVar(id)""")), Endpoints.splitCasePattern("""GET -> Root / "users" / IntVar(id)"""))
    assertEquals(Some(("POST", """Root / "users"""")), Endpoints.splitCasePattern("""req @ POST -> Root / "users""""))
    assertEquals(Some(("GET", "Root / IntVar(id)")), Endpoints.splitCasePattern("r @ (GET -> Root / IntVar(id))"))
    assertEquals(None, Endpoints.splitCasePattern("""Some(x) -> y"""))
  }

  def testScanProject(): Unit = {
    myFixture.addFileToProject(
      "conf/routes",
      "GET  /users/:id  controllers.UserController.show(id: Long)\nPOST /users  controllers.UserController.create\n"
    )
    myFixture.addFileToProject(
      "controllers/UserController.scala",
      """package controllers
        |class UserController {
        |  def show(id: Long): String = id.toString
        |  def create: String = ""
        |}
        |""".stripMargin
    )
    myFixture.addFileToProject(
      "routes/UserRoutes.scala",
      """import org.http4s._
        |import org.http4s.dsl.io._
        |object UserRoutes {
        |  val routes = HttpRoutes.of[IO] {
        |    case GET -> Root / "users" / IntVar(id) => ???
        |    case req @ POST -> Root / "users" => ???
        |  }
        |  def notRoutes(x: Any) = x match { case GET -> Root / "nope" => 1 }
        |}
        |""".stripMargin
    )
    myFixture.addFileToProject(
      "api/Api.scala",
      """import sttp.tapir._
        |object Api {
        |  val base = endpoint.in("api")
        |  val getUser = base.get.in("users" / path[Int]("id"))
        |  val createUser = endpoint.post.in("users")
        |}
        |""".stripMargin
    )
    configureFromFileText("object Empty")
    val found = Endpoints.scan(getProject).map(e => (e.method, e.path, e.framework))
    val expected = Seq(
      ("GET", "/users/:id", "Play"),
      ("POST", "/users", "Play"),
      ("GET", "/users/{id: Int}", "http4s"),
      ("POST", "/users", "http4s"),
      ("GET", "/api/users/{id: Int}", "tapir"),
      ("POST", "/users", "tapir")
    )
    expected.foreach(e => assertTrue(s"$e not in $found", found.contains(e)))
    assertFalse(found.toString, found.exists(_._2 == "/nope"))
    assertFalse("the base endpoint has no method", found.exists(_._2 == "/api"))
    assertEquals(found.toString, expected.size, found.size)

    val show = Endpoints.scan(getProject).find(e => e.framework == "Play" && e.method == "GET").get
    assertEquals("show", show.target(getProject).map(_.asInstanceOf[com.intellij.psi.PsiNamedElement].getName).orNull)

    locationsAreComputedDuringTheScan()
  }

  /**
   * The tool window paints `location` on the EDT, which holds no read lock in 2026.2: reading a document there tripped
   * "Read access is allowed from inside read-action only" on every repaint. Evaluate it on a pooled thread with no
   * read action, where any document/PSI access fails the same assertion.
   */
  private def locationsAreComputedDuringTheScan(): Unit = {
    val scanned = Endpoints.scan(getProject)
    val locations = com.intellij.openapi.application.ApplicationManager.getApplication
      .executeOnPooledThread[Seq[(String, String, String)]](() => scanned.map(e => (e.framework, e.method, e.location)))
      .get(30, java.util.concurrent.TimeUnit.SECONDS)
    val expected = Seq(
      ("Play", "GET", "routes:1"),
      ("Play", "POST", "routes:2"),
      ("http4s", "GET", "UserRoutes.scala:5"),
      ("http4s", "POST", "UserRoutes.scala:6"),
      ("tapir", "GET", "Api.scala:4"),
      ("tapir", "POST", "Api.scala:5")
    )
    expected.foreach(e => assertTrue(s"$e not in $locations", locations.contains(e)))
  }

  def testNoEndpointsInPlainCode(): Unit = {
    configureFromFileText(
      """object Plain {
        |  val endpoint = "http://example.com"
        |  def m(x: Any) = x match { case "GET" -> y => y }
        |}
        |""".stripMargin
    )
    assertEquals(Nil, Endpoints.http4sIn(getFile.asInstanceOf[ScalaFile]).toList)
    assertEquals(Nil, Endpoints.scan(getProject).toList)
  }
}
