package pounce.core

import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.junit.Assert.*
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, OpKind}
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.testing.PounceFixtureTestBase

import scala.jdk.CollectionConverters.*

/** Validates the type bridge, bounds reader and instance oracle against the real Scala plugin + cats jars. */
class OracleFixtureTest extends PounceFixtureTestBase {

  private def refNamed(name: String): ScReferenceExpression = {
    val refs = PsiTreeUtil.findChildrenOfType(getFile, classOf[ScReferenceExpression]).asScala
    refs.find(_.refName == name).getOrElse(fail(s"no reference $name").asInstanceOf[Nothing])
  }

  private def tyOf(name: String): Ty = {
    val ref = refNamed(name)
    ref.`type`() match {
      case Right(t) => ScTypeBridge.toTy(t, ref)
      case Left(f)  => fail(s"no type for $name: $f (resolves to ${ref.resolve()})").asInstanceOf[Nothing]
    }
  }

  def testBridgeConvertsStdAndCatsTypes(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |import cats.data.{EitherT, ValidatedNel}
        |object T {
        |  val xs: List[Either[String, Int]] = Nil
        |  val io: IO[Int] = IO.pure(1)
        |  val v: ValidatedNel[String, Int] = ???
        |  val f: Int => IO[String] = ???
        |  val t: (IO[Int], IO[String]) = ???
        |  def use = (xs, io, v, f, t)
        |}
        |""".stripMargin
    )
    val xs = tyOf("xs")
    assertEquals("List[Either[String, Int]]", xs.show)
    assertEquals(Some("scala.collection.immutable.List"), xs.headFqn.map(Ty.canonicalFqn))
    assertEquals(Some("cats.effect.IO"), tyOf("io").headFqn)
    // aliases are expanded for lookups
    assertEquals(Some("cats.data.Validated"), tyOf("v").headFqn)
    assertTrue(tyOf("f").isInstanceOf[Ty.Fun])
    assertTrue(tyOf("t").isInstanceOf[Ty.Tuple])
  }

  def testBoundsInTaglessCode(): Unit = {
    configureFromFileText(
      """import cats.Monad
        |import cats.effect.Sync
        |class Repo[F[_]: Sync] {
        |  def find[G[_]](id: Int)(implicit G: Monad[G]): G[Int] = {
        |    val marker = 1
        |    ???
        |  }
        |}
        |""".stripMargin
    )
    val place  = refNamed("???")
    val bounds = Bounds.at(place)
    assertEquals(Set("Sync"), bounds.getOrElse("F", Set.empty))
    assertEquals(Set("Monad"), bounds.getOrElse("G", Set.empty))
    val oracle = InstanceOracle(place)
    assertEquals(Answer.Yes, oracle.has("FlatMap", Ty.Lambda("F", Nil, rigid = true)))
    assertEquals(Answer.No, oracle.has("Traverse", Ty.Lambda("G", Nil, rigid = true)))
  }

  def testApplicabilityTraverseOnList(): Unit = {
    configureFromFileText(
      """object T {
        |  val ids: List[Int] = Nil
        |  def use = ids
        |}
        |""".stripMargin
    )
    val ref     = refNamed("ids")
    val recv    = tyOf("ids")
    val results = Applicability.applicable(Catalog.bundled.ops, recv, ref, Set(OpKind.Syntax))
    val names   = results.map(_.op.name)
    assertTrue(names.toString, names.contains("traverse"))
    assertTrue(names.toString, names.contains("foldMap"))
    assertFalse("sequence needs F[G[A]]", names.contains("sequence"))
    assertFalse("IO-specific syntax must not appear on List", names.contains("rethrow"))
  }

  def testApplicabilityRespectsMissingInstances(): Unit = {
    configureFromFileText(
      """import cats.effect.IO
        |object T {
        |  val io: IO[Int] = IO.pure(1)
        |  def use = io
        |}
        |""".stripMargin
    )
    val ref   = refNamed("io")
    val names = Applicability.applicable(Catalog.bundled.ops, tyOf("io"), ref, Set(OpKind.Syntax)).map(_.op.name)
    assertTrue(names.contains("void"))
    assertTrue(names.contains("attempt"))
    assertFalse("IO is not Traverse", names.contains("traverse"))
  }

  def testImplicitSearchFallbackForUserTypes(): Unit = {
    configureFromFileText(
      """import cats.Show
        |final case class User(name: String)
        |object User { implicit val show: Show[User] = Show.show(_.name) }
        |final case class NoShow(x: Int)
        |object T { def use = 1 }
        |""".stripMargin
    )
    val place  = getFile.findElementAt(getFile.getText.indexOf("def use"))
    val oracle = InstanceOracle(place)
    assertEquals(Answer.Yes, oracle.has("Show", Ty.Con("User", Nil, "User")))
    assertEquals(Answer.No, oracle.has("Show", Ty.Con("NoShow", Nil, "NoShow")))
  }
}
