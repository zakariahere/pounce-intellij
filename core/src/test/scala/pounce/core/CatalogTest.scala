package pounce.core

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.core.catalog.{Catalog, OpKind}

class CatalogTest extends TestCase {

  private lazy val catalog = Catalog.loadBundled(getClass.getClassLoader)

  def testBundledCatalogLoads(): Unit = {
    assertTrue(s"expected a substantial catalog, got ${catalog.ops.size}", catalog.ops.size >= 150)
  }

  def testEveryFileContributes(): Unit = {
    Seq("traverse" -> OpKind.Syntax, "some" -> OpKind.Lift, "mapN" -> OpKind.Tuple, "blocking" -> OpKind.Companion).foreach {
      case (name, kind) => assertTrue(s"$name/$kind missing", catalog.byName.getOrElse(name, Vector.empty).exists(_.kind == kind))
    }
  }

  def testIdsAreUnique(): Unit = {
    val dups = catalog.ops.groupBy(_.id).collect { case (id, ops) if ops.size > 1 => id }
    assertTrue(s"duplicate ids: ${dups.mkString(", ")}", dups.isEmpty)
  }

  def testExtensionsHaveReceiverAndSyntax(): Unit =
    catalog.extensions.foreach { op =>
      assertTrue(s"${op.id} has no receiver", op.receiver.isDefined)
      assertTrue(s"${op.id} has no syntax module", op.syntax.isDefined)
    }

  def testRequiresReferToKnownTypeclasses(): Unit =
    catalog.ops.foreach { op =>
      op.requires.foreach { case (_, tc) =>
        assertTrue(s"${op.id}: unknown typeclass $tc", Typeclasses.canonical(tc).isDefined)
      }
    }

  def testRequiresReferToShapeVariables(): Unit =
    catalog.ops.foreach { op =>
      val vars = (op.receiver.toList ++ op.params ++ op.result.toList).flatMap(_.vars).toSet
      op.requires.foreach { case (v, _) =>
        assertTrue(s"${op.id}: constraint on unknown variable $v (vars: $vars)", vars.contains(v))
      }
    }

  def testInsertTextCaret(): Unit = {
    val traverse = catalog.find("traverse", "Traverse").get
    assertEquals(("traverse()", "traverse(".length), traverse.insertTextAndCaret)
    val void = catalog.find("void", "Functor").get
    assertEquals(("void", 4), void.insertTextAndCaret)
  }

  def testInlineParsing(): Unit = {
    val c = Catalog.fromString(
      """library = cats-core
        |kind = syntax
        |ops = [ { name = foo, owner = Functor, syntax = functor, receiver = "F[A]", result = "F[A]", requires { F = Functor } } ]
        |""".stripMargin
    )
    assertEquals(1, c.ops.size)
    assertEquals(List("F" -> "Functor"), c.ops.head.requires)
  }

  /** Operator names contain `|`: only an explicit template's last `|` is the caret marker. */
  def testInsertTextCaretWithOperatorNames(): Unit = {
    val combine = catalog.find("|+|", "Semigroup").get // insert = "|+| |"
    assertEquals(("|+| ", 4), combine.insertTextAndCaret)
    val noTemplate = Catalog
      .fromString("""ops = [ { name = "|+|", owner = Semigroup, syntax = semigroup, receiver = "A", params = ["A"], result = "A", requires { A = Semigroup } } ]""")
      .ops
      .head
    assertEquals(("|+|()", 4), noTemplate.insertTextAndCaret)
  }

  /** `Spawn[F].start(fa)` only exists when F has Spawn: summoner entries get `F = owner` without spelling it out. */
  def testSummonersRequireTheirOwner(): Unit = summonersRequireTheirOwner()

  private def summonersRequireTheirOwner(): Unit = {
    val c = Catalog.fromString(
      """kind = companion
        |ops = [
        |  { name = start, owner = Spawn, variant = summoner, params = ["F[A]"], result = "F[Fiber[F, Throwable, A]]" }
        |  { name = raiseError, owner = ApplicativeError, variant = summoner, params = ["E"], result = "F[A]", requires { F = MonadThrow } }
        |  { name = of, owner = Ref, params = ["A"], result = "F[Ref[F, A]]" }
        |]""".stripMargin
    )
    val byName = c.ops.map(op => op.name -> op.requires).toMap
    assertEquals(List("F" -> "Spawn"), byName("start"))
    assertEquals("an explicit F constraint wins", List("F" -> "MonadThrow"), byName("raiseError"))
    assertEquals("not a summoner: nothing implied", Nil, byName("of"))

    val unconstrained = catalog.ops.filter(op => op.id.endsWith("#summoner") && !op.requires.exists(_._1 == "F"))
    assertTrue(s"summoners without an F constraint: ${unconstrained.map(_.id)}", unconstrained.isEmpty)
  }
}
