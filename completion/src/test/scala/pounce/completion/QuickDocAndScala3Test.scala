package pounce.completion

import com.intellij.codeInsight.completion.CompletionType
import junit.framework.TestCase
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.junit.Assert.*
import pounce.completion.doc.{LensDocTarget, LensDocumentation}
import pounce.core.catalog.Catalog

import com.typesafe.config.{ConfigFactory, ConfigParseOptions}

import scala.jdk.CollectionConverters.*

/** A7: quick documentation for Pounce lookup items. */
class QuickDocTest extends CompletionTestBase {

  private def docOf(name: String): String = {
    val e = lookupElements
      .find(e => e.getLookupString == name && (LensLookup.itemOf(e).isDefined || LensLookup.curatedOf(e).isDefined))
      .getOrElse(element(name))
    val target = new LensDocumentation().documentationTarget(myFixture.getFile, e, myFixture.getCaretOffset)
    assertNotNull(s"no documentation target for $name", target)
    assertNotNull(target.computeDocumentation())
    assertNotNull(target.createPointer().dereference())
    target.asInstanceOf[LensDocTarget].html
  }

  def testSyntaxItemDocExplainsWhyAndImport(): Unit = {
    complete(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    val html = docOf("traverse")
    assertTrue(html, html.contains("Traverse[List]"))
    assertTrue(html, html.contains("Why here"))
    assertTrue(html, html.contains("import cats.syntax.all._"))
    assertTrue(html, html.contains("Constraints"))
  }

  /**
   * Under `import cats.syntax.all._` the Scala plugin already offers `traverse`. Pounce keeps that element (no
   * duplicate) and explains it: the same tail text and quick documentation as its own items, without an import note.
   */
  def testImportedSyntaxIsExplained(): Unit = {
    val names = complete(
      """import cats.syntax.all._
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    assertEquals(s"traverse once in $names", 1, names.count(_ == "traverse"))
    val e = element("traverse")
    assertTrue("the Scala plugin's element is kept", LensLookup.itemOf(e).isEmpty)
    assertTrue("and explained by Pounce", LensLookup.explainedOf(e).isDefined)
    val tail = presentation(e).getTailText
    assertTrue(tail, tail.contains("Traverse[List] · cats"))
    val html = docOf("traverse")
    assertTrue(html, html.contains("Why here"))
    assertTrue(html, html.contains("Traverse[List]"))
    assertFalse("nothing to import", html.contains("added on insert"))
  }

  /** `NonEmptyList.traverse` is the type's own method, not cats syntax: it must not get the cats explanation. */
  def testNativeMethodIsNotExplained(): Unit = {
    complete(
      """import cats.data.NonEmptyList
        |import cats.syntax.all._
        |
        |object Test {
        |  val ids: NonEmptyList[Int] = NonEmptyList.of(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    val natives = lookupElements.filter(e => e.getLookupString == "traverse" && declaringClass(e).contains("cats.data.NonEmptyList"))
    assertFalse(s"no native traverse in ${lookupElements.map(e => e.getLookupString -> declaringClass(e))}", natives.isEmpty)
    natives.foreach(e => assertTrue(s"explained: ${presentation(e).getTailText}", LensLookup.explainedOf(e).isEmpty))
  }

  private def declaringClass(e: com.intellij.codeInsight.lookup.LookupElement): Option[String] =
    Option(e.as(classOf[org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem]))
      .flatMap(i => Option(i.getPsiElement: com.intellij.psi.PsiElement))
      .flatMap(p => Option(com.intellij.psi.util.PsiTreeUtil.getParentOfType(p, classOf[com.intellij.psi.PsiClass])))
      .flatMap(c => Option(c.getQualifiedName))

  def testTaglessDocNamesTheBound(): Unit = {
    complete(
      """import cats.Monad
        |
        |object Test {
        |  def run[F[_]: Monad](fa: F[Int]) = fa.flatT<caret>
        |}
        |""".stripMargin
    )
    val html = docOf("flatTap")
    assertTrue(html, html.contains("from the bound Monad on F"))
  }

  def testCuratedCompanionMemberDoc(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  def run = IO.<caret>
        |}
        |""".stripMargin
    )
    val html = docOf("blocking")
    assertTrue(html, html.contains("blocking pool"))
    assertTrue(html, html.contains("Example"))
  }

  def testSmartItemDoc(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  final case class User(name: String)
        |  def find(user: User): IO[Option[User]] = <caret>
        |}
        |""".stripMargin,
      CompletionType.SMART
    )
    val html = docOf("user.some.pure")
    assertTrue(html, html.contains("Applicative[IO]"))
  }

  def testNoDocForOrdinaryItems(): Unit = {
    complete(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.<caret>
        |}
        |""".stripMargin
    )
    val head = element("head")
    assertNull(new LensDocumentation().documentationTarget(myFixture.getFile, head, myFixture.getCaretOffset))
  }
}

/** Scala 3 sources get the `*` wildcard. */
class Scala3CompletionTest extends CompletionTestBase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_3)

  def testScala3ImportUsesStar(): Unit = {
    complete(
      """import cats.effect.IO
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.trav<caret>
        |}
        |""".stripMargin
    )
    selectPounce("traverse")
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all.*
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.traverse(<caret>)
        |}
        |""".stripMargin
    )
  }
}

/** Without cats on the classpath the Cats Lens stays out of the way. */
class NoCatsCompletionTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)

  override protected def includeScalaLibraryFilesInSdk: Boolean = true

  def testNothingWithoutCats(): Unit = {
    configureFromFileText(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.<caret>
        |}
        |""".stripMargin
    )
    val elements = Option(myFixture.complete(CompletionType.BASIC)).map(_.toSeq).getOrElse(Seq.empty)
    assertTrue(elements.forall(e => LensLookup.itemOf(e).isEmpty && LensLookup.curatedOf(e).isEmpty))
    assertFalse(elements.map(_.getLookupString).contains("traverse"))
  }
}

/** Every bundled catalog entry parses (the loader skips bad entries with a warning, which tests would not notice). */
class CatalogCoverageTest extends TestCase {

  def testEveryEntryParses(): Unit = {
    val cl    = classOf[Catalog].getClassLoader
    val index = ConfigFactory.parseResources(cl, "pounce/catalog/index.conf", ConfigParseOptions.defaults().setAllowMissing(false))
    val files = index.getStringList("files").asScala.toSeq
    assertTrue(files.size >= 12)
    files.foreach { f =>
      val config = ConfigFactory.parseResources(cl, s"pounce/catalog/$f", ConfigParseOptions.defaults().setAllowMissing(false))
      val raw    = config.getConfigList("ops").size
      val parsed = Catalog.fromConfig(config, f).ops.size
      assertEquals(s"entries of $f that failed to parse", raw, parsed)
    }
  }

  def testCatalogHasAtLeast300Ops(): Unit = {
    val n = Catalog.bundled.ops.size
    assertTrue(s"catalog has $n ops", n >= 300)
  }

  // The same invariants as core's CatalogTest, so they are checked whenever this pillar's tests run.

  def testIdsAreUnique(): Unit = {
    val dups = Catalog.bundled.ops.groupBy(_.id).collect { case (id, ops) if ops.size > 1 => id }
    assertTrue(s"duplicate ids: ${dups.mkString(", ")}", dups.isEmpty)
  }

  def testExtensionsHaveReceiverAndSyntax(): Unit =
    Catalog.bundled.extensions.foreach { op =>
      assertTrue(s"${op.id} has no receiver", op.receiver.isDefined)
      assertTrue(s"${op.id} has no syntax module", op.syntax.isDefined)
    }

  def testRequiresReferToKnownTypeclassesAndShapeVariables(): Unit =
    Catalog.bundled.ops.foreach { op =>
      val vars = (op.receiver.toList ++ op.params ++ op.result.toList).flatMap(_.vars).toSet
      op.requires.foreach { case (v, tc) =>
        assertTrue(s"${op.id}: unknown typeclass $tc", pounce.core.Typeclasses.canonical(tc).isDefined)
        assertTrue(s"${op.id}: constraint on unknown variable $v (vars: $vars)", vars.contains(v))
      }
    }

  /** `|+|` is the exception: its name contains the marker (completion inserts symbolic operators itself). */
  def testEveryInsertTextHasAtMostOneCaret(): Unit =
    Catalog.bundled.ops.filterNot(_.name.contains('|')).foreach { op =>
      assertTrue(op.id, op.insert.forall(_.count(_ == '|') <= 1))
    }
}
