package pounce.completion

import com.intellij.codeInsight.template.impl.TemplateManagerImpl
import org.junit.Assert.*

/** A6: postfix templates, expanded with Tab. */
class PostfixTemplateTest extends CompletionTestBase {

  override protected def setUp(): Unit = {
    super.setUp()
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable)
  }

  private def expand(text: String): String = {
    configureFromFileText(text)
    myFixture.`type`('\t')
    myFixture.getFile.getText
  }

  def testIoWrapsAndImports(): Unit = {
    expand(
      """object Test {
        |  def compute(): Int = 1
        |  def run = compute().io<caret>
        |}
        |""".stripMargin
    )
    checkResult(
      """import cats.effect.IO
        |
        |object Test {
        |  def compute(): Int = 1
        |  def run = IO(compute())<caret>
        |}
        |""".stripMargin
    )
  }

  def testIoNotOfferedOnAnEffect(): Unit = {
    val text = expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.io<caret>
        |}
        |""".stripMargin
    )
    assertFalse(text, text.contains("IO(io)"))
  }

  def testDelayUsesTheSyncBound(): Unit = {
    expand(
      """import cats.effect.Sync
        |
        |object Test {
        |  def sideEffect(n: Int): Int = n
        |  def run[F[_]: Sync](n: Int): F[Int] = sideEffect(n).delay<caret>
        |}
        |""".stripMargin
    )
    checkResult(
      """import cats.effect.Sync
        |
        |object Test {
        |  def sideEffect(n: Int): Int = n
        |  def run[F[_]: Sync](n: Int): F[Int] = Sync[F].delay(sideEffect(n))<caret>
        |}
        |""".stripMargin
    )
  }

  def testDelayNeedsASyncBound(): Unit = {
    val text = expand(
      """object Test {
        |  def sideEffect(n: Int): Int = n
        |  def run[F[_]](n: Int) = sideEffect(n).delay<caret>
        |}
        |""".stripMargin
    )
    assertFalse(text, text.contains("Sync["))
  }

  def testBlockingFallsBackToIo(): Unit = {
    val text = expand(
      """object Test {
        |  def read(): String = "x"
        |  def run = read().blocking<caret>
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("IO.blocking(read())"))
    assertTrue(text, text.contains("import cats.effect.IO"))
  }

  def testLiftFOnAnEffect(): Unit = {
    val text = expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.liftF<caret>
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("EitherT.liftF(io)"))
    assertTrue(text, text.contains("import cats.data.EitherT"))
  }

  def testLiftFNotOfferedOnAPlainValue(): Unit = {
    val text = expand(
      """object Test {
        |  val n: Int = 1
        |  def run = n.liftF<caret>
        |}
        |""".stripMargin
    )
    assertFalse(text, text.contains("EitherT"))
  }

  def testEvalBuildsAResource(): Unit = {
    val text = expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.eval<caret>
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("Resource.eval(io)"))
    assertTrue(text, text.contains("import cats.effect.Resource") || text.contains("import cats.effect.{IO, Resource}"))
  }

  def testTraverseOnAList(): Unit = {
    expand(
      """object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.traverse<caret>
        |}
        |""".stripMargin
    )
    checkResult(
      """import cats.syntax.all._
        |
        |object Test {
        |  val ids: List[Int] = List(1, 2)
        |  def run = ids.traverse(a => <caret>)
        |}
        |""".stripMargin
    )
  }

  def testTraverseNotOfferedOnIo(): Unit = {
    val text = expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.traverse<caret>
        |}
        |""".stripMargin
    )
    assertFalse(text, text.contains("traverse(a =>"))
  }

  def testVoidAndImport(): Unit = {
    expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.void<caret>
        |}
        |""".stripMargin
    )
    checkResult(
      """import cats.effect.IO
        |import cats.syntax.all._
        |
        |object Test {
        |  val io: IO[Int] = IO.pure(1)
        |  def run = io.void<caret>
        |}
        |""".stripMargin
    )
  }

  def testSome(): Unit = {
    expand(
      """object Test {
        |  val n: Int = 1
        |  def run = n.some<caret>
        |}
        |""".stripMargin
    )
    checkResult(
      """import cats.syntax.all._
        |
        |object Test {
        |  val n: Int = 1
        |  def run = n.some<caret>
        |}
        |""".stripMargin
    )
  }

  def testPureTakesTheEffectFromTheExpectedType(): Unit = {
    val text = expand(
      """import cats.effect.IO
        |
        |object Test {
        |  val n: Int = 1
        |  def run: IO[Int] = n.pure<caret>
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("n.pure[IO]"))
    assertTrue(text, text.contains("import cats.syntax.all._"))
  }

  def testInfixReceiverIsParenthesised(): Unit = {
    val text = expand(
      """object Test {
        |  val n: Int = 1
        |  def run: Option[Int] = (n + 1).some<caret>
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("(n + 1).some"))
  }
}
