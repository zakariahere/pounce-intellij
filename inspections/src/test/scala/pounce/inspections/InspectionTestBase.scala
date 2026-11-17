package pounce.inspections

import com.intellij.codeInspection.LocalInspectionTool
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import pounce.testing.{PounceFixtureTestBase, PounceInspectionTestBase}

import scala.reflect.ClassTag

/**
 * cats-effect 3 splits into kernel/std jars and cats-core needs cats-kernel; Ivy loading is not transitive, so
 * without these `Sync`, `Resource`, `Monoid`, ... don't resolve and tagless code goes red.
 */
object InspectionLibraries {
  def kernels(version: ScalaVersion): Seq[LibraryLoader] = {
    given ScalaVersion = version
    Seq(
      IvyManagedLoader(
        "org.typelevel" %% "cats-kernel"        % "2.13.0",
        "org.typelevel" %% "cats-effect-kernel" % "3.6.3",
        "org.typelevel" %% "cats-effect-std"    % "3.6.3"
      )
    )
  }
}

abstract class InspectionTestBase[T <: LocalInspectionTool: ClassTag] extends PounceInspectionTestBase[T] {

  override protected def extraLibraries: Seq[LibraryLoader] = InspectionLibraries.kernels(version) ++ moreLibraries

  protected def moreLibraries: Seq[LibraryLoader] = Nil

  /** Snippet wrapped with cats imports, an implicit IORuntime and the usual helpers. */
  protected def ce(body: String, imports: String = InspectionTestBase.Imports): String = code(body, imports)

  /** Re-throws assertion failures with all highlights in the file appended. */
  protected def withHighlightDump(body: => Unit): Unit =
    try body
    catch {
      case e: AssertionError =>
        val text = try getFile.getText catch { case _: Throwable => "<no file>" }
        val dump =
          try {
            import scala.jdk.CollectionConverters.*
            myFixture.doHighlighting().asScala.toSeq
              .filter(h => h.getDescription != null && h.getSeverity.myVal >= com.intellij.lang.annotation.HighlightSeverity.WEAK_WARNING.myVal)
              .filterNot(_.getDescription.startsWith("Unused import"))
              .map(h => s"  [${text.substring(h.getStartOffset, h.getEndOffset)}] ${h.getSeverity}: ${h.getDescription}")
              .mkString("\n")
          } catch { case _: Throwable => "  <no highlights available>" }
        throw new AssertionError(s"${e.getMessage}\nHighlights in file:\n$dump\nFile text now:\n$text", e)
    }

  /** A tagless-final snippet: `body` goes inside `def run[F[_]: <bounds>](fa: F[Int], fu: F[Unit])`. */
  protected def tagless(body: String, bounds: String = "Sync", imports: String = InspectionTestBase.Imports): String =
    s"""$imports
       |
       |object Test {
       |  def run[F[_]: $bounds](fa: F[Int], fu: F[Unit]) = {
       |${body.linesIterator.map("    " + _).mkString("\n")}
       |  }
       |}
       |""".stripMargin
}

/**
 * Inspection tests whose messages vary (they quote a type, a method name, ...): each assertion names the exact
 * message it expects; negative tests reject any message the inspection could produce ([[isOwnMessage]]). Failures
 * list every highlight in the file, which makes a wrong expected message easy to spot.
 */
abstract class MessageTestBase[T <: LocalInspectionTool: ClassTag] extends InspectionTestBase[T] {

  /** Recognises this inspection's messages (for negative tests). */
  protected def isOwnMessage(s: String): Boolean

  private var current: Option[String] = None

  override protected def description: String = current.getOrElse("<any message of the inspection>")

  override protected def descriptionMatches(s: String): Boolean = current.fold(isOwnMessage(s))(_ == s)

  protected def assertHighlight(message: String, text: String): Unit = {
    current = Some(message)
    withHighlightDump(checkTextHasError(text))
  }

  protected def assertFix(message: String, hint: String, before: String, after: String): Unit = {
    current = Some(message)
    withHighlightDump(testQuickFix(before, after, hint))
  }

  protected def assertNone(text: String): Unit = {
    current = None
    withHighlightDump(checkTextHasNoErrors(text))
  }

}

object InspectionTestBase {
  val Imports: String =
    """import cats._
      |import cats.syntax.all._
      |import cats.effect._""".stripMargin
}

/** Fixture tests (no single inspection under test) with the kernel jars. */
abstract class InspectionFixtureTestBase extends PounceFixtureTestBase {
  override protected def extraLibraries: Seq[LibraryLoader] = InspectionLibraries.kernels(version)
}
