package pounce.testing

import com.intellij.codeInspection.LocalInspectionTool
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionTestBase

import scala.reflect.ClassTag

/**
 * Libraries for Pounce fixture tests: real cats-core and cats-effect jars resolved through Ivy (cached after the
 * first run). Pillars add more via [[extraLibraries]] (fs2, play, http4s, ...).
 */
trait PounceLibraries { this: ScalaLightCodeInsightFixtureTestCase =>

  protected def catsVersion: String       = "2.13.0"
  protected def catsEffectVersion: String = "3.6.3"

  /** `"org" %% "name" % "ver"` needs the test's Scala version in scope. */
  protected def pounceLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    Seq(
      // IvyManagedLoader is not transitive: list the kernel modules too, or `cats.kernel.Monoid`,
      // `cats.effect.kernel.Sync`, `cats.effect.std.Console`, ... don't resolve.
      IvyManagedLoader(
        "org.typelevel" %% "cats-core"          % catsVersion,
        "org.typelevel" %% "cats-kernel"        % catsVersion,
        "org.typelevel" %% "cats-effect"        % catsEffectVersion,
        "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion,
        "org.typelevel" %% "cats-effect-std"    % catsEffectVersion
      )
    )
  }

  protected def extraLibraries: Seq[LibraryLoader] = Seq.empty
}

/** A light Scala 2.13 project with cats + cats-effect. */
abstract class PounceFixtureTestBase extends ScalaLightCodeInsightFixtureTestCase with PounceLibraries {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)

  override protected def additionalLibraries: Seq[LibraryLoader] = pounceLibraries ++ extraLibraries

  /** The vendored testkit defaults this to false (zio-local); our Ivy loaders are not transitive, so keep the stdlib. */
  override protected def includeScalaLibraryFilesInSdk: Boolean = true
}

/**
 * Inspection tests: highlight ranges with `START`/`END` markers, check messages and quick fixes.
 *
 * {{{
 * class VoidTest extends PounceInspectionTestBase[SimplifyVoidInspection] {
 *   override protected val description = "Replace with .void"
 *   def testHighlight(): Unit = checkTextHasError(s"io.$START${"map(_ => ())"}$END")
 * }
 * }}}
 */
abstract class PounceInspectionTestBase[T <: LocalInspectionTool: ClassTag] extends ScalaInspectionTestBase with PounceLibraries {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(ScalaVersion.Latest.Scala_2_13)

  override protected def additionalLibraries: Seq[LibraryLoader] = pounceLibraries ++ extraLibraries

  /** The vendored testkit defaults this to false (zio-local); our Ivy loaders are not transitive, so keep the stdlib. */
  override protected def includeScalaLibraryFilesInSdk: Boolean = true

  final override protected val classOfInspection: Class[? <: LocalInspectionTool] =
    summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[? <: LocalInspectionTool]]

  /** Wraps a snippet in an object with common imports and helpers. */
  protected def code(body: String, imports: String = PounceInspectionTestBase.DefaultImports): String =
    s"""$imports
       |
       |object Test {
       |  final case class User(id: Int, name: String)
       |  def findUser(id: Int): IO[User] = IO.pure(User(id, "a"))
       |  def save(u: User): IO[Unit] = IO.unit
       |  val io: IO[Int] = IO.pure(1)
       |  val ids: List[Int] = List(1, 2, 3)
       |
       |  def run = {
       |${body.linesIterator.map("    " + _).mkString("\n")}
       |  }
       |}
       |""".stripMargin
}

object PounceInspectionTestBase {
  val DefaultImports: String =
    """import cats._
      |import cats.syntax.all._
      |import cats.effect._""".stripMargin
}
