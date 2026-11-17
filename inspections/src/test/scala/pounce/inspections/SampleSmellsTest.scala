package pounce.inspections

import com.intellij.codeInspection.LocalInspectionTool
import com.intellij.lang.annotation.HighlightSeverity
import org.junit.Assert.*

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/**
 * `samples/play3-cats/app/playground/Smells.scala` is the showcase the sample README (and the website) points people
 * to: each method must trip its inspection. It once drifted: `nested` declared `IO[IO[Int]]` and `escaped` used a
 * `Resource[IO, String]`, both of which the inspections rightly accept. This test highlights the real file with every
 * Pounce inspection enabled, as the IDE does.
 */
class SampleSmellsTest extends InspectionFixtureTestBase {
  import SampleSmellsTest.*

  def testEverySmellIsFlaggedOnItsLine(): Unit = everySmellIsFlagged()

  private def everySmellIsFlagged(): Unit = {
    myFixture.enableInspections(allPounceInspections()*)
    val text = smellsSource()
    myFixture.configureByText("Smells.scala", text)
    val highlights = myFixture.doHighlighting().asScala.toSeq
      .filter(h => h.getDescription != null && h.getSeverity.myVal >= HighlightSeverity.WEAK_WARNING.myVal)
    val lines = text.split("\n", -1).toIndexedSeq
    def lineOf(offset: Int): Int = text.substring(0, offset).count(_ == '\n')

    val missing = Expected.flatMap { case (method, prefix) =>
      val line = lines.indexWhere(_.contains(s"def $method"))
      assertTrue(s"no `def $method` in Smells.scala", line >= 0)
      Option.unless(highlights.exists(h => lineOf(h.getStartOffset) == line && h.getDescription.startsWith(prefix)))(
        s"$method (line ${line + 1}): expected '$prefix…'"
      )
    }
    val seen = highlights.map(h => s"  line ${lineOf(h.getStartOffset) + 1}: ${h.getDescription}").mkString("\n")
    assertTrue(s"Not flagged:\n${missing.mkString("\n")}\nHighlights:\n$seen", missing.isEmpty)
  }

  /** Every `implementationClass` registered in pounce-inspections.xml. */
  private def allPounceInspections(): Seq[LocalInspectionTool] = {
    val xml = new String(getClass.getClassLoader.getResourceAsStream("META-INF/pounce-inspections.xml").readAllBytes(), StandardCharsets.UTF_8)
    val classes = """implementationClass="([^"]+)"""".r.findAllMatchIn(xml).map(_.group(1)).toSeq
    assertTrue(s"too few inspections: $classes", classes.size >= 28)
    classes.map(c => Class.forName(c).getDeclaredConstructor().newInstance().asInstanceOf[LocalInspectionTool])
  }
}

object SampleSmellsTest {

  /** Method in Smells.scala → the start of the message expected on its line. */
  val Expected: Seq[(String, String)] = Seq(
    "discarded" -> "Discarded effect",
    "nested"    -> "Nested effect",
    "blocking"  -> "Blocking call (",
    "edge"      -> "unsafeRunSync outside the edge",
    "eager"     -> "'running' is a Future created outside the effect",
    "leak"      -> "Fiber started with .start",
    "escaped"   -> "The acquired value escapes Resource.use",
    "simplify"  -> "fa.map(_ => ()) can be replaced with fa.void"
  )

  def smellsSource(): String = {
    val start = new File(System.getProperty("user.dir")).getAbsoluteFile
    val file = Iterator
      .iterate(start)(_.getParentFile)
      .takeWhile(_ != null)
      .map(new File(_, "samples/play3-cats/app/playground/Smells.scala"))
      .find(_.isFile)
      .getOrElse(throw new AssertionError(s"samples/play3-cats/app/playground/Smells.scala not found above $start"))
    new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8).replace("\r\n", "\n")
  }
}
