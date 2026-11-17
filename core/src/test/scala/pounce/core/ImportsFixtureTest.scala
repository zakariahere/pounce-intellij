package pounce.core

import com.intellij.openapi.command.WriteCommandAction
import org.junit.Assert.*
import pounce.core.catalog.Catalog
import pounce.testing.PounceFixtureTestBase

/** Import insertion follows the file's existing style. */
class ImportsFixtureTest extends PounceFixtureTestBase {

  private def addImportFor(opName: String, owner: String, fileText: String): String = {
    configureFromFileText(fileText)
    val place = getFile.findElementAt(getFile.getText.lastIndexOf("ids"))
    val op    = Catalog.bundled.find(opName, owner).getOrElse(fail(s"no op $owner.$opName").asInstanceOf[Nothing])
    WriteCommandAction.runWriteCommandAction(getProject, (() => Imports.ensureImportFor(op, place)): Runnable)
    getFile.getText
  }

  def testGranularStyleIsKept(): Unit = {
    val text = addImportFor(
      "traverse",
      "Traverse",
      """import cats.syntax.functor._
        |
        |object T {
        |  val ids: List[Int] = Nil
        |  def use = ids
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("import cats.syntax.traverse._"))
    assertFalse(text, text.contains("cats.syntax.all"))
  }

  def testImplicitsStyleIsKept(): Unit = {
    // cats.implicits._ already provides traverse: nothing must be added
    val before =
      """import cats.implicits._
        |
        |object T {
        |  val ids: List[Int] = Nil
        |  def use = ids
        |}
        |""".stripMargin
    assertEquals(before, addImportFor("traverse", "Traverse", before))
  }

  def testFirstImportGetsOneBlankLine(): Unit = {
    val text = addImportFor(
      "traverse",
      "Traverse",
      """package demo
        |
        |object T {
        |  val ids: List[Int] = Nil
        |  def use = ids
        |}
        |""".stripMargin
    )
    assertTrue(text, text.contains("import cats.syntax.all._") || text.contains("import cats.implicits._"))
    assertFalse("more than one blank line after the new import:\n" + text, text.contains("\n\n\n"))
  }
}
