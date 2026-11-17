package pounce.completion

import com.intellij.codeInsight.CodeInsightSettings
import com.intellij.codeInsight.completion.CompletionType
import com.intellij.codeInsight.lookup.{Lookup, LookupElement, LookupElementPresentation}
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert.*
import pounce.core.settings.PounceSettings
import pounce.testing.PounceFixtureTestBase

import scala.jdk.CollectionConverters.*

/** Completion fixture: cats + cats-effect (with kernel/std, the loader is not transitive), helpers for lookups. */
abstract class CompletionTestBase extends PounceFixtureTestBase {

  // cats-kernel / cats-effect-kernel / cats-effect-std now come from PounceFixtureTestBase.

  private var savedAutocomplete      = true
  private var savedSmartAutocomplete = true
  private var savedSettings: PounceSettings = scala.compiletime.uninitialized

  override protected def setUp(): Unit = {
    super.setUp()
    val cis = CodeInsightSettings.getInstance
    savedAutocomplete = cis.AUTOCOMPLETE_ON_CODE_COMPLETION
    savedSmartAutocomplete = cis.AUTOCOMPLETE_ON_SMART_TYPE_COMPLETION
    cis.AUTOCOMPLETE_ON_CODE_COMPLETION = false
    cis.AUTOCOMPLETE_ON_SMART_TYPE_COMPLETION = false
    savedSettings = new PounceSettings
    savedSettings.loadState(PounceSettings())
  }

  override protected def tearDown(): Unit = {
    try {
      val cis = CodeInsightSettings.getInstance
      cis.AUTOCOMPLETE_ON_CODE_COMPLETION = savedAutocomplete
      cis.AUTOCOMPLETE_ON_SMART_TYPE_COMPLETION = savedSmartAutocomplete
      if (savedSettings != null) PounceSettings().loadState(savedSettings)
    } finally super.tearDown()
  }

  /** Configures the file and runs basic completion; returns the lookup strings (in lookup order). */
  protected def complete(text: String, kind: CompletionType = CompletionType.BASIC): Seq[String] = {
    configureFromFileText(text)
    val elements = myFixture.complete(kind)
    if (elements == null) Seq.empty
    else Option(myFixture.getLookupElementStrings).map(_.asScala.toSeq).getOrElse(Seq.empty)
  }

  protected def lookupElements: Seq[LookupElement] =
    Option(myFixture.getLookupElements).map(_.toSeq).getOrElse(Seq.empty)

  protected def element(name: String): LookupElement =
    lookupElements.find(_.getLookupString == name).getOrElse(fail(s"no lookup element '$name' in ${lookupElements.map(_.getLookupString)}").asInstanceOf[Nothing])

  protected def pounceElement(name: String): LookupElement =
    lookupElements.find(e => e.getLookupString == name && LensLookup.itemOf(e).isDefined)
      .getOrElse(fail(s"no Pounce lookup element '$name' in ${lookupElements.map(_.getLookupString)}").asInstanceOf[Nothing])

  protected def presentation(e: LookupElement): LookupElementPresentation = LookupElementPresentation.renderElement(e)

  /** Selects the element and finishes the lookup as if Enter was pressed. */
  protected def select(e: LookupElement): Unit = {
    myFixture.getLookup.setCurrentItem(e)
    myFixture.finishLookup(Lookup.NORMAL_SELECT_CHAR)
  }

  protected def selectPounce(name: String): Unit = select(pounceElement(name))

  protected def assertContains(names: Seq[String], expected: String*): Unit =
    expected.foreach(e => assertTrue(s"expected '$e' in $names", names.contains(e)))

  protected def assertNotContains(names: Seq[String], unexpected: String*): Unit =
    unexpected.foreach(e => assertFalse(s"did not expect '$e' in $names", names.contains(e)))

  protected def pounceNames: Seq[String] = lookupElements.filter(e => LensLookup.itemOf(e).isDefined).map(_.getLookupString)

  /**
   * Compares the editor text (with a `<caret>` marker) to `expected`. Runs of blank lines are collapsed on both sides:
   * the first import added to a file without imports is followed by two blank lines (core's `Imports`), which is
   * formatting, not completion behaviour.
   */
  protected def checkResult(expected: String): Unit = {
    val doc    = myFixture.getEditor.getDocument.getText
    val offset = myFixture.getCaretOffset
    val actual = doc.substring(0, offset) + "<caret>" + doc.substring(offset)
    def norm(s: String) = s.replace("\r", "").replaceAll("\n{3,}", "\n\n")
    assertEquals(norm(expected), norm(actual))
  }
}
