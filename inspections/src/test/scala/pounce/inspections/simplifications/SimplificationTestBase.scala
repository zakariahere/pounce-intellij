package pounce.inspections.simplifications

import pounce.inspections.InspectionTestBase

import scala.reflect.ClassTag

/**
 * One test class per simplification inspection: each test names the [[Rule]] it checks, so the expected highlight
 * description and quick-fix hint come from the rule itself. Negative tests accept no highlight from *any* rule of
 * the inspection.
 */
abstract class SimplificationTestBase[T <: SimplificationInspection: ClassTag] extends InspectionTestBase[T] {

  private var current: String = null

  private lazy val allDescriptions: Set[String] =
    summon[ClassTag[T]].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[SimplificationInspection].rules.map(_.description).toSet

  override protected def description: String = Option(current).getOrElse(s"any of: ${allDescriptions.mkString(" | ")}")

  override protected def descriptionMatches(s: String): Boolean =
    if (current == null) allDescriptions.contains(s) else s == current

  /** `text` marks the expected range with START / END. */
  protected def assertHighlight(rule: Rule, text: String): Unit = {
    current = rule.description
    withHighlightDump(checkTextHasError(text))
  }

  protected def assertFix(rule: Rule, before: String, after: String): Unit = {
    current = rule.description
    withHighlightDump(testQuickFix(before, after, rule.hint))
  }

  protected def assertNone(text: String): Unit = {
    current = null
    withHighlightDump(checkTextHasNoErrors(text))
  }
}
