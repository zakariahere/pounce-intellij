package pounce.runtime.tests

import com.intellij.execution.lineMarker.{ExecutorAction, RunLineMarkerContributor}
import com.intellij.icons.AllIcons
import com.intellij.psi.PsiElement
import pounce.core.{Lib, Libraries}
import pounce.runtime.settings.PounceRuntimeSettings

/**
 * Run / Debug gutter icons on weaver suites and on statically named `test("...")`, `pureTest("...")`,
 * `loggedTest("...")` calls. The actions come from [[WeaverSbtTestConfigurationProducer]] (sbt `testOnly ... -- -o`).
 *
 * The Scala plugin (2026.2.18) ships no weaver support at all; munit-cats-effect suites are already covered by its
 * MUnit integration (`CatsEffectSuite` extends `munit.FunSuite`), so this contributor stays out of them.
 */
final class WeaverRunLineMarkerContributor extends RunLineMarkerContributor {

  override def getInfo(element: PsiElement): RunLineMarkerContributor.Info =
    if (!PounceRuntimeSettings().weaverGutters || !Libraries.has(element, Lib.Weaver)) null
    else
      WeaverSupport.targetAtLeaf(element) match {
        case Some(target) if WeaverSupport.moduleOf(element).exists(WeaverSupport.sbtAvailable) =>
          val icon    = if (target.testName.isEmpty) AllIcons.RunConfigurations.TestState.Run_run else AllIcons.RunConfigurations.TestState.Run
          val tooltip = target.testName.fold(s"Run weaver suite ${target.selector}")(n => s"Run weaver test \"$n\"")
          new RunLineMarkerContributor.Info(icon, ExecutorAction.getActions(1), (_: PsiElement) => tooltip)
        case _ => null
      }
}
