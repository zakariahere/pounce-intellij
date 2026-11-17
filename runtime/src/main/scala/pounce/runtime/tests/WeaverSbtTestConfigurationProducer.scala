package pounce.runtime.tests

import com.intellij.execution.actions.{ConfigurationContext, ConfigurationFromContext, LazyRunConfigurationProducer}
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeUtil}
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import org.jetbrains.sbt.runner.{SbtConfigurationType, SbtRunConfiguration}
import pounce.core.{Lib, Libraries}

/**
 * Creates an sbt run configuration (the Scala plugin's "sbt Task" type) for a weaver suite or a single weaver test:
 * `testOnly com.acme.UserSuite -- -o "com.acme.UserSuite.finds a user"`. With the template default ("Use sbt
 * shell") the command runs in the project's sbt shell, so repeated runs are fast.
 *
 * Gap: results show as sbt console output, not in the SM test tree (weaver has no IntelliJ test-event reporter).
 */
final class WeaverSbtTestConfigurationProducer extends LazyRunConfigurationProducer[SbtRunConfiguration] {

  override def getConfigurationFactory: ConfigurationFactory =
    ConfigurationTypeUtil.findConfigurationType(classOf[SbtConfigurationType]).getConfigurationFactories.head

  private def targetFor(context: ConfigurationContext): Option[WeaverSupport.Target] =
    Option(context.getPsiLocation)
      .filter(e => Libraries.has(e, Lib.Weaver))
      .flatMap(WeaverSupport.targetAround)

  override protected def setupConfigurationFromContext(
    configuration: SbtRunConfiguration,
    context: ConfigurationContext,
    sourceElement: Ref[PsiElement]
  ): Boolean =
    (for {
      target <- targetFor(context)
      module <- Option(context.getModule).orElse(WeaverSupport.moduleOf(target.anchor))
      if WeaverSupport.sbtAvailable(module)
    } yield {
      configuration.setName(target.displayName)
      // `commands` is a Scala var: its @BeanProperty setters are invisible from Scala 3 (not in TASTy)
      configuration.commands = WeaverSupport.sbtCommand(target, WeaverSupport.projectPrefix(module))
      configuration.setModule(module)
      sourceElement.set(target.anchor)
      true
    }).getOrElse(false)

  override def isConfigurationFromContext(configuration: SbtRunConfiguration, context: ConfigurationContext): Boolean =
    targetFor(context).exists { target =>
      val module = Option(context.getModule).orElse(WeaverSupport.moduleOf(target.anchor))
      val prefix = module.map(WeaverSupport.projectPrefix).getOrElse("")
      configuration.commands == WeaverSupport.sbtCommand(target, prefix)
    }

  /** Weaver suites carry JUnit's `@RunWith(WeaverRunner)`; for weaver code our sbt configuration is the one to use. */
  override def shouldReplace(self: ConfigurationFromContext, other: ConfigurationFromContext): Boolean =
    !other.getConfiguration.isInstanceOf[SbtRunConfiguration]

  override def isPreferredConfiguration(self: ConfigurationFromContext, other: ConfigurationFromContext): Boolean = true
}
