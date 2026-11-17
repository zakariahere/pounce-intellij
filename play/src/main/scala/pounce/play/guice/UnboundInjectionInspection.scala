package pounce.play.guice

import com.intellij.codeInspection.{LocalInspectionTool, ProblemsHolder}
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import pounce.play.config.ConfCalls

/**
 * "Unbound injection": an `@Inject()` constructor parameter whose type Guice cannot provide — a project trait /
 * abstract class (or a library one from a jar that ships no Guice module) with no `bind`, `@Provides`,
 * `@ImplementedBy` / `@ProvidedBy`, or a project class without an injectable constructor. That is a
 * `ConfigurationException` at application start; this catches it while typing.
 *
 * Conservative by design: types Play / Pekko / Guice bind themselves are allow-listed, any mention of the type inside
 * a Guice / Play module counts as a binding, and compile-time DI projects are skipped.
 */
final class UnboundInjectionInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = element match {
        case cls: ScClass if ConfCalls.hasInjectConstructor(cls) && GuiceInjection.enabled(cls) =>
          if (!GuiceInjection.isCompileTimeDi(cls.getProject))
            cls.constructor.toSeq.flatMap(_.parameters).foreach { param =>
              GuiceInjection.injectionPoint(param).foreach { point =>
                GuiceInjection.unboundReason(point).foreach(reason => holder.registerProblem(point.typeElement, reason))
              }
            }
        case _ =>
      }
    }
}
