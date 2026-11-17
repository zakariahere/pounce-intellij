package pounce.runtime.tests

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.util.{InheritanceUtil, PsiTreeUtil}
import com.intellij.psi.{PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.ScInterpolatedStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScTemplateDefinition, ScTrait, ScTypeDefinition}

import scala.annotation.tailrec

/**
 * weaver-test (0.8 `com.disneystreaming`, 0.9+ `org.typelevel`) support: finds suites and statically named tests,
 * and builds the sbt command that runs one of them.
 *
 * Verified against weaver 0.13.1 class files: tests are registered by `FSuite.test/pureTest/loggedTest(TestName)`
 * (`FunSuiteF.test` for pure suites); `-o`/`--only` takes a glob matched against `suiteName + "." + testName`, where
 * `suiteName` is the suite's JVM class name with every `$` removed (`EffectSuite.suiteName`).
 */
object WeaverSupport {

  /** Any of these in the hierarchy makes a class/object a weaver suite (0.8 and 0.9+ names). */
  val SuiteBases: Seq[String] =
    Seq("weaver.EffectSuite", "weaver.RunnableSuite", "weaver.BaseSuiteClass", "weaver.MutableFSuite", "weaver.FunSuiteF")

  val TestMethods: Set[String] = Set("test", "pureTest", "loggedTest")

  /** Tags that keep the test's name (`"name".only`, `"name".ignore`, `"name".tagged("x")`). */
  private val NameTags = Set("only", "ignore", "tagged")

  /** A test (or a whole suite when `testName` is None) that can be run on its own. */
  final case class Target(
    /** Name sbt's test discovery reports (`testOnly` selector): source-level FQN, e.g. `com.acme.UserSuite`. */
    selector: String,
    /** weaver's own suite name: JVM class name without `$` (`com.acme.Outer$Inner$` → `com.acme.OuterInner`). */
    suiteName: String,
    testName: Option[String],
    /** The element the gutter is attached to / the configuration points back to. */
    anchor: PsiElement
  ) {
    def displayName: String = {
      val simple = selector.substring(selector.lastIndexOf('.') + 1)
      testName.fold(s"weaver: $simple")(t => s"weaver: $simple.$t")
    }
  }

  /** Classes and objects (not traits, not local or anonymous classes) extending a weaver suite. */
  def isWeaverSuite(td: ScTemplateDefinition): Boolean = td match {
    case _: ScTrait            => false
    case t: ScTypeDefinition => !isLocal(t) && SuiteBases.exists(InheritanceUtil.isInheritor(t, _))
    case _                     => false
  }

  private def isLocal(td: ScTypeDefinition): Boolean = td.containingClass == null && !td.isTopLevel

  /** Owner chain from the outermost type to `td`, or None for local / anonymous definitions. */
  private def owners(td: ScTypeDefinition): Option[List[ScTypeDefinition]] = {
    @tailrec def go(t: ScTypeDefinition, acc: List[ScTypeDefinition]): Option[List[ScTypeDefinition]] =
      if (t == null || t.name == null) None
      else if (t.isTopLevel) Some(t :: acc)
      else
        t.containingClass match {
          case outer: ScTypeDefinition => go(outer, t :: acc)
          case _                       => None
        }
    go(td, Nil)
  }

  def suiteTarget(td: ScTemplateDefinition, anchor: PsiElement): Option[Target] = td match {
    case t: ScTypeDefinition if isWeaverSuite(t) => suiteTargetOf(t, anchor)
    case _                                       => None
  }

  private def suiteTargetOf(td: ScTypeDefinition, anchor: PsiElement): Option[Target] =
    owners(td).flatMap { chain =>
      val outer = chain.head
      Option(outer.qualifiedName).map { outerFqn =>
        val pkg      = outerFqn.stripSuffix(outer.name).stripSuffix(".")
        val prefix   = if (pkg.isEmpty) "" else pkg + "."
        val selector = prefix + chain.map(_.name).mkString(".")
        val weaver   = prefix + chain.map(_.name).mkString
        Target(selector, weaver, None, anchor)
      }
    }

  /** The static name of a weaver test call's first argument, if it has one. */
  def staticTestName(arg: ScExpression): Option[String] = arg match {
    case _: ScInterpolatedStringLiteral => None
    case lit: ScStringLiteral           => Option(lit.getValue)
    case ref: ScReferenceExpression if NameTags(ref.refName) => ref.qualifier.flatMap(staticTestName)
    case ScMethodCall(ref: ScReferenceExpression, _) if NameTags(ref.refName) => ref.qualifier.flatMap(staticTestName)
    case _ => None
  }

  /**
   * `test("name")` / `pureTest("name") { ... }` / `loggedTest("name") { log => ... }` written directly in a suite
   * body, whose method resolves into weaver. The innermost call `test("name")` is returned with the reference.
   */
  def testCall(ref: ScReferenceExpression): Option[(ScMethodCall, ScTemplateDefinition)] =
    if (ref == null || !TestMethods(ref.refName) || ref.qualifier.isDefined) None
    else
      ref.getParent match {
        case call: ScMethodCall if call.getInvokedExpr == ref =>
          val statement = outermostCall(call)
          statement.getParent match {
            case body: ScTemplateBody =>
              PsiTreeUtil.getParentOfType(body, classOf[ScTemplateDefinition]) match {
                case td if isWeaverSuite(td) && resolvesIntoWeaver(ref) => Some(call -> td)
                case _                                                   => None
              }
            case _ => None
          }
        case _ => None
      }

  @tailrec private def outermostCall(call: ScMethodCall): ScExpression = call.getParent match {
    case outer: ScMethodCall if outer.getInvokedExpr == call => outermostCall(outer)
    case _                                                  => call
  }

  private def resolvesIntoWeaver(ref: ScReferenceExpression): Boolean = ref.resolve() match {
    case m: PsiMethod => Option(m.getContainingClass).flatMap(c => Option(c.getQualifiedName)).exists(_.startsWith("weaver."))
    case _            => false
  }

  def testTarget(ref: ScReferenceExpression): Option[Target] =
    for {
      (call, td) <- testCall(ref)
      arg        <- call.argumentExpressions.headOption
      name       <- staticTestName(arg)
      suite      <- suiteTarget(td, ref)
    } yield suite.copy(testName = Some(name), anchor = ref)

  /**
   * The gutter anchor rule: run line markers are computed for leaf elements, so this accepts the identifier of a
   * test method reference or of a suite's name.
   */
  def targetAtLeaf(leaf: PsiElement): Option[Target] =
    if (leaf == null || leaf.getNode == null || leaf.getNode.getElementType != ScalaTokenTypes.tIDENTIFIER) None
    else
      leaf.getParent match {
        case ref: ScReferenceExpression                          => testTarget(ref)
        case td: ScTemplateDefinition if td.nameId == leaf => suiteTarget(td, td)
        case _                                                   => None
      }

  /** Same, for any element inside the test call or suite (run configuration contexts). */
  def targetAround(element: PsiElement): Option[Target] =
    targetAtLeaf(element).orElse {
      val ref = PsiTreeUtil.getParentOfType(element, classOf[ScReferenceExpression], false)
      Option(ref).flatMap(testTarget).orElse {
        element match {
          case td: ScTemplateDefinition => suiteTarget(td, td)
          case _                        => None
        }
      }
    }

  // ------------------------------------------------------------------ sbt command

  /** sbt's quoted-string syntax (`StringEscapable`): backslash-escape `\` and `"`. */
  def sbtQuote(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /**
   * `testOnly com.acme.UserSuite -- -o "com.acme.UserSuite.finds a user"`; `*` inside test names stays a weaver
   * glob wildcard (it still matches the name itself).
   *
   * @param projectPrefix sbt project selector such as `{file:/work/app/}core/` (empty for the root project)
   */
  def sbtCommand(target: Target, projectPrefix: String = ""): String = {
    val base = s"${projectPrefix}testOnly ${target.selector}"
    target.testName.fold(base)(name => s"$base -- -o ${sbtQuote(target.suiteName + "." + name)}")
  }

  /** The sbt project of a module, as the Scala plugin's own sbt test runner selects it. */
  def projectPrefix(module: Module): String =
    try
      org.jetbrains.sbt.SbtUtil.getSbtProjectUriAndId(module) match {
        case Some(uriAndId) =>
          val p = org.jetbrains.sbt.shell.SettingQueryHandler.getProjectIdPrefix(uriAndId)
          if (p == null || p.isEmpty || p.endsWith("/")) Option(p).getOrElse("") else p + "/"
        case None => ""
      }
    catch { case _: Throwable => "" }

  /** sbt can run the module's tests: an sbt-imported module, or a project with a `build.sbt` at its root. */
  def sbtAvailable(module: Module): Boolean =
    module != null && !module.isDisposed && (
      com.intellij.openapi.application.ApplicationManager.getApplication.isUnitTestMode ||
        org.jetbrains.sbt.SbtUtil.isSbtModule(module) ||
        hasBuildSbt(module.getProject)
    )

  private def hasBuildSbt(project: Project): Boolean =
    Option(project.getBasePath).exists(p => java.nio.file.Files.isRegularFile(java.nio.file.Path.of(p, "build.sbt")))

  def moduleOf(element: PsiElement): Option[Module] = Option(ModuleUtilCore.findModuleForPsiElement(element))
}
