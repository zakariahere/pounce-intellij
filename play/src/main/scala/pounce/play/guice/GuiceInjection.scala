package pounce.play.guice

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}
import com.intellij.psi.*
import com.intellij.psi.search.searches.{DirectClassInheritorsSearch, ReferencesSearch}
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, PsiModificationTracker, PsiTreeUtil}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt}
import pounce.core.Libraries
import pounce.core.settings.PounceSettings
import pounce.play.config.{ConfCalls, ConfIndex}
import pounce.play.util.{PlayPsi, TextFiles}

import scala.jdk.CollectionConverters.*

/** A constructor parameter of a class with an `@Inject()` constructor: `class C @Inject() (repo: UserRepo)`. */
final case class InjectionPoint(param: ScClassParameter, typeElement: ScTypeElement, injected: PsiClass) {
  def fqn: String = PlayPsi.fqn(injected)
}

object GuiceInjection {

  private val Providers = Set("javax.inject.Provider", "jakarta.inject.Provider", "com.google.inject.Provider")

  def enabled(e: PsiElement): Boolean = PounceSettings().playGuice && Libraries.hasPlay(e)

  def injectionPoint(param: ScClassParameter): Option[InjectionPoint] =
    param.containingClass match {
      case cls: ScClass if ConfCalls.hasInjectConstructor(cls) && cls.constructor.exists(_.parameters.contains(param)) =>
        for {
          te  <- param.typeElement
          tpe <- te.`type`().toOption
          cls <- injectedClass(tpe, te)
        } yield InjectionPoint(param, te, cls)
      case _ => None
    }

  /** `Provider[A]` injects A lazily: the binding needed is A's. */
  private def injectedClass(tpe: ScType, place: PsiElement): Option[PsiClass] = {
    given Context = Context(place)
    PlayPsi.safe {
      val unwrapped = tpe match {
        case ParameterizedType(designator, Seq(arg)) if designator.extractClass.exists(c => Providers.contains(PlayPsi.fqn(c))) => arg
        case other => other
      }
      unwrapped.extractClass
    }.flatten
  }

  /** Every `@Inject` constructor parameter in the project whose injected type is `cls` (Provider[cls] included). */
  def injectionPointsOf(cls: PsiClass): Seq[InjectionPoint] = {
    val scope = GlobalSearchScope.projectScope(cls.getProject)
    ReferencesSearch.search(cls, scope).findAll().asScala.toSeq.flatMap { ref =>
      Option(PsiTreeUtil.getParentOfType(ref.getElement, classOf[ScClassParameter])).flatMap(injectionPoint)
    }.filter(_.injected == cls).distinctBy(_.param)
  }

  /** `@ImplementedBy(classOf[Impl])` / `@ProvidedBy(classOf[P])` on the injected type itself. */
  def annotationBindings(cls: PsiClass): Seq[GuiceBinding] = {
    val fqn = PlayPsi.fqn(cls)
    Seq("ImplementedBy" -> BindingKind.ImplementedBy, "ProvidedBy" -> BindingKind.ProvidedBy).flatMap { (name, kind) =>
      val annotation = cls match {
        case td: ScTypeDefinition =>
          td.annotations.find(a => a.typeElement.getText == name || a.typeElement.getText.endsWith(s".$name"))
        case _ =>
          Option(cls.getModifierList).flatMap(_.getAnnotations.find(a => Option(a.getQualifiedName).exists(_.endsWith(s".$name"))))
      }
      annotation.map { a =>
        val target = a match {
          case sa: org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation =>
            PsiTreeUtil.findChildrenOfType(sa, classOf[org.jetbrains.plugins.scala.lang.psi.api.expr.ScGenericCall]).asScala
              .find(_.referencedExpr.getText == "classOf")
              .flatMap(_.typeArguments.headOption.flatMap(_.typeElement))
              .flatMap(te => PlayPsi.safe(te.`type`().toOption.flatMap(_.extractClass(using Context(te)))).flatten)
              .map(PlayPsi.fqn)
          case ja: PsiAnnotation =>
            Option(ja.findAttributeValue("value")).collect { case o: PsiClassObjectAccessExpression => o }
              .flatMap(o => Option(com.intellij.psi.util.PsiTypesUtil.getPsiClass(o.getOperand.getType)))
              .map(PlayPsi.fqn)
        }
        GuiceBinding(fqn, cls.getName, kind, target, a, a)
      }
    }
  }

  def bindingsOf(cls: PsiClass): Seq[GuiceBinding] =
    GuiceIndex.snapshot(cls.getProject).bindingsFor(PlayPsi.fqn(cls)) ++ annotationBindings(cls)

  // ---- unbound injection --------------------------------------------------------------------------------------------

  /** Types Play, Pekko and Guice bind themselves (or that are never Guice-constructed from user bindings). */
  val AllowedPrefixes: Seq[String] = Seq(
    "play.",
    "org.apache.pekko.",
    "akka.",
    "com.google.inject.",
    "javax.inject.",
    "jakarta.inject.",
    "net.codingwell.scalaguice.",
    "com.typesafe.config.",
    "org.slf4j."
  )

  val AllowedFqns: Set[String] = Set(
    "scala.concurrent.ExecutionContext",
    "scala.concurrent.ExecutionContextExecutor",
    "java.util.concurrent.Executor"
  )

  def isAllowListed(fqn: String): Boolean = AllowedFqns.contains(fqn) || AllowedPrefixes.exists(fqn.startsWith)

  private val CompileTimeDiBases = Seq("play.api.BuiltInComponentsFromContext", "play.api.BuiltInComponents")

  /**
   * Compile-time DI (`BuiltInComponentsFromContext`) projects don't use Guice: skip the Guice checks entirely.
   *
   * Direct inheritors in project scope only: a full `ClassInheritorsSearch` walks (and decompiles) the library side of
   * the hierarchy too, which is slow and loads library ASTs while highlighting. Every compile-time DI app has a class
   * extending one of these bases directly.
   */
  def isCompileTimeDi(project: Project): Boolean =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val facade = JavaPsiFacade.getInstance(project)
        val all    = GlobalSearchScope.allScope(project)
        val scope  = GlobalSearchScope.projectScope(project)
        val found = CompileTimeDiBases.exists { fqn =>
          Option(facade.findClass(fqn, all)).exists(base => DirectClassInheritorsSearch.search(base, scope).findFirst() != null)
        }
        CachedValueProvider.Result.create(java.lang.Boolean.valueOf(found), PsiModificationTracker.getInstance(project))
      }
    ).booleanValue

  private val ModulesEnabled = "play.modules.enabled"
  private val EnabledList    = raw"""enabled\s*(?:\+=|=|:)\s*(\[[^\]]*\]|"[^"]*")""".r
  private val QuotedFqn      = raw""""([A-Za-z_][\w.$$]*)"""".r

  /** Module classes named in `play.modules.enabled` (`+=` or a list) in `file`. */
  private def enabledModules(file: VirtualFile): Seq[String] =
    TextFiles.text(file).toSeq.flatMap { text =>
      EnabledList.findAllMatchIn(text).flatMap(m => QuotedFqn.findAllMatchIn(m.group(1)).map(_.group(1))).toSeq
    }

  /**
   * Library jars that register a Play module (`play.modules.enabled` in their `reference.conf`), plus the jars of
   * modules the project enables (`play.modules.enabled += ...`) or installs (`install(new FooModule)`, i.e. any library
   * class mentioned inside a project module): their types are presumably bound by that module.
   *
   * Read from the conf files and the module index, not by searching `Module` inheritors: that search would decompile
   * every library class in the hierarchy.
   */
  private def jarsWithModules(project: Project, guice: GuiceIndex.Snapshot): Set[VirtualFile] = {
    val snapshot = ConfIndex.snapshot(project)
    val facade   = JavaPsiFacade.getInstance(project)
    val all      = GlobalSearchScope.allScope(project)
    val entries  = snapshot.definitions(ModulesEnabled)
    val fromLibraries = entries.filter(_.library).flatMap(e => Option(VfsUtilCore.getRootFile(e.file)))
    val named         = entries.map(_.file).distinct.flatMap(enabledModules)
    val classes = (named ++ guice.mentioned).distinct.flatMap(fqn => Option(facade.findClass(fqn, all)))
    (fromLibraries ++ classes.filterNot(PlayPsi.isInProjectContent).flatMap(jarOf)).toSet
  }

  private def jarOf(cls: PsiElement): Option[VirtualFile] =
    Option(cls.getContainingFile).flatMap(f => Option(f.getVirtualFile)).flatMap(vf => Option(VfsUtilCore.getRootFile(vf)))

  def isAbstract(cls: PsiClass): Boolean = cls match {
    case _: ScTrait => true
    case c: ScClass => c.hasModifierProperty(PsiModifier.ABSTRACT)
    case c          => c.isInterface || c.hasModifierProperty(PsiModifier.ABSTRACT)
  }

  /** Guice can construct a concrete class itself when it has an `@Inject` constructor or a no-arg constructor. */
  def isJitConstructible(cls: PsiClass): Boolean = cls match {
    case c: ScClass =>
      ConfCalls.hasInjectConstructor(c) ||
      c.constructor.forall(_.parameters.isEmpty) ||
      c.secondaryConstructors.exists(f => f.parameters.isEmpty || f.annotations.exists(a => ConfCalls.isInjectAnnotation(a.typeElement.getText)))
    case _ => true // Java and library classes: not modelled, assume fine
  }

  /** Why `point` would make Guice fail at startup, or None when it is (or may be) bound. Conservative by design. */
  def unboundReason(point: InjectionPoint): Option[String] = {
    val cls     = point.injected
    val fqn     = point.fqn
    val project = cls.getProject
    if (fqn.isEmpty || isAllowListed(fqn) || cls.isInstanceOf[PsiTypeParameter]) None
    else if (annotationBindings(cls).nonEmpty) None
    else {
      val snapshot = GuiceIndex.snapshot(project)
      if (snapshot.isBound(fqn) || snapshot.isMentioned(fqn)) None
      else {
        val inProject = PlayPsi.isInProjectContent(cls)
        val library   = !inProject
        if (library && jarOf(cls).exists(jarsWithModules(project, snapshot).contains)) None
        else if (isAbstract(cls)) {
          val what = cls match {
            case _: ScTrait                    => "trait"
            case c if c.isInterface            => "interface"
            case _                             => "abstract class"
          }
          Some(s"No Guice binding found for $what '${cls.getName}' (no bind(...), @Provides or @ImplementedBy)")
        } else if (inProject && !isJitConstructible(cls))
          Some(s"'${cls.getName}' has no @Inject constructor and no Guice binding (Guice cannot construct it)")
        else None
      }
    }
  }
}
