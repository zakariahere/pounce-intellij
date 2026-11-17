package pounce.play.guice

import com.intellij.openapi.project.Project
import com.intellij.psi.*
import com.intellij.psi.search.{GlobalSearchScope, PsiSearchHelper, UsageSearchContext}
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, InheritanceUtil, PsiModificationTracker, PsiTreeUtil, PsiTypesUtil}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScParameterizedTypeElement, ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotationsHolder
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScTypeExt}
import pounce.play.util.PlayPsi

import scala.jdk.CollectionConverters.*

/** How a type gets its Guice binding. */
enum BindingKind(val label: String) {
  case Bind          extends BindingKind("bind")
  case Provider      extends BindingKind("provider")
  case Instance      extends BindingKind("instance")
  case Self          extends BindingKind("self binding")
  case Provides      extends BindingKind("@Provides")
  case ImplementedBy extends BindingKind("@ImplementedBy")
  case ProvidedBy    extends BindingKind("@ProvidedBy")
}

/**
 * One binding of `boundFqn`.
 *
 * @param element the navigation target: the whole `bind(...).to(...)` chain, the `@Provides` method, the annotation
 * @param anchor  the leaf the binding gutter sits on (`bind`, the `@Provides` method name)
 */
final case class GuiceBinding(
  boundFqn: String,
  boundName: String,
  kind: BindingKind,
  targetFqn: Option[String],
  element: PsiElement,
  anchor: PsiElement
)

final case class FileBindings(bindings: Seq[GuiceBinding], mentioned: Set[String])

/**
 * Project-wide Guice bindings: `bind(classOf[A]).to(classOf[B])`, scala-guice / Play `bind[A].to[B]`, `toSelf`,
 * `toInstance`, `toProvider`, `bind(new TypeLiteral[A] {})`, `@Provides def a(...): A` (Scala and Java modules), plus
 * the set of types merely *mentioned* inside a Guice / Play module (used by the unbound-injection inspection to stay
 * quiet about anything a module might bind in a way we don't model, e.g. `FactoryModuleBuilder`).
 */
object GuiceIndex {

  final class Snapshot(val bindings: Map[String, Seq[GuiceBinding]], val mentioned: Set[String]) {
    def bindingsFor(fqn: String): Seq[GuiceBinding] = bindings.getOrElse(fqn, Nil)
    def isBound(fqn: String): Boolean                = bindings.contains(fqn)
    def isMentioned(fqn: String): Boolean            = mentioned.contains(fqn)
    def all: Seq[GuiceBinding]                       = bindings.values.flatten.toSeq
  }

  val ModuleTypes: Seq[String] =
    Seq("com.google.inject.Module", "play.api.inject.Module", "net.codingwell.scalaguice.ScalaModule")

  def snapshot(project: Project): Snapshot =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => CachedValueProvider.Result.create(compute(project), PsiModificationTracker.getInstance(project))
    )

  private def compute(project: Project): Snapshot = {
    val scope  = GlobalSearchScope.projectScope(project)
    val helper = PsiSearchHelper.getInstance(project)
    val files  = scala.collection.mutable.LinkedHashSet.empty[PsiFile]
    for (word <- Seq("bind", "Provides"))
      helper.processAllFilesWithWord(word, scope, (f: PsiFile) => { files += f; true }, true)
    val perFile = files.toSeq.collect {
      case f: ScalaFile    => fileBindings(f)
      case f: PsiJavaFile  => fileBindings(f)
    }
    val bindings = perFile.flatMap(_.bindings)
    new Snapshot(bindings.groupBy(_.boundFqn), perFile.flatMap(_.mentioned).toSet ++ bindings.flatMap(_.targetFqn))
  }

  /** Bindings declared in one file (cached until the file changes). */
  def fileBindings(file: PsiFile): FileBindings =
    CachedValuesManager.getCachedValue(file, () => CachedValueProvider.Result.create(scan(file), file))

  def scan(file: PsiFile): FileBindings = file match {
    case f: ScalaFile   => ScalaScanner.scan(f)
    case f: PsiJavaFile => JavaScanner.scan(f)
    case _              => FileBindings(Nil, Set.empty)
  }

  def isModuleClass(cls: PsiClass): Boolean = ModuleTypes.exists(t => InheritanceUtil.isInheritor(cls, t))

  // ---- Scala --------------------------------------------------------------------------------------------------------

  private object ScalaScanner {

    def scan(file: ScalaFile): FileBindings = {
      val binds = PsiTreeUtil.findChildrenOfType(file, classOf[ScReferenceExpression]).asScala.toSeq
        .filter(_.refName == "bind") // bind[A], play.api.inject.bind[A], binder().bind(classOf[A])
        .flatMap(bindingOf)
      val provides = PsiTreeUtil.findChildrenOfType(file, classOf[ScFunctionDefinition]).asScala.toSeq.flatMap(providesOf)
      val mentioned = PsiTreeUtil.findChildrenOfType(file, classOf[ScTemplateDefinition]).asScala.toSeq
        .filter(isModuleClass)
        .flatMap(mentionedIn)
      FileBindings(binds ++ provides, mentioned.toSet)
    }

    private def classOfType(te: ScTypeElement): Option[PsiClass] =
      PlayPsi.safe(te.`type`().toOption.flatMap(_.extractClass(using Context(te)))).flatten

    /** `classOf[X]` → X, `new TypeLiteral[X] {}` → X. */
    private def typeElementOfArg(arg: ScExpression): Option[ScTypeElement] = arg match {
      case g: ScGenericCall if g.referencedExpr.getText == "classOf" => g.typeArguments.headOption.flatMap(_.typeElement)
      case n: ScNewTemplateDefinition =>
        PsiTreeUtil.findChildrenOfType(n, classOf[ScParameterizedTypeElement]).asScala
          .find(_.typeElement.getText.endsWith("TypeLiteral"))
          .flatMap(_.typeArgList.typeArguments.headOption.flatMap(_.typeElement))
      case _ => None
    }

    private def bindingOf(ref: ScReferenceExpression): Option[GuiceBinding] = {
      // bind[A]  |  bind(classOf[A])  |  bind(new TypeLiteral[A] {})
      val (root, boundTe): (ScExpression, Option[ScTypeElement]) = ref.getParent match {
        case g: ScGenericCall if g.referencedExpr == ref =>
          g.getParent match {
            case call: ScMethodCall if call.getInvokedExpr == g => (call, g.typeArguments.headOption.flatMap(_.typeElement))
            case _                                              => (g, g.typeArguments.headOption.flatMap(_.typeElement))
          }
        case call: ScMethodCall if call.getInvokedExpr == ref =>
          (call, call.args.exprs.headOption.flatMap(typeElementOfArg))
        case _ => (ref, None)
      }
      for {
        te    <- boundTe
        bound <- classOfType(te)
        fqn    = PlayPsi.fqn(bound)
        if fqn.nonEmpty
      } yield {
        val (kind, target, outer) = chain(root)
        GuiceBinding(fqn, bound.getName, kind, target.orElse(if (kind == BindingKind.Self) Some(fqn) else None), outer, ref.nameId)
      }
    }

    /** Walks `.annotatedWith(...).to[B].in[Singleton]` after the bind root. */
    private def chain(root: ScExpression): (BindingKind, Option[String], ScExpression) = {
      var current: ScExpression           = root
      var kind: BindingKind               = BindingKind.Self
      var target: Option[String]          = None
      var continue                        = true
      while (continue) current.getParent match {
        case ref: ScReferenceExpression if ref.qualifier.contains(current) =>
          val name = ref.refName
          val (typeArgs, args, next): (Seq[ScTypeElement], Seq[ScExpression], ScExpression) = ref.getParent match {
            case g: ScGenericCall if g.referencedExpr == ref =>
              val tas = g.typeArguments.flatMap(_.typeElement)
              g.getParent match {
                case call: ScMethodCall if call.getInvokedExpr == g => (tas, call.args.exprs, call)
                case _                                              => (tas, Nil, g)
              }
            case call: ScMethodCall if call.getInvokedExpr == ref => (Nil, call.args.exprs, call)
            case _                                                => (Nil, Nil, ref)
          }
          def targetFqn: Option[String] =
            typeArgs.headOption.orElse(args.headOption.flatMap(typeElementOfArg)).flatMap(classOfType).map(PlayPsi.fqn)
              .orElse(args.headOption.collect { case n: ScNewTemplateDefinition => n }.flatMap { n =>
                n.firstConstructorInvocation.flatMap(ci => classOfType(ci.typeElement)).map(PlayPsi.fqn)
              })
          name match {
            case "to"            => kind = if (targetFqn.isEmpty && typeArgs.isEmpty) BindingKind.Instance else BindingKind.Bind; target = targetFqn
            case "toInstance"    => kind = BindingKind.Instance; target = targetFqn
            case "toProvider"    => kind = BindingKind.Provider; target = targetFqn
            case "toConstructor" => kind = BindingKind.Bind
            case "toSelf"        => kind = BindingKind.Self
            case _               =>
          }
          current = next
        case _ => continue = false
      }
      (kind, target, current)
    }

    private def providesOf(fn: ScFunctionDefinition): Option[GuiceBinding] =
      if (!fn.annotations.exists(a => isProvidesAnnotation(a.typeElement.getText))) None
      else
        for {
          tpe <- fn.returnType.toOption
          cls <- PlayPsi.safe(tpe.extractClass(using Context(fn))).flatten
          fqn  = PlayPsi.fqn(cls)
          if fqn.nonEmpty
        } yield GuiceBinding(fqn, cls.getName, BindingKind.Provides, None, fn, fn.nameId)

    private def mentionedIn(td: ScTemplateDefinition): Seq[String] =
      PsiTreeUtil.findChildrenOfType(td.extendsBlock, classOf[ScSimpleTypeElement]).asScala.toSeq.flatMap { te =>
        te.reference.flatMap(r => Option(r.resolve())).collect { case c: PsiClass => PlayPsi.fqn(c) }
      }
  }

  def isProvidesAnnotation(text: String): Boolean = {
    val t = text.trim
    t == "Provides" || t.endsWith(".Provides")
  }

  // ---- Java ---------------------------------------------------------------------------------------------------------

  private object JavaScanner {
    def scan(file: PsiJavaFile): FileBindings = {
      val binds = PsiTreeUtil.findChildrenOfType(file, classOf[PsiMethodCallExpression]).asScala.toSeq.flatMap { call =>
        val ref = call.getMethodExpression
        if (ref.getReferenceName != "bind" || ref.getQualifierExpression != null) None
        else
          call.getArgumentList.getExpressions.headOption.collect { case c: PsiClassObjectAccessExpression => c }.flatMap { arg =>
            Option(PsiTypesUtil.getPsiClass(arg.getOperand.getType)).map { bound =>
              val fqn = PlayPsi.fqn(bound)
              // bind(A.class).to(B.class)
              var outer: PsiExpression = call
              var target: Option[String] = None
              var kind = BindingKind.Self
              var continue = true
              while (continue) outer.getParent match {
                case r: PsiReferenceExpression if r.getQualifierExpression == outer =>
                  r.getParent match {
                    case c: PsiMethodCallExpression =>
                      val classArg = c.getArgumentList.getExpressions.headOption.collect { case o: PsiClassObjectAccessExpression => o }
                      r.getReferenceName match {
                        case "to"         => kind = BindingKind.Bind; target = classArg.flatMap(o => Option(PsiTypesUtil.getPsiClass(o.getOperand.getType))).map(PlayPsi.fqn)
                        case "toInstance" => kind = BindingKind.Instance
                        case "toProvider" => kind = BindingKind.Provider; target = classArg.flatMap(o => Option(PsiTypesUtil.getPsiClass(o.getOperand.getType))).map(PlayPsi.fqn)
                        case _            =>
                      }
                      outer = c
                    case _ => continue = false
                  }
                case _ => continue = false
              }
              GuiceBinding(fqn, bound.getName, kind, target.orElse(if (kind == BindingKind.Self) Some(fqn) else None), outer, ref.getReferenceNameElement)
            }
          }
      }
      val provides = PsiTreeUtil.findChildrenOfType(file, classOf[PsiMethod]).asScala.toSeq.flatMap { m =>
        if (!m.getModifierList.getAnnotations.exists(a => Option(a.getQualifiedName).exists(n => isProvidesAnnotation(n)))) None
        else
          Option(PsiTypesUtil.getPsiClass(m.getReturnType)).map { cls =>
            GuiceBinding(PlayPsi.fqn(cls), cls.getName, BindingKind.Provides, None, m, Option(m.getNameIdentifier).getOrElse(m))
          }
      }
      val mentioned = PsiTreeUtil.findChildrenOfType(file, classOf[PsiClass]).asScala.toSeq.filter(isModuleClass).flatMap { cls =>
        PsiTreeUtil.findChildrenOfType(cls, classOf[PsiJavaCodeReferenceElement]).asScala.flatMap { r =>
          Option(r.resolve()).collect { case c: PsiClass => PlayPsi.fqn(c) }
        }
      }
      FileBindings(binds ++ provides, mentioned.toSet)
    }
  }
}
