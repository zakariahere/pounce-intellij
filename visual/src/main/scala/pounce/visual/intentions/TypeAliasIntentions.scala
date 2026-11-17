package pounce.visual.intentions

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScParameterizedTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import pounce.core.Libraries
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.visual.hints.TypeAliases
import pounce.visual.util.VisualPsi

import scala.jdk.CollectionConverters.*

/** Shared analysis for the D2 intentions. */
object TypeAliasSupport {

  /** Monad transformers / effect stacks worth an alias. */
  val Stacks: Set[String] = Set(
    "cats.data.EitherT",
    "cats.data.OptionT",
    "cats.data.Kleisli",
    "cats.data.IndexedStateT",
    "cats.data.WriterT",
    "cats.data.IorT",
    "cats.effect.kernel.Resource",
    "fs2.Stream"
  )

  /** The parameterised type element at the caret (innermost one that is an effect stack). */
  def stackAt(element: PsiElement): Option[(ScParameterizedTypeElement, Ty)] =
    element.withParents
      .takeWhile(!_.isInstanceOf[PsiFile])
      .collect { case te: ScParameterizedTypeElement => te }
      .flatMap(te => stackTy(te).map(te -> _))
      .nextOption()

  /** The (dealiased) type of `te` if it is written as a stack, e.g. `EitherT[IO, AppError, User]`. */
  def stackTy(te: ScParameterizedTypeElement): Option[Ty] =
    te match {
      case ScParameterizedTypeElement(_, args) if args.size >= 2 =>
        for {
          t    <- VisualPsi.safe(te.`type`().toOption).flatten
          full <- VisualPsi.safe(ScTypeBridge.toTy(t, te, dealias = true))
          raw  <- VisualPsi.safe(ScTypeBridge.toTy(t, te, dealias = false))
          if full.headFqn.map(Ty.canonicalFqn).exists(Stacks) && raw.headName == full.headName
        } yield full
      case _ => None
    }

  /** Every type element in `root` written as `prefix[..., X]` for the given stack prefix, innermost last. */
  def occurrences(root: PsiElement, stack: Ty): Seq[ScParameterizedTypeElement] = {
    val prefix = stack match {
      case Ty.Con(n, args, fqn, r) => Ty.Con(n, args.init :+ Ty.Var("A\u0000"), fqn, r)
      case other                   => other
    }
    PsiTreeUtil
      .findChildrenOfType(root, classOf[ScParameterizedTypeElement])
      .asScala
      .toSeq
      .filterNot(te => te.parentOfType[ScTypeAliasDefinition].isDefined)
      .filter(te => stackTy(te).exists(t => Ty.matchShape(prefix, t).isDefined))
  }

  def lastArgText(te: ScParameterizedTypeElement): Option[String] = te match {
    case ScParameterizedTypeElement(_, args) => args.lastOption.map(_.getText)
    case _                                   => None
  }

  def replaceWithAlias(te: ScParameterizedTypeElement, aliasName: String): Unit =
    lastArgText(te).foreach { last =>
      val created = ScalaPsiElementFactory.createTypeElementFromText(s"$aliasName[$last]", te, null)
      te.replace(created)
    }
}

/**
 * D2: "Introduce type alias for EitherT[IO, AppError, *]". Creates `type AppIO[A] = EitherT[IO, AppError, A]` as the
 * first member of the enclosing top-level template and rewrites every `EitherT[IO, AppError, X]` in it to `AppIO[X]`.
 * Rename the alias afterwards with Shift+F6 if the derived name doesn't fit.
 */
final class IntroduceTypeAliasIntention extends PsiElementBaseIntentionAction {

  import TypeAliasSupport.*

  override def getFamilyName: String = "Pounce: introduce type alias for an effect stack"

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    plan(element) match {
      case Some(p) =>
        setText(s"Introduce type alias ${p.name}[${p.param}] for ${p.displayStack}")
        true
      case None => false
    }

  final case class Plan(name: String, param: String, aliasText: String, displayStack: String, stack: Ty, target: ScTemplateDefinition)

  def plan(element: PsiElement): Option[Plan] =
    if (!Libraries.hasCats(element)) None
    else
      for {
        (te, stack) <- stackAt(element)
        if TypeAliases.matching(stack, TypeAliases.inScope(te)).isEmpty
        (head, args) <- ScParameterizedTypeElement.unapply(te)
        target <- targetTemplate(te, args.init)
      } yield {
        val prefixTexts = args.init.map(_.getText)
        val param       = Iterator("A", "X", "T", "V").find(p => !prefixTexts.exists(_.matches(s".*\\b$p\\b.*"))).getOrElse("A")
        val name        = uniqueName(deriveName(stack), target)
        val stackText   = s"${head.getText}[${prefixTexts.mkString(", ")}, $param]"
        val display     = s"${head.getText}[${prefixTexts.mkString(", ")}, *]"
        Plan(name, param, s"type $name[$param] = $stackText", display, stack, target)
      }

  /** Outermost template that sees every type parameter used by the stack's fixed arguments. */
  private def targetTemplate(te: ScTypeElement, prefix: Seq[ScTypeElement]): Option[ScTemplateDefinition] = {
    val templates = te.withContexts.collect { case t: ScTemplateDefinition => t }.toList
    val needed = prefix.flatMap(p => VisualPsi.safe(p.`type`().toOption).flatten).flatMap { t =>
      VisualPsi.safe(ScTypeBridge.toTy(t, te)).toSeq.flatMap(VisualPsi.rigidNames)
    }.toSet
    if (needed.isEmpty) templates.lastOption
    else templates.reverse.find(t => needed.subsetOf(t.getTypeParameters.map(_.getName).toSet))
  }

  private def deriveName(stack: Ty): String = {
    def simple(t: Ty): String = t.headName.getOrElse("F").filter(_.isLetterOrDigit)
    stack match {
      case Ty.Con(_, List(m, e, _), "cats.data.EitherT", _) =>
        val err  = simple(e)
        val base = err.stripSuffix("Error").stripSuffix("Err").stripSuffix("Failure")
        if (base.nonEmpty && base != err) base + simple(m) else "Eff"
      case Ty.Con(_, List(m, r, _), "cats.data.Kleisli", _)       => simple(r) + simple(m)
      case Ty.Con(_, List(m, s, _), "cats.data.IndexedStateT", _) => simple(s) + simple(m)
      case Ty.Con(_, List(m, _), "cats.data.OptionT", _)          => "Opt" + simple(m)
      case Ty.Con(n, args, _, _) if args.size >= 2                => n + simple(args.head)
      case _                                                      => "Eff"
    }
  }

  private def uniqueName(base: String, scope: PsiElement): String = {
    val text = scope.getContainingFile.getText
    Iterator.from(0).map(i => if (i == 0) base else s"$base$i").find(n => !text.matches(s"(?s).*\\b$n\\b.*")).get
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    plan(element).foreach { p =>
      val found = occurrences(p.target, p.stack).sortBy(-_.getTextRange.getStartOffset)
      val alias = ScalaPsiElementFactory.createTypeAliasDefinitionFromText(p.aliasText, p.target, null)
      found.foreach(replaceWithAlias(_, p.name))
      p.target.addMember(alias, p.target.members.headOption)
    }

  override def generatePreview(project: Project, editor: Editor, file: PsiFile): IntentionPreviewInfo = IntentionPreviewInfo.EMPTY
}

/**
 * D2: "Use type alias AppIO" on `EitherT[IO, AppError, User]` when `type AppIO[A] = EitherT[IO, AppError, A]` is in
 * scope (enclosing templates, top-level of the file, or a package object). Mirrors zio-intellij's SuggestTypeAlias.
 */
final class UseTypeAliasIntention extends PsiElementBaseIntentionAction {

  import TypeAliasSupport.*

  override def getFamilyName: String = "Pounce: use type alias"

  private def find(element: PsiElement): Option[(ScParameterizedTypeElement, String)] =
    for {
      (te, stack) <- stackAt(element)
      if te.parentOfType[ScTypeAliasDefinition].isEmpty
      (alias, _) <- TypeAliases.matching(stack, TypeAliases.inScope(te).filter(a => a.params.size == 1 && a.valueParam.isDefined))
    } yield te -> alias.name

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    find(element) match {
      case Some((te, name)) =>
        setText(s"Use type alias ${name}[${lastArgText(te).getOrElse("…")}]")
        true
      case None => false
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    find(element).foreach { case (te, name) => replaceWithAlias(te, name) }
}
