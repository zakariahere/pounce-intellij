package pounce.completion.postfix

import com.intellij.codeInsight.template.postfix.templates.{PostfixTemplate, PostfixTemplateProvider, PostfixTemplateWithExpressionSelector}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.util.Condition
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.selector.AncestorSelector
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import pounce.completion.{Lens, LensOracle, LensPlanner, Owners, SmartPlanner}
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.{Bounds, Effects, Imports, Lib, Libraries, Typeclasses}

import java.util
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * A6: Cats postfix templates (`expr.io<Tab>` → `IO(expr)`). Each template is only offered where the expression's type
 * makes sense (an effect for `.liftF` / `.eval`, a Traverse for `.traverse`, a Sync bound for `.delay`, ...), and adds
 * the imports it needs (class imports for `IO` / `EitherT` / `Resource`, cats syntax in the project's style).
 */
final class PouncePostfixTemplateProvider extends PostfixTemplateProvider {

  private lazy val templates: util.Set[PostfixTemplate] =
    new util.LinkedHashSet[PostfixTemplate](PouncePostfix.all(this).asJava)

  override def getId: String = "pounce.cats"

  override def getPresentableName: String = "Pounce (Cats)"

  override def getTemplates: util.Set[PostfixTemplate] = templates

  override def isTerminalSymbol(currentChar: Char): Boolean = currentChar == '.' || currentChar == '!'

  override def preExpand(file: PsiFile, editor: Editor): Unit = ()

  override def afterExpand(file: PsiFile, editor: Editor): Unit = ()

  override def preCheck(copyFile: PsiFile, realEditor: Editor, currentOffset: Int): PsiFile = copyFile
}

/** What a template turns the expression into. `caret` is relative to the start of `text`. */
final case class Expansion(text: String, caret: Int, syntaxOps: List[Op] = Nil, classImports: List[String] = Nil)

/** A template built from `plan`: applicable where `plan` gives an expansion. */
final class PounceTemplate(key: String, example: String, provider: PostfixTemplateProvider, plan: ScExpression => Option[Expansion])
    extends PostfixTemplateWithExpressionSelector(
      s"pounce.$key",
      key,
      example,
      new AncestorSelector.SelectAllAncestors(PouncePostfix.condition(plan)),
      provider
    ) {

  override protected def expandForChooseExpression(expression: PsiElement, editor: Editor): Unit = expression match {
    case expr: ScExpression =>
      PouncePostfix.safe(plan(expr)).foreach { e =>
        val document = editor.getDocument
        val project  = expr.getProject
        val file     = expr.getContainingFile
        val start    = expr.getTextRange.getStartOffset
        document.replaceString(start, expr.getTextRange.getEndOffset, e.text)
        val caret = document.createRangeMarker(start + e.caret, start + e.caret)
        if (e.syntaxOps.nonEmpty || e.classImports.nonEmpty) {
          val docs = PsiDocumentManager.getInstance(project)
          docs.commitDocument(document)
          Option(file.findElementAt(start)).foreach { place =>
            e.syntaxOps.foreach(op => if (!Imports.isProvided(op, place)) Imports.ensureImportFor(op, place))
            e.classImports.foreach(Owners.ensureImported(_, place))
          }
          docs.doPostponedOperationsAndUnblockDocument(document)
        }
        editor.getCaretModel.moveToOffset(caret.getStartOffset)
        caret.dispose()
      }
    case _ =>
  }
}

object PouncePostfix {

  def all(provider: PostfixTemplateProvider): Seq[PostfixTemplate] = Seq(
    new PounceTemplate("io", "IO(expr)", provider, io),
    new PounceTemplate("delay", "Sync[F].delay(expr)", provider, suspend("delay")),
    new PounceTemplate("blocking", "Sync[F].blocking(expr)", provider, suspend("blocking")),
    new PounceTemplate("pure", "expr.pure[F]", provider, pure),
    new PounceTemplate("some", "expr.some", provider, syntax("some", OpKind.Lift)),
    new PounceTemplate("asRight", "expr.asRight[E]", provider, syntax("asRight", OpKind.Lift)),
    new PounceTemplate("liftF", "EitherT.liftF(expr)", provider, liftF),
    new PounceTemplate("eval", "Resource.eval(expr)", provider, eval),
    new PounceTemplate("traverse", "expr.traverse(a => )", provider, traverse),
    new PounceTemplate("void", "expr.void", provider, syntax("void", OpKind.Syntax)),
    new PounceTemplate("attempt", "expr.attempt", provider, syntax("attempt", OpKind.Syntax)),
    new PounceTemplate("start", "expr.start", provider, syntax("start", OpKind.Syntax))
  )

  /** Every template needs cats on the module's classpath (the cats-effect ones check for cats-effect too). */
  def condition(plan: ScExpression => Option[Expansion]): Condition[PsiElement] = {
    case e: ScExpression => Libraries.hasCats(e) && safe(plan(e)).isDefined
    case _               => false
  }

  def safe[A](f: => Option[A]): Option[A] =
    try f
    catch {
      case e: ProcessCanceledException => throw e
      case NonFatal(_)                 => None
    }

  // ---------------------------------------------------------------------------------------------------------------
  // helpers
  // ---------------------------------------------------------------------------------------------------------------

  private def typeOf(e: ScExpression): Option[Ty] = e.`type`().toOption.map(ScTypeBridge.toTy(_, e))

  private def isValue(e: ScExpression): Boolean = e match {
    case r: ScReferenceExpression => r.resolve() match {
        case _: com.intellij.psi.PsiPackage => false
        case _                              => typeOf(e).nonEmpty
      }
    case _ => typeOf(e).nonEmpty
  }

  /** The expression as a method receiver: `(a + b).void`, but `xs.void`. */
  private def receiverText(e: ScExpression): String = e match {
    case _: ScReferenceExpression | _: ScMethodCall | _: ScGenericCall | _: ScParenthesisedExpr | _: ScLiteral | _: ScTuple |
        _: ScBlockExpr | _: ScUnderscoreSection =>
      e.getText
    case _ => s"(${e.getText})"
  }

  private def isEffect(e: ScExpression, t: Ty): Boolean = Effects.kindOf(t, e) match {
    case Effects.Kind.Lazy | Effects.Kind.Tagless => true
    case _                                        => false
  }

  private def hasEffect(e: ScExpression): Boolean = Libraries.hasCatsEffect(e)

  /** A type parameter in scope whose bounds give `typeclass`, with the bound's source name (for `Sync[F]`). */
  private def boundGiving(e: ScExpression, typeclass: String): Option[(String, String)] =
    Bounds.all(e)
      .filter(b => Typeclasses.implied(b.canonical).contains(typeclass))
      .filter(b => Typeclasses.byName.get(b.canonical).exists(_.kind == Typeclasses.Kind.HigherKinded))
      .filterNot(b => Set("ApplicativeError", "MonadError").contains(b.canonical) && b.typeclass == b.canonical)
      .sortBy(b => Typeclasses.implied(b.canonical).size)
      .headOption
      .map(b => (b.typeParam, b.typeclass))

  private def op(name: String, kind: OpKind): Option[Op] =
    Catalog.bundled.extensions.find(o => o.name == name && o.kind == kind)

  /** The constructor of the expected type at the expression, e.g. `IO` for `IO[Option[User]]`. */
  private def expectedHead(e: ScExpression): Option[Ty] = LensPlanner.expectedTypes(e).headOption

  // ---------------------------------------------------------------------------------------------------------------
  // templates
  // ---------------------------------------------------------------------------------------------------------------

  /** `expr.io` → `IO(expr)`, for any non-effect value when cats-effect is on the classpath. */
  private def io(e: ScExpression): Option[Expansion] =
    Option.when(hasEffect(e) && isValue(e) && typeOf(e).exists(t => !isEffect(e, t))) {
      val text = s"IO(${e.getText})"
      Expansion(text, text.length, classImports = List("cats.effect.IO"))
    }

  /** `expr.delay` → `Sync[F].delay(expr)` with F from a Sync bound in scope; `.blocking` falls back to `IO.blocking`. */
  private def suspend(method: String)(e: ScExpression): Option[Expansion] =
    if (!hasEffect(e) || !isValue(e) || typeOf(e).exists(isEffect(e, _))) None
    else
      boundGiving(e, "Sync") match {
        case Some((f, tc)) =>
          val text = s"$tc[$f].$method(${e.getText})"
          Some(Expansion(text, text.length))
        case None if method == "blocking" =>
          val text = s"IO.blocking(${e.getText})"
          Some(Expansion(text, text.length, classImports = List("cats.effect.IO")))
        case None => None
      }

  /**
   * `expr.pure` → `expr.pure[G]`: G from the expected type when it is an effect, else a type parameter with an
   * Applicative bound, else `IO.pure(expr)`.
   */
  private def pure(e: ScExpression): Option[Expansion] = {
    if (!isValue(e)) return None
    val oracle = new LensOracle(e)
    val fromExpected = expectedHead(e).collect {
      case outer @ Ty.Con(g, List(_), fqn, rigid) if isEffect(e, outer) && oracle.has("Applicative", Ty.Lambda(g, Nil, fqn, rigid)) == Answer.Yes => g
    }
    val g = fromExpected.orElse(boundGiving(e, "Applicative").map(_._1))
    (g, op("pure", OpKind.Lift)) match {
      case (Some(name), Some(pureOp)) =>
        val text = s"${receiverText(e)}.pure[$name]"
        Some(Expansion(text, text.length, syntaxOps = List(pureOp)))
      case (None, _) if hasEffect(e) && typeOf(e).exists(t => !isEffect(e, t)) =>
        val text = s"IO.pure(${e.getText})"
        Some(Expansion(text, text.length, classImports = List("cats.effect.IO")))
      case _ => None
    }
  }

  /** A catalog extension applied as `receiver.op`, offered when [[Lens.check]] says Yes; type args from the expected type. */
  private def syntax(name: String, kind: OpKind)(e: ScExpression): Option[Expansion] =
    for {
      o   <- op(name, kind)
      if isValue(e)
      t   <- typeOf(e)
      oracle = new LensOracle(e)
      r   <- Lens.check(o, t, oracle) if r.answer == Answer.Yes
    } yield {
      val expected = LensPlanner.expectedTypes(e)
      val call = Lens.fit(o, r.subst, expected, oracle) match {
        case Some(fit) => SmartPlanner.callText(o, r.subst, fit)
        case None      => o.insertTextAndCaret._1
      }
      val (_, rawCaret) = o.insertTextAndCaret
      val prefix        = s"${receiverText(e)}."
      val text          = prefix + call
      // keep the caret in an unfilled `[|]`, otherwise at the end
      val caret = if (call == o.insertTextAndCaret._1 && rawCaret < call.length) prefix.length + rawCaret else text.length
      Expansion(text, caret, syntaxOps = List(o))
    }

  /** `expr.traverse` → `expr.traverse(a => |)` when the receiver is traversable. */
  private def traverse(e: ScExpression): Option[Expansion] =
    for {
      o <- op("traverse", OpKind.Syntax)
      if isValue(e)
      t <- typeOf(e)
      r <- Lens.check(o, t, new LensOracle(e)) if r.answer == Answer.Yes
    } yield {
      val prefix = s"${receiverText(e)}.traverse(a => "
      Expansion(prefix + ")", prefix.length, syntaxOps = List(o))
    }

  /** `fa.liftF` → `EitherT.liftF(fa)` (or `OptionT.liftF` when an OptionT is expected), for effects. */
  private def liftF(e: ScExpression): Option[Expansion] =
    typeOf(e).filter(t => isValue(e) && isEffect(e, t)).map { _ =>
      val optionT = expectedHead(e).flatMap(_.headFqn).contains("cats.data.OptionT")
      val owner   = if (optionT) "OptionT" else "EitherT"
      val text    = s"$owner.liftF(${e.getText})"
      Expansion(text, text.length, classImports = List(s"cats.data.$owner"))
    }

  /** `fa.eval` → `Resource.eval(fa)` (or `Stream.eval` when a Stream is expected), for effects. */
  private def eval(e: ScExpression): Option[Expansion] =
    typeOf(e).filter(t => isValue(e) && isEffect(e, t) && hasEffect(e)).map { _ =>
      val stream = expectedHead(e).flatMap(_.headFqn).contains("fs2.Stream") && Libraries.has(e, Lib.Fs2)
      val (owner, fqn) = if (stream) ("Stream", "fs2.Stream") else ("Resource", "cats.effect.Resource")
      val text = s"$owner.eval(${e.getText})"
      Expansion(text, text.length, classImports = List(fqn))
    }
}
