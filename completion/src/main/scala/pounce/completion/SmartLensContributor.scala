package pounce.completion

import com.intellij.codeInsight.completion.*
import com.intellij.openapi.progress.{ProcessCanceledException, ProgressManager}
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScCaseClause}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.settings.PounceSettings
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.{Bounds, Effects, Libraries, Typeclasses}

import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * A4: smart completion (Ctrl+Shift+Space) from the expected type. At an unqualified position whose expected type is
 * known, offers whole expressions built from catalog constructions:
 *
 *  - companion constructors of the expected type's owner: `IO.pure(|)`, `IO.blocking(|)`, `EitherT.fromEither[|]()`,
 *    `Resource.make(|)(a => )`, `Validated.condNel(|, , )` (adds the class import when the name doesn't resolve);
 *  - summoner constructors for a tagless `F[X]`, through a bound in scope: `Sync[F].delay(|)`, `Sync[F].pure(|)`;
 *  - one-step lifts / conversions of values in scope (locals, parameters, lambda / case / for bindings, members):
 *    `user.some`, `user.asRight[String]`, `user.pure[IO]`, `ios.sequence`, `io.void`, `ids.toNel`;
 *  - two-step lifts into an effect: `user.some.pure[IO]`, `user.asRight[AppError].pure[F]`.
 *
 * Only constructions whose result unifies with the expected type and whose instances are known to exist (Yes) are
 * offered; type arguments are filled in from the expected type when they can be written down.
 */
final class SmartLensContributor extends CompletionContributor {

  override def fillCompletionVariants(parameters: CompletionParameters, result: CompletionResultSet): Unit = {
    if (parameters.getCompletionType != CompletionType.SMART) return
    val items =
      try SmartPlanner.items(parameters, result.getPrefixMatcher)
      catch {
        case e: ProcessCanceledException => throw e
        case NonFatal(_)                 => Nil
      }
    items.foreach(i => result.addElement(LensLookup.create(i)))
  }
}

object SmartPlanner {

  /** How many values in scope are considered, nearest first. */
  private val MaxValues = 40

  def items(parameters: CompletionParameters, prefix: PrefixMatcher): Seq[LensItem] = {
    if (!PounceSettings().smartCompletion) return Nil
    if (!Libraries.hasCats(parameters.getOriginalFile)) return Nil
    val position = org.jetbrains.plugins.scala.lang.completion.positionFromParameters(using parameters)
    val ref = position.getParent match {
      case r: ScReferenceExpression if r.qualifier.isEmpty => r
      case _                                               => return Nil
    }
    val expected = LensPlanner.expectedTypes(ref)
    if (expected.isEmpty) return Nil
    val oracle = new LensOracle(ref)
    val out    = mutable.LinkedHashMap.empty[String, LensItem]
    def add(i: LensItem): Unit =
      if (prefix.prefixMatches(i.lookupString) && out.get(i.lookupString).forall(_.priority < i.priority)) out(i.lookupString) = i

    companionItems(expected, oracle).foreach(add)
    summonerItems(ref, expected, oracle).foreach(add)
    val values = valuesInScope(ref).take(MaxValues)
    if (values.nonEmpty) {
      liftItems(values, expected, oracle).foreach(add)
      twoStepItems(ref, values, expected, oracle).foreach(add)
    }
    out.values.toSeq.sortBy(-_.priority)
  }

  // ---------------------------------------------------------------------------------------------------------------
  // IO.pure(|), EitherT.fromEither[|]()
  // ---------------------------------------------------------------------------------------------------------------

  private def companionItems(expected: Seq[Ty], oracle: LensOracle): Seq[LensItem] = {
    val owners = expected.flatMap(_.headFqn).flatMap(fqn => Owners.ownerOf(Ty.canonicalFqn(fqn))).distinct
    owners.flatMap { owner =>
      val info = Owners.info(owner)
      Catalog.bundled.byOwner.getOrElse(owner, Vector.empty).filter(_.kind == OpKind.Companion).flatMap { op =>
        ProgressManager.checkCanceled()
        Lens.fit(op, Ty.Subst.empty, expected, oracle).map { fit =>
          val (text, caret) = op.insertTextAndCaret
          constructionItem(
            op,
            fit,
            s"$owner.$text",
            owner.length + 1 + caret,
            lookup = s"$owner.${op.name}",
            why = s"$owner.${op.name}: builds the expected ${Lens.resultText(op, Ty.Subst.empty, Some(fit))}",
            classImports = info.map(_.importPath).toList,
            priority = 2.0 + op.rank / 100.0
          )
        }
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Sync[F].delay(|) for an expected F[X]
  // ---------------------------------------------------------------------------------------------------------------

  /** Typeclasses whose summoner text takes the effect alone: `MonadThrow[F]` yes, `MonadError[F]` no. */
  private def summonable(b: Bounds.Bound): Boolean =
    Typeclasses.byName.get(b.canonical).exists(_.kind == Typeclasses.Kind.HigherKinded) &&
      !(Set("ApplicativeError", "MonadError").contains(b.canonical) && b.typeclass == b.canonical)

  private def summonerItems(place: PsiElement, expected: Seq[Ty], oracle: LensOracle): Seq[LensItem] = {
    val rigidHeads = expected.collect { case Ty.Con(f, _ :: _, _, true) => f }.distinct
    if (rigidHeads.isEmpty) return Nil
    val bounds = Bounds.all(place).filter(summonable)
    rigidHeads.flatMap { f =>
      val fBounds = bounds.filter(_.typeParam == f).distinctBy(_.typeclass)
      if (fBounds.isEmpty) Nil
      else {
        val ops = Catalog.bundled.ops.filter(op => op.kind == OpKind.Companion && Typeclasses.byName.contains(op.owner))
        ops.flatMap { op =>
          ProgressManager.checkCanceled()
          val via = fBounds
            .filter(b => Typeclasses.implied(b.canonical).contains(op.owner))
            .sortBy(b => (Typeclasses.implied(b.canonical).size, b.typeclass))
            .headOption
          via.flatMap { b =>
            Lens.fit(op, Ty.Subst.empty, expected, oracle).filter(_.subst.get("F").exists(isRigid(_, f))).map { fit =>
              val (text, caret) = op.insertTextAndCaret
              val summoner      = s"${b.typeclass}[$f]"
              constructionItem(
                op,
                fit,
                s"$summoner.$text",
                summoner.length + 1 + caret,
                lookup = s"$summoner.${op.name}",
                why = s"${b.typeclass}[$f]: from the bound on $f (implies ${op.owner})",
                classImports = Nil,
                priority = 1.5 + op.rank / 100.0
              )
            }
          }
        }
      }
    }
  }

  private def isRigid(t: Ty, name: String): Boolean = t match {
    case Ty.Lambda(`name`, Nil, _, true) => true
    case Ty.Con(`name`, Nil, _, true)    => true
    case _                               => false
  }

  private def constructionItem(
    op: Op,
    fit: Lens.Fit,
    text: String,
    caret: Int,
    lookup: String,
    why: String,
    classImports: List[String],
    priority: Double
  ): LensItem =
    LensItem(
      op = op,
      source = ItemSource.Smart,
      lookupString = lookup,
      insertText = text,
      caret = caret,
      answer = Answer.Yes,
      subst = fit.subst,
      fit = Some(fit),
      receiverText = "",
      typeText = Lens.resultText(op, fit.subst, Some(fit)),
      why = why :: fit.evidence.map(e => s"$e: required by the expected type"),
      syntaxImport = false,
      classImports = classImports,
      priority = priority,
      infix = false
    )

  // ---------------------------------------------------------------------------------------------------------------
  // user.some, user.asRight[String], ios.sequence, io.void
  // ---------------------------------------------------------------------------------------------------------------

  /** Lift ops, and syntax ops without parameters (their result is decided by the receiver and the expected type). */
  private lazy val valueOps: Vector[Op] =
    Catalog.bundled.extensions.filter(op =>
      (op.kind == OpKind.Lift || op.kind == OpKind.Syntax) && op.params.isEmpty && !LensItem.isSymbolic(op.name)
    )

  private def liftItems(values: Seq[(String, Ty)], expected: Seq[Ty], oracle: LensOracle): Seq[LensItem] = {
    // ops whose result can fit at all (cheap, receiver-independent)
    val candidates = valueOps.filter(op => Lens.fit(op, Ty.Subst.empty, expected, oracle).isDefined)
    for {
      ((name, ty), index) <- values.zipWithIndex
      op                  <- candidates
      r                   <- Lens.check(op, ty, oracle).toSeq if r.answer == Answer.Yes
      fit                 <- Lens.fit(op, r.subst, expected, oracle).toSeq
    } yield {
      ProgressManager.checkCanceled()
      val call = callText(op, r.subst, fit)
      LensItem(
        op = op,
        source = ItemSource.Smart,
        lookupString = s"$name.${op.name}",
        insertText = s"$name.$call",
        caret = s"$name.$call".length,
        answer = Answer.Yes,
        subst = r.subst,
        fit = Some(fit),
        receiverText = ty.show,
        typeText = Lens.resultText(op, r.subst, Some(fit)),
        why = s"$name: ${ty.show}, .${op.name} gives the expected ${Lens.resultText(op, r.subst, Some(fit))}" ::
          LensItem.reasons(op, r.subst, r.answer, Some(fit), oracle),
        syntaxImport = op.syntax.isDefined,
        classImports = Nil,
        priority = 1.0 + fit.score / 10.0 + op.rank / 1000.0 - index / 1000.0,
        infix = false
      )
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // user.some.pure[IO]
  // ---------------------------------------------------------------------------------------------------------------

  private def twoStepItems(place: PsiElement, values: Seq[(String, Ty)], expected: Seq[Ty], oracle: LensOracle): Seq[LensItem] = {
    val pureOp = Catalog.bundled.extensions.find(op => op.kind == OpKind.Lift && op.name == "pure")
    val lifts  = Catalog.bundled.extensions.filter(op => op.kind == OpKind.Lift && op.params.isEmpty && op.name != "pure")
    pureOp.toSeq.flatMap { pure =>
      expected.flatMap {
        case outer @ Ty.Con(g, args @ (_ :: _), fqn, rigid) =>
          val inner  = args.last
          val effect = Ty.Lambda(g, args.init, fqn, rigid)
          val gText  = if (args.size == 1) Some(g) else None
          val isEffect = Effects.kindOf(outer, place) match {
            case Effects.Kind.Lazy | Effects.Kind.Tagless => true
            case _                                        => false
          }
          if (gText.isEmpty || !isEffect || oracle.has("Applicative", effect) != Answer.Yes) Nil
          else
            for {
              ((name, ty), index) <- values.zipWithIndex
              op                  <- lifts
              r                   <- Lens.check(op, ty, oracle).toSeq if r.answer == Answer.Yes
              fit                 <- Lens.fit(op, r.subst, Seq(inner), oracle).toSeq if fit.score == 3
            } yield {
              ProgressManager.checkCanceled()
              val text     = s"$name.${callText(op, r.subst, fit)}.pure[${gText.get}]"
              val innerTxt = Lens.resultText(op, r.subst, Some(fit))
              LensItem(
                op = op,
                source = ItemSource.Smart,
                lookupString = s"$name.${op.name}.pure",
                insertText = text,
                caret = text.length,
                answer = Answer.Yes,
                subst = r.subst,
                fit = Some(fit),
                receiverText = ty.show,
                typeText = outer.show,
                why = List(
                  s"$name: ${ty.show}, .${op.name} gives $innerTxt",
                  s"Applicative[${gText.get}]: lifts it into the expected ${outer.show}"
                ),
                syntaxImport = true,
                classImports = Nil,
                priority = 0.5 + op.rank / 1000.0 - index / 1000.0,
                infix = false,
                moreOps = List(pure)
              )
            }
        case _ => Nil
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // helpers
  // ---------------------------------------------------------------------------------------------------------------

  /**
   * The op call with its type arguments written out when the expected type decides them: `asRight[String]`,
   * `pure[IO]`, `raiseError[F, User]`. Falls back to the catalog insert text (minus the caret marker).
   */
  private[completion] def callText(op: Op, receiverSubst: Ty.Subst, fit: Lens.Fit): String = {
    val (text, caret) = op.insertTextAndCaret
    val holeAt        = text.indexOf("[]")
    if (holeAt < 0 || holeAt + 1 != caret) return text
    val free = op.result.toList.flatMap(orderedVars).filterNot(v => receiverSubst.get(v).isDefined).distinct
    val rendered = free.map(v => fit.subst.get(v).flatMap(t => sourceText(fit.subst(t))))
    if (free.isEmpty || rendered.exists(_.isEmpty)) text
    else text.patch(holeAt, rendered.flatten.mkString("[", ", ", "]"), 2)
  }

  private def orderedVars(t: Ty): List[String] = t match {
    case Ty.Var(n)               => List(n)
    case Ty.App(f, args)         => f.name :: args.flatMap(orderedVars)
    case Ty.Con(_, args, _, _)   => args.flatMap(orderedVars)
    case Ty.Fun(ps, r)           => ps.flatMap(orderedVars) ++ orderedVars(r)
    case Ty.Tuple(es)            => es.flatMap(orderedVars)
    case Ty.Lambda(_, fx, _, _)  => fx.flatMap(orderedVars)
  }

  /** Source text for a fully known type; `None` for variables and partially applied constructors. */
  private[completion] def sourceText(t: Ty): Option[String] = t match {
    case Ty.Con(n, Nil, _, _)    => Some(n)
    case Ty.Con(n, args, _, _)   => all(args.map(sourceText)).map(as => s"$n[${as.mkString(", ")}]")
    case Ty.Lambda(n, Nil, _, _) => Some(n)
    case Ty.Tuple(es)            => all(es.map(sourceText)).map(_.mkString("(", ", ", ")"))
    case Ty.Fun(List(p), r)      => for { a <- sourceText(p); b <- sourceText(r) } yield s"$a => $b"
    case Ty.Fun(ps, r)           => for { as <- all(ps.map(sourceText)); b <- sourceText(r) } yield as.mkString("(", ", ", s") => $b")
    case _                       => None
  }

  private def all[A](xs: List[Option[A]]): Option[List[A]] =
    if (xs.forall(_.isDefined)) Some(xs.flatten) else None

  /**
   * Named values visible at `place`, nearest first: earlier statements of enclosing blocks, lambda / method
   * parameters, case-clause and for-comprehension bindings, then members of enclosing templates.
   */
  private[completion] def valuesInScope(place: PsiElement): Seq[(String, Ty)] = {
    val seen = mutable.HashSet.empty[String]
    val out  = mutable.ArrayBuffer.empty[(String, Ty)]
    def add(d: ScTypedDefinition): Unit = {
      val name = d.name
      if (name != null && name.nonEmpty && name != "_" && !seen.contains(name)) {
        seen += name
        d.`type`().toOption.foreach { t =>
          val ty = ScTypeBridge.toTy(t, place)
          out += name -> ty
        }
      }
    }
    var prev: PsiElement = place
    var cur: PsiElement  = place.getContext
    while (cur != null && !cur.isInstanceOf[PsiFile] && out.size < MaxValues) {
      ProgressManager.checkCanceled()
      cur match {
        case _: ScBlock =>
          var s = prev.getPrevSibling
          while (s != null) {
            s match {
              case v: ScValueOrVariable => v.declaredElements.foreach(add)
              case _                    =>
            }
            s = s.getPrevSibling
          }
        case f: ScFunctionDefinition => f.parameters.foreach(add)
        case fe: ScFunctionExpr      => fe.parameters.foreach(add)
        case cc: ScCaseClause        => cc.pattern.toSeq.flatMap(_.bindings).foreach(add)
        case fr: ScFor if !fr.enumerators.exists(_ == prev) =>
          fr.enumerators.toSeq.flatMap(e => descendants(e).collect { case b: ScBindingPattern => b }).foreach(add)
        case tb: ScTemplateBody =>
          tb.getChildren.foreach {
            case v: ScValueOrVariable if !isAncestor(v, prev) => v.declaredElements.foreach(add)
            case _                                            =>
          }
        case _ =>
      }
      prev = cur
      cur = cur.getContext
    }
    out.toSeq
  }

  private def isAncestor(a: PsiElement, b: PsiElement): Boolean = {
    var e = b
    while (e != null && e != a) e = e.getContext
    e == a
  }

  private def descendants(e: PsiElement): Seq[PsiElement] = {
    val b = Seq.newBuilder[PsiElement]
    def go(x: PsiElement): Unit = { b += x; x.getChildren.foreach(go) }
    go(e)
    b.result()
  }
}
