package pounce.completion

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi.PsiPackage
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt}
import pounce.core.Libraries
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.settings.PounceSettings
import pounce.core.types.{ScTypeBridge, Ty}

import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * The Cats Lens: `qualifier.<caret>` completion.
 *
 *  - A1 syntax ops whose instances exist for the receiver (`xs.traverse`, `fa.void` with `F[_]: Functor`), offered
 *    without any cats syntax import; picking one adds the import in the project's style;
 *  - A2 tuple ops on tuples of effects (`(fa, fb).mapN((a, b) => |)`);
 *  - A3 lift ops on any value (`.some`, `.asRight[E]`, `.pure[F]`), ranked by the expected type;
 *  - A5 curated members of companions and summoners (`IO.blocking`, `Sync[F].delay`) and of effect types
 *    (`io.attempt`) are moved to the top and documented.
 *
 * It runs first and wraps the remaining contributors, so anything the Scala plugin already offers (because the
 * import is there, or because the type has such a method) is not duplicated.
 */
final class CatsLensContributor extends CompletionContributor {

  override def fillCompletionVariants(parameters: CompletionParameters, result: CompletionResultSet): Unit = {
    if (parameters.getCompletionType != CompletionType.BASIC) return
    val plan =
      try LensPlanner.plan(parameters, result.getPrefixMatcher)
      catch {
        case e: com.intellij.openapi.progress.ProcessCanceledException => throw e
        case NonFatal(_)                                               => None
      }
    plan.foreach(run(parameters, result, _))
  }

  private def run(parameters: CompletionParameters, result: CompletionResultSet, plan: LensPlanner.Plan): Unit = {
    val ourNames = plan.items.map(_.lookupString).toSet
    val seen     = mutable.HashSet.empty[String]
    result.runRemainingContributors(
      parameters,
      (r: CompletionResult) => {
        val element = r.getLookupElement
        val name    = element.getLookupString
        if (ourNames.contains(name) && isNotImportedGlobalMember(element)) {
          // the Scala plugin's "import implicit conversion" item; ours adds the import in the project's style
        } else {
          seen += name
          plan.curated.get(name) match {
            case Some(op) => result.passResult(r.withLookupElement(LensLookup.decorate(element, op)))
            case None     => result.passResult(r)
          }
        }
      }
    )
    plan.items.iterator.filterNot(i => seen.contains(i.lookupString)).foreach(i => result.addElement(LensLookup.create(i)))
  }

  private def isNotImportedGlobalMember(element: LookupElement): Boolean =
    Option(element.as(classOf[ScalaLookupItem])).exists(_.shouldImport)
}

/** Decides what the Cats Lens offers at a `qualifier.<caret>` position. */
object LensPlanner {

  /** @param items our lookup items @param curated Scala-plugin items to decorate (name → catalog op) */
  final case class Plan(items: Seq[LensItem], curated: Map[String, Op])

  def plan(parameters: CompletionParameters, prefix: PrefixMatcher): Option[Plan] = {
    val settings = PounceSettings()
    if (!(settings.syntaxCompletion || settings.liftCompletion || settings.tupleCompletion || settings.companionCompletion))
      return None
    val original = parameters.getOriginalFile
    if (!Libraries.hasCats(original)) return None

    val position = org.jetbrains.plugins.scala.lang.completion.positionFromParameters(using parameters)
    val ref = position.getParent match {
      case r: ScReferenceExpression => r
      case _                        => return None
    }
    val qualifier = ref.qualifier match {
      case Some(q) => q
      case None    => return None
    }
    qualifier match {
      case _: ScThisReference | _: ScSuperReference => return None
      case r: ScReferenceExpression if r.resolve().isInstanceOf[PsiPackage] => return None
      case _ =>
    }
    summonerOwner(qualifier) match {
      case Some((owners, f)) => return if (settings.companionCompletion) Some(summonerPlan(owners, f, prefix)) else None
      case None              =>
    }
    val qType = qualifier.`type`().toOption match {
      case Some(t) => t
      case None    => return None
    }
    given Context = Context(qualifier)
    singletonObject(qType) match {
      case Some(obj) =>
        val fqn = Owners.fqnOfResolved(obj).getOrElse("")
        Owners.ownerOf(fqn) match {
          case Some(owner) => return if (settings.companionCompletion) Some(companionPlan(owner, prefix)) else None
          case None if ScalaPsiUtil.getCompanionModule(obj).isDefined || obj.isPackageObject => return None
          case None => // a plain object / case object is a value: `NotFound.asLeft[User]`
        }
      case None =>
    }
    Some(valuePlan(ref, qualifier, qType, prefix, settings))
  }

  // ---------------------------------------------------------------------------------------------------------------
  // value.<caret>
  // ---------------------------------------------------------------------------------------------------------------

  private def valuePlan(ref: ScReferenceExpression, qualifier: ScExpression, qType: ScType, prefix: PrefixMatcher, settings: PounceSettings): Plan = {
    val recv     = ScTypeBridge.toTy(qType, qualifier)
    val oracle   = new LensOracle(ref)
    val expected = expectedTypes(ref)
    val isTuple  = recv.isInstanceOf[Ty.Tuple]

    def wanted(op: Op): Boolean =
      prefix.prefixMatches(op.name) && (op.kind match {
        case OpKind.Syntax => settings.syntaxCompletion && !(isTuple && isHigherKindedShape(op))
        case OpKind.Lift   => settings.liftCompletion
        case OpKind.Tuple  => settings.tupleCompletion && isTuple
        case _             => false
      })

    val results = Lens.applicable(Catalog.bundled.extensions.filter(wanted), recv, oracle)
    val items   = results.map(r => extensionItem(r, recv, expected, oracle))

    val curated = if (!settings.companionCompletion) Map.empty[String, Op] else curatedMethods(recv)
    Plan(items, curated)
  }

  private def isHigherKindedShape(op: Op): Boolean = op.receiver.exists {
    case _: Ty.App => true
    case Ty.Con(_, args, _, _) => args.nonEmpty
    case _ => false
  }

  /** Curated instance methods of effect types (`io.attempt`, `resource.use`, `stream.evalMap`). */
  private def curatedMethods(recv: Ty): Map[String, Op] =
    recv.headFqn.flatMap(fqn => Owners.ownerOf(Ty.canonicalFqn(fqn))) match {
      case Some(owner) => bestByName(Catalog.bundled.byOwner.getOrElse(owner, Vector.empty).filter(_.kind == OpKind.Method))
      case None        => Map.empty
    }

  private def bestByName(ops: Iterable[Op]): Map[String, Op] =
    ops.groupBy(_.name).view.mapValues(_.maxBy(_.rank)).toMap

  def extensionItem(r: Lens.Result, recv: Ty, expected: Seq[Ty], oracle: LensOracle): LensItem = {
    val op            = r.op
    val fit           = if (expected.isEmpty) None else Lens.fit(op, r.subst, expected, oracle)
    val (text, caret) = (op.kind, fit) match {
      // `n.asRight[String]` when the expected type decides the type arguments
      case (OpKind.Lift, Some(f)) =>
        val call = SmartPlanner.callText(op, r.subst, f)
        if (call == op.insertTextAndCaret._1) op.insertTextAndCaret else (call, call.length)
      case _ => LensItem.extensionInsert(op, recv)
    }
    val base = op.kind match {
      case OpKind.Lift => -0.5 + op.rank / 1000.0
      case _           => if (r.answer == Answer.Yes) op.rank / 100.0 else -1.0 + op.rank / 1000.0
    }
    val boost = fit.map(f => if (r.answer == Answer.Yes) f.score.toDouble else f.score / 10.0).getOrElse(0.0)
    LensItem(
      op = op,
      source = ItemSource.Extension,
      lookupString = op.name,
      insertText = text,
      caret = caret,
      answer = r.answer,
      subst = r.subst,
      fit = fit,
      receiverText = recv.show,
      typeText = Lens.resultText(op, r.subst, fit),
      why = LensItem.reasons(op, r.subst, r.answer, fit, oracle),
      syntaxImport = op.syntax.isDefined,
      classImports = Nil,
      priority = base + boost,
      infix = LensItem.isSymbolic(op.name)
    )
  }

  /** The expected type at the reference, alias-preserving first and dealiased second (both as [[Ty]]). */
  def expectedTypes(expr: ScExpression): Seq[Ty] =
    try {
      expr.expectedType().toSeq.flatMap { t =>
        val kept      = ScTypeBridge.toTy(t, expr, dealias = false)
        val dealiased = ScTypeBridge.toTy(t, expr, dealias = true)
        if (kept == dealiased) Seq(kept) else Seq(kept, dealiased)
      }.filter {
        case Ty.Con("Any" | "AnyRef" | "Nothing" | "Object", Nil, _, _) => false
        case _                                                         => true
      }
    } catch {
      case e: com.intellij.openapi.progress.ProcessCanceledException => throw e
      case NonFatal(_)                                               => Seq.empty
    }

  // ---------------------------------------------------------------------------------------------------------------
  // IO.<caret>, Resource.<caret>, Sync[F].<caret>
  // ---------------------------------------------------------------------------------------------------------------

  /** A companion object (or `val X = kernel.X` alias of one) as the qualifier's type: `IO.type`. */
  private def singletonObject(t: ScType)(using Context): Option[ScObject] = t match {
    case _: ParameterizedType => None
    case _ =>
      try t.extractDesignated(expandAliases = false) match {
          case Some(o: ScObject) => Some(o)
          case _                 => None
        }
      catch { case NonFatal(_) => None }
  }

  private def companionPlan(owner: String, prefix: PrefixMatcher): Plan = {
    val ops = Catalog.bundled.byOwner.getOrElse(owner, Vector.empty).filter(_.kind == OpKind.Companion)
    // For concrete companions the Scala plugin's member list is authoritative: only decorate, never add.
    Plan(Nil, bestByName(ops.filter(op => prefix.prefixMatches(op.name))))
  }

  /**
   * `Sync[F].` / `Async[F].` / an implicit `F: Sync[F]`: the owners whose companion ops apply (everything the
   * typeclass implies) and the type parameter.
   */
  private[completion] def summonerOwner(qualifier: ScExpression): Option[(Set[String], Option[String])] = {
    val fromSyntax = qualifier match {
      case g: ScGenericCall =>
        g.referencedExpr match {
          case r: ScReferenceExpression =>
            // `Sync[F]` resolves to the companion (Scala 3) or to its `apply` (Scala 2)
            val companion = r.resolve() match {
              case o: ScObject                         => Some(o)
              case f: ScFunction if f.name == "apply" =>
                f.containingClass match {
                  case o: ScObject => Some(o)
                  case _           => None
                }
              case _                                   => None
            }
            companion.flatMap { o =>
              val owners = Owners.typeclassOwners(Option(o.qualifiedName).getOrElse(""))
              if (owners.isEmpty) None else Some((owners, g.typeArguments.headOption.map(_.getText)))
            }
          case _ => None
        }
      case _ => None
    }
    fromSyntax.orElse {
      qualifier.`type`().toOption.flatMap { t =>
        given Context = Context(qualifier)
        // `F.type` / `sync$F$0.type` (summoners return the evidence's singleton type): look at the underlying type
        t.widen match {
          case ParameterizedType(designator, args) if args.nonEmpty =>
            val (_, fqn) = ScTypeBridge.nameAndFqn(designator)
            val owners   = Owners.typeclassOwners(fqn)
            if (owners.isEmpty) None else Some((owners, Some(ScTypeBridge.toTy(args.head, qualifier).show)))
          case _ => None
        }
      }
    }
  }

  private def summonerPlan(owners: Set[String], typeParam: Option[String], prefix: PrefixMatcher): Plan = {
    val ops     = Catalog.bundled.ops.filter(op => op.kind == OpKind.Companion && owners.contains(op.owner) && prefix.prefixMatches(op.name))
    val curated = bestByName(ops)
    val items = curated.values.toSeq.map { op =>
      ProgressManager.checkCanceled()
      val (text, caret) = op.insertTextAndCaret
      val f             = typeParam.getOrElse("F")
      val subst         = Ty.Subst(Map("F" -> Ty.Lambda(f, Nil, rigid = true)))
      LensItem(
        op = op,
        source = ItemSource.Companion,
        lookupString = op.name,
        insertText = text,
        caret = caret,
        answer = Answer.Yes,
        subst = subst,
        fit = None,
        receiverText = s"${op.owner}[$f]",
        typeText = op.result.map(r => subst(r).show).getOrElse(""),
        why = List(s"${op.owner}[$f]: implied by the summoned typeclass"),
        syntaxImport = false,
        classImports = Nil,
        priority = 3.0 + op.rank / 100.0,
        infix = false
      )
    }
    Plan(items, curated)
  }
}
