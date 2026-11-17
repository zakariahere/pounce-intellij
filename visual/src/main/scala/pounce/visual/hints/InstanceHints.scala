package pounce.visual.hints

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.{Applicability, InstanceOracle, Typeclasses}
import pounce.visual.util.VisualPsi

/**
 * D3: which typeclass instances a cats call uses: `ids.traverse(findUser)` ▸ `Traverse[List] · Applicative[IO]`.
 *
 * Prefers the implicits the Scala plugin actually resolved (the syntax conversion's arguments plus the method's own
 * implicit clause), so a click goes to the real instance. Falls back to the catalog + instance oracle when the
 * resolver gives nothing (e.g. while code is still being typed).
 */
object InstanceHints {

  final case class Instance(text: String, target: Option[PsiElement])

  final case class InstanceHint(offset: Int, instances: Seq[Instance], tooltip: String) {
    def text: String = instances.map(_.text).mkString(" · ")
  }

  private val Kinds = Set(OpKind.Syntax, OpKind.Tuple, OpKind.Lift)

  private lazy val opNames: Set[String] = Catalog.bundled.ops.filter(o => Kinds(o.kind)).map(_.name).toSet

  def hintFor(element: PsiElement): Option[InstanceHint] = element match {
    case call: ScMethodCall if !call.getParent.isInstanceOf[ScMethodCall] =>
      invokedRef(call.getInvokedExpr).flatMap(ref => ref.qualifier.flatMap(q => hint(call, ref, q)))
    case ref: ScReferenceExpression if !isInvoked(ref) =>
      ref.qualifier.flatMap(q => hint(ref, ref, q))
    case infix: ScInfixExpr =>
      hint(infix, infix.operation, infix.left)
    case _ => None
  }

  private def invokedRef(e: ScExpression): Option[ScReferenceExpression] = e match {
    case r: ScReferenceExpression => Some(r)
    case g: ScGenericCall         => invokedRef(g.referencedExpr)
    case m: ScMethodCall          => invokedRef(m.getInvokedExpr)
    case _                        => None
  }

  private def isInvoked(ref: ScReferenceExpression): Boolean = ref.getParent match {
    case m: ScMethodCall  => m.getInvokedExpr == ref
    case _: ScGenericCall => true
    case i: ScInfixExpr   => i.operation == ref
    case p: ScPostfixExpr => p.operation == ref
    case _                => false
  }

  private def hint(owner: ScExpression, ref: ScReferenceExpression, qualifier: ScExpression): Option[InstanceHint] = {
    val name = ref.refName
    for {
      _        <- Option.when(opNames.contains(name))(())
      resolved <- VisualPsi.safe(ref.bind()).flatten
      if VisualPsi.ownerFqn(resolved.element).exists(VisualPsi.isTypelevel)
      fromResolver = resolvedInstances(owner, qualifier, resolved)
      instances    = if (fromResolver.nonEmpty) fromResolver else fromCatalog(owner, name, qualifier)
      if instances.nonEmpty
    } yield {
      val via = if (fromResolver.nonEmpty) "resolved by the Scala plugin" else "inferred from the Pounce catalog"
      InstanceHint(owner.getTextRange.getEndOffset, instances, s".$name uses ${instances.map(_.text).mkString(", ")} ($via)")
    }
  }

  private def resolvedInstances(owner: ScExpression, qualifier: ScExpression, resolved: ScalaResolveResult): Seq[Instance] = {
    val conversion = resolved.implicitConversion.orElse(VisualPsi.safe(qualifier.implicitConversion()).flatten)
    val args =
      conversion.toSeq.flatMap(_.implicitArguments).flatMap(_.args) ++
        VisualPsi.safe(owner.findImplicitArguments).toSeq.flatten.flatMap(_.args)
    args
      .filterNot(_.isImplicitParameterProblem)
      .flatMap { arg =>
        arg.implicitSearchState.map(_.tp).flatMap { tp =>
          VisualPsi.safe(ScTypeBridge.toTy(tp, owner, dealias = false)).collect {
            case t @ Ty.Con(n, _, _, _) if Typeclasses.canonical(n).isDefined => Instance(clean(t.show), Option(arg.element))
          }
        }
      }
      .distinctBy(_.text)
  }

  /** `Traverse[List]` · `Applicative[IO]` from the catalog entry: receiver shape + the call's result type. */
  private def fromCatalog(owner: ScExpression, name: String, qualifier: ScExpression): Seq[Instance] = {
    val result = for {
      recv   <- VisualPsi.tyOf(qualifier)
      actual = VisualPsi.tyOf(owner)
      oracle = InstanceOracle(owner)
      ops    = Catalog.bundled.byName.getOrElse(name, Vector.empty).filter(o => Kinds(o.kind))
      found  <- ops.iterator.flatMap(op => Applicability.check(op, recv, owner, oracle).map(op -> _)).nextOption()
    } yield {
      val (op, res) = found
      val subst     = actual.flatMap(a => op.result.flatMap(r => Ty.unify(r, a, res.subst))).getOrElse(res.subst)
      op.requires.flatMap { case (v, tc) => subst.get(v).map(t => Instance(s"$tc[${subst(t).show}]", None)) }
    }
    result.getOrElse(Nil)
  }

  /** `Monad[[β$0$]Either[String, β$0$]]` → `Monad[Either[String, *]]`. */
  def clean(text: String): String = {
    val lambda = """\[([^\[\]\s,]+)\]([\w.]+)\[([^\[\]]*), ([^\[\]\s,]+)\]""".r
    lambda.replaceAllIn(text, m => if (m.group(1) == m.group(4)) java.util.regex.Matcher.quoteReplacement(s"${m.group(2)}[${m.group(3)}, *]") else java.util.regex.Matcher.quoteReplacement(m.matched))
  }
}
