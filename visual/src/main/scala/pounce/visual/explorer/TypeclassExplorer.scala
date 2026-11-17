package pounce.visual.explorer

import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.{Applicability, InstanceOracle, Typeclasses}
import pounce.visual.util.VisualPsi

/**
 * D5 model: what the Typeclass Explorer shows for a type — its normalised shape, which typeclasses have an instance
 * (Yes / No / Unknown, through the [[InstanceOracle]]), the hierarchy below the strongest instances, and the
 * catalog operations available on it. UI-free so it can be tested directly.
 */
object TypeclassExplorer {

  /** One typeclass row: `Traverse[List]` → Yes. */
  final case class Row(tc: Typeclasses.TC, subject: String, answer: Answer) {
    def label: String = s"${tc.name}[$subject]"
  }

  /** Hierarchy node: a typeclass and the classes it implies. */
  final case class Node(name: String, children: Seq[Node])

  final case class OpRow(op: Op, answer: Answer, bindings: String) {
    def label: String = s".${op.name}"
  }

  final case class Report(typeText: String, shape: String, rows: Seq[Row], strongest: Seq[String], tree: Seq[Node], ops: Seq[OpRow]) {
    def answerFor(tc: String): Option[Answer] = rows.find(_.tc.name == tc).map(_.answer)
    def yes: Seq[String]                      = rows.filter(_.answer == Answer.Yes).map(_.tc.name)
  }

  /** The subject under the caret: the innermost typed expression, or a type element. */
  final case class Subject(scType: ScType, place: PsiElement, text: String, expression: Option[ScExpression])

  def subjectAt(file: PsiFile, offset: Int): Option[Subject] = {
    val leaf = Option(file.findElementAt(offset)).orElse(Option(file.findElementAt(offset - 1)))
    leaf.flatMap { l =>
      l.withParents.takeWhile(!_.isInstanceOf[PsiFile]).flatMap {
        case te: ScTypeElement => VisualPsi.safe(te.`type`().toOption).flatten.map(t => Subject(t, te, te.getText, None))
        case r: ScReferenceExpression if isInvokedName(r) => None // `traverse` in `xs.traverse(f)`: explore `xs` instead
        case e: ScExpression   => VisualPsi.typeOf(e).map(t => Subject(t, e, e.getText, Some(e)))
        case _                 => None
      }.nextOption()
    }
  }

  private def isInvokedName(r: ScReferenceExpression): Boolean = r.getParent match {
    case m: ScMethodCall => m.getInvokedExpr == r && r.qualifier.isDefined
    case _               => false
  }

  /** Parses a type typed into the explorer's field (`Either[String, Int]`), resolving names at `context`. */
  def parseType(text: String, context: PsiElement): Option[ScType] =
    VisualPsi.safe(ScalaPsiElementFactory.createTypeFromText(text, context, null)).flatten

  def explore(t: ScType, place: PsiElement): Report =
    explore(ScTypeBridge.toTy(t, place), place, VisualPsi.safe(ScTypeBridge.present(t, place)).getOrElse(t.toString))

  def explore(ty: Ty, place: PsiElement, typeText: String): Report = {
    val oracle = InstanceOracle(place)
    val ctor1  = ScTypeBridge.constructorOf(ty, 1)
    val ctor2  = ScTypeBridge.constructorOf(ty, 2)
    val rows = Typeclasses.all.flatMap { tc =>
      tc.kind match {
        case Typeclasses.Kind.HigherKinded => ctor1.map(c => Row(tc, c.show, ask(oracle, tc.name, c)))
        case Typeclasses.Kind.Binary       => ctor2.map(c => Row(tc, c.show, ask(oracle, tc.name, c)))
        case Typeclasses.Kind.Proper       => Some(Row(tc, ty.show, ask(oracle, tc.name, ty)))
      }
    }
    val yes       = rows.filter(_.answer == Answer.Yes).map(_.tc.name)
    val strongest = yes.filterNot(n => yes.exists(o => o != n && Typeclasses.implied(o).contains(n)))
    val ops = VisualPsi
      .safe(Applicability.applicable(Catalog.bundled.ops, ty, place, Set(OpKind.Syntax, OpKind.Method, OpKind.Tuple)))
      .getOrElse(Nil)
      .map(r => OpRow(r.op, r.answer, r.bindingsText))
      .distinctBy(_.op.name)
    Report(typeText, shapeOf(ty), rows, strongest.sortBy(n => -Typeclasses.depth(n)), strongest.map(tree), ops)
  }

  private def ask(oracle: InstanceOracle, tc: String, t: Ty): Answer =
    VisualPsi.safe(oracle.has(tc, t)).getOrElse(Answer.Unknown)

  def tree(name: String): Node =
    Node(name, Typeclasses.byName.get(name).toSeq.flatMap(_.parents).map(tree))

  /** `F[A] (F = Either[String, *], A = Int)`. */
  def shapeOf(ty: Ty): String = ty match {
    case Ty.Con(_, args, _, _) if args.nonEmpty =>
      val f = ScTypeBridge.constructorOf(ty, 1).map(_.show).getOrElse("?")
      s"F[A]  (F = $f, A = ${args.last.show})"
    case Ty.Fun(_, r)  => s"F[A]  (F = ${ScTypeBridge.constructorOf(ty, 1).map(_.show).getOrElse("?")}, A = ${r.show})"
    case Ty.Tuple(es)  => s"(${es.indices.map(i => s"A${i + 1}").mkString(", ")})"
    case other         => s"A  (A = ${other.show})"
  }
}
