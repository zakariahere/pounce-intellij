package pounce.visual.finder

import com.intellij.psi.util.InheritanceUtil
import com.intellij.psi.{JavaPsiFacade, PsiElement}
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression, ScFunctionExpr, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.catalog.Catalog
import pounce.core.types.{ScTypeBridge, Ty}
import pounce.core.StaticInstances.Answer
import pounce.core.InstanceOracle
import pounce.visual.finder.CombinatorFinder.{Have, Match}
import pounce.visual.util.VisualPsi

/**
 * D6 "Fill `???`": one-step catalog expressions built from values in scope (parameters and local vals declared
 * before the hole) whose result unifies with the hole's expected type. Complete expressions come first; ones that
 * still need a `???` follow.
 */
object HoleFiller {

  private val MaxValues = 8

  def isHole(e: PsiElement): Boolean = e match {
    case r: ScReferenceExpression => r.refName == "???" && r.qualifier.isEmpty
    case _                        => false
  }

  def holeAt(element: PsiElement): Option[ScReferenceExpression] =
    element.withParents.take(3).collectFirst { case r: ScReferenceExpression if isHole(r) => r }

  def expectedTy(hole: ScExpression): Option[Ty] =
    VisualPsi.safe(hole.expectedType()).flatten.flatMap(t => VisualPsi.safe(ScTypeBridge.toTy(t, hole)))

  /** Values visible at `place`, nearest first. */
  def valuesInScope(place: PsiElement): Seq[Have] = {
    val found = Seq.newBuilder[(String, ScType)]
    var child: PsiElement = place
    place.contexts.foreach { ctx =>
      ctx match {
        case f: ScFunctionDefinition => f.parameters.foreach(p => paramValue(p).foreach(found += _))
        case l: ScFunctionExpr       => l.parameters.foreach(p => paramValue(p).foreach(found += _))
        case c: ScClass              => c.parameters.foreach(p => paramValue(p).foreach(found += _))
        case b: ScBlock =>
          b.statements.takeWhile(s => s != child && s.getTextRange.getEndOffset <= child.getTextRange.getStartOffset).reverse.foreach {
            case d: ScPatternDefinition =>
              d.pList.patterns.foreach {
                case rp: ScReferencePattern => VisualPsi.safe(rp.`type`().toOption).flatten.foreach(t => found += (rp.name -> t))
                case _                      =>
              }
            case _ =>
          }
        case _ =>
      }
      child = ctx
    }
    found.result().distinctBy(_._1).take(MaxValues).flatMap { case (n, t) =>
      VisualPsi.safe(ScTypeBridge.toTy(t, place)).map(Have(n, _))
    }
  }

  private def paramValue(p: ScParameter): Option[(String, ScType)] =
    VisualPsi.safe(p.`type`().toOption).flatten.map(p.name -> _)

  def candidates(hole: ScExpression, limit: Int = 12): Seq[Match] =
    expectedTy(hole).toSeq.flatMap { want =>
      val values = valuesInScope(hole)
      if (values.isEmpty) Nil
      else {
        val oracle = InstanceOracle(hole)
        val has: CombinatorFinder.Has = (tc, t) => VisualPsi.safe(oracle.has(tc, t)).getOrElse(Answer.Unknown)
        val known = scala.collection.mutable.HashMap.empty[String, Answer]
        val isThrowable: CombinatorFinder.IsThrowable = {
          case t @ Ty.Con(_, _, fqn, false) if fqn.nonEmpty =>
            known.getOrElseUpdate(fqn, VisualPsi.safe(throwableClass(hole, fqn)).flatten.getOrElse(CombinatorFinder.staticThrowable(t)))
          case t => CombinatorFinder.staticThrowable(t)
        }
        CombinatorFinder
          .searchValues(values.toList, want, Catalog.bundled, has, isThrowable = isThrowable)
          .filter(_.usedHave > 0)
          .sortBy(m => (m.holes.size, -m.score))
          .take(limit)
      }
    }

  /** Yes / No from the class hierarchy; None when the class is not found (the name rules decide then). */
  private def throwableClass(place: PsiElement, fqn: String): Option[Answer] =
    Option(JavaPsiFacade.getInstance(place.getProject).findClass(fqn, place.getResolveScope)).map { cls =>
      if (fqn == "java.lang.Throwable" || InheritanceUtil.isInheritor(cls, "java.lang.Throwable")) Answer.Yes else Answer.No
    }
}
