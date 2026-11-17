package pounce.inspections.util

import org.jetbrains.plugins.scala.codeInspection.collections.{isIndependentOf, MethodRepr, ParameterlessCallOnParameterTemplate, Qualified, stripped}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScPatternDefinition, ScValue, ScVariable}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import pounce.inspections.util.Typing.fromCats

import scala.util.control.NonFatal

/**
 * Extractors for the shapes the inspections look for. Method extractors are name-based (the Scala plugin's
 * `Qualified`); rules add semantic checks (receiver is an effect, method comes from cats) on top.
 */
object Patterns {

  def call(names: String*): Qualified = new Qualified(names.toSet.contains)

  val `.map`: Qualified             = call("map")
  val `.flatMap`: Qualified         = call("flatMap")
  val `.flatten`: Qualified         = call("flatten")
  val `.as`: Qualified              = call("as")
  val `.void`: Qualified            = call("void")
  val `.*>`: Qualified              = call("*>", "productR")
  val `.>>`: Qualified              = call(">>")
  val `.sequence`: Qualified        = call("sequence")
  val `.sequence_`: Qualified       = call("sequence_")
  val `.traverse`: Qualified        = call("traverse")
  val `.traverse_`: Qualified       = call("traverse_")
  val `.parSequence`: Qualified     = call("parSequence")
  val `.parSequence_`: Qualified    = call("parSequence_")
  val `.parTraverse`: Qualified     = call("parTraverse")
  val `.parTraverse_`: Qualified    = call("parTraverse_")
  val `.combineAll`: Qualified      = call("combineAll")
  val `.foldLeft`: Qualified        = call("foldLeft")
  val `.attempt`: Qualified         = call("attempt")
  val `.handleErrorWith`: Qualified = call("handleErrorWith")
  val `.fold`: Qualified            = call("fold")
  val `.getOrElse`: Qualified       = call("getOrElse")
  val `.product`: Qualified         = call("product")
  val `.tupled`: Qualified          = call("tupled")
  val `.mapN`: Qualified            = call("mapN")
  val `.start`: Qualified           = call("start")
  val `.use`: Qualified             = call("use")
  val `.allocated`: Qualified       = call("allocated")
  val `.foreach`: Qualified         = call("foreach")
  val `.fromFuture`: Qualified      = call("fromFuture")

  val `_.toOption` = new ParameterlessCallOnParameterTemplate("toOption")
  val `_.flatten`  = new ParameterlessCallOnParameterTemplate("flatten")
  val `_._1`       = new ParameterlessCallOnParameterTemplate("_1")

  /** `x => body`, `_ => body`, `{ x => body }`, `(x: A) => body`. */
  object Lambda1 {
    def unapply(e: ScExpression): Option[(ScParameter, ScExpression)] = stripped(e) match {
      case ScFunctionExpr(Seq(p), Some(body)) => Some((p, stripped(body)))
      case _                                  => None
    }
  }

  /** `_ => b` or `x => b` where `b` doesn't use `x`. */
  object ConstFn {
    def unapply(e: ScExpression): Option[ScExpression] = e match {
      case Lambda1(p, body) if p.isWildcard || isIndependentOf(body, p) => Some(body)
      case _                                                           => None
    }
  }

  /** `identity`, `identity[A]`, `x => x`. */
  object IdentityFn {
    def unapply(e: ScExpression): Boolean = stripped(e) match {
      case Lambda1(p, r: ScReferenceExpression) => !r.isQualified && refersTo(r, p)
      case r: ScReferenceExpression             => r.refName == "identity" && Typing.resolvedOwner(r).contains("scala.Predef")
      case g: ScGenericCall =>
        g.referencedExpr match {
          case r: ScReferenceExpression => r.refName == "identity" && Typing.resolvedOwner(r).contains("scala.Predef")
          case _                        => false
        }
      case _ => false
    }
  }

  def refersTo(r: ScReferenceExpression, p: ScParameter): Boolean =
    try r.resolve() == p
    catch { case NonFatal(_) => false }

  /** `x` where `x` is the lambda parameter `p`. */
  def isRefTo(e: ScExpression, p: ScParameter): Boolean = stripped(e) match {
    case r: ScReferenceExpression => !r.isQualified && refersTo(r, p)
    case _                        => false
  }

  object UnitLit {
    def unapply(e: ScExpression): Boolean = stripped(e).isInstanceOf[ScUnitExpr]
  }

  /**
   * `X.pure(a)` / `X.pure[A](a)` (IO, Applicative[F], F, Sync[F], ...) or `a.pure[F]`, from cats. Returns `a` and the
   * pure call's qualifier for the first form (`IO`, `F`, `Applicative[F]`).
   */
  object Pure {
    def unapply(e: ScExpression): Option[(ScExpression, Option[ScExpression])] = stripped(e) match {
      case MethodRepr(_, Some(qual), Some(ref), Seq(arg)) if ref.refName == "pure" && fromCats(ref) => Some((arg, Some(qual)))
      case g: ScGenericCall =>
        g.referencedExpr match {
          case ref: ScReferenceExpression if ref.refName == "pure" && Typing.resolvesTo(ref, "cats.syntax.ApplicativeIdOps") =>
            ref.qualifier.map(q => (q, None))
          case _ => None
        }
      case _ => None
    }
  }

  object PureOf {
    def unapply(e: ScExpression): Option[ScExpression] = Pure.unapply(e).map(_._1)
  }

  /** `IO.unit`, `F.unit`, `Applicative[F].unit`, `IO.pure(())`, `().pure[F]`. */
  object UnitEffect {
    def unapply(e: ScExpression): Boolean = stripped(e) match {
      case r: ScReferenceExpression if r.refName == "unit" && r.isQualified && fromCats(r) => true
      case PureOf(UnitLit())                                                            => true
      case _                                                                            => false
    }
  }

  /** `IO(a)`, `IO.apply(a)`, `IO.delay(a)`, `Sync[F].delay(a)`, `F.delay(a)`: returns (call, body). */
  object Delay {
    def unapply(e: ScExpression): Option[(ScMethodCall, ScExpression)] = e match {
      case call: ScMethodCall =>
        val body = call.argumentExpressions match {
          case Seq(b) => Some(b)
          case _      => None
        }
        body.filter(_ => isDelayCall(call)).map(b => (call, b))
      case _ => None
    }

    def isDelayCall(call: ScMethodCall): Boolean = invokedRef(call) match {
      case Some(ref) if ref.refName == "IO" && !ref.isQualified => Typing.objectFqn(ref).contains("cats.effect.IO")
      case Some(ref) if ref.refName == "delay" || ref.refName == "apply" =>
        Typing.resolvedOwner(ref).exists(o => o == "cats.effect.IO" || o == "cats.effect.kernel.Sync")
      case _ => false
    }
  }

  /** The reference being called, looking through type arguments (`IO.delay[A](...)`). */
  def invokedRef(call: ScMethodCall): Option[ScReferenceExpression] = call.getInvokedExpr match {
    case r: ScReferenceExpression => Some(r)
    case g: ScGenericCall =>
      g.referencedExpr match {
        case r: ScReferenceExpression => Some(r)
        case _                        => None
      }
    case _ => None
  }

  /**
   * An expression that is cheap and side-effect free to evaluate eagerly, so moving it from a lazy position
   * (`map(_ => b)`) to a strict one (`as(b)`) cannot change behaviour.
   */
  def isStableValue(e: ScExpression, depth: Int = 0): Boolean =
    depth < 6 && (stripped(e) match {
      case _: ScLiteral | _: ScUnitExpr | _: ScThisReference => true
      case t: ScTuple                                        => t.exprs.forall(isStableValue(_, depth + 1))
      case r: ScReferenceExpression =>
        r.qualifier.forall(isStableValue(_, depth + 1)) && (try {
          r.resolve() match {
            case _: ScObject | _: ScParameter => true
            case p: ScReferencePattern =>
              ScalaPsiUtil.inNameContext.unapply(p) match {
                case Some(_: ScVariable) => false
                case Some(v: ScValue)    => !v.hasModifierProperty("lazy")
                case _                   => true // pattern-bound names
              }
            case _ => false
          }
        } catch { case NonFatal(_) => false })
      case MethodRepr(_, Some(r: ScReferenceExpression), None, args) if isPureConstructor(r) =>
        args.forall(isStableValue(_, depth + 1))
      case _ => false
    })

  private val pureFactories = Set("Some", "Right", "Left", "List", "Vector", "Set", "Map", "Seq", "Option", "Nil", "None")

  private def isPureConstructor(r: ScReferenceExpression): Boolean =
    !r.isQualified && (pureFactories.contains(r.refName) || (try {
      r.resolve() match {
        case f: ScFunction if f.name == "apply" =>
          Option(f.containingClass).exists {
            case o: ScObject => o.fakeCompanionClassOrCompanionClass match {
                case c: ScClass => c.isCase
                case _          => false
              }
            case _ => false
          }
        case c: ScClass  => c.isCase
        case o: ScObject => o.fakeCompanionClassOrCompanionClass match {
            case c: ScClass => c.isCase
            case _          => false
          }
        case _ => false
      }
    } catch { case NonFatal(_) => false }))

  /** The local `val` a stable reference points at (for "move the Future inside" fixes). */
  def localValOf(r: ScReferenceExpression): Option[ScPatternDefinition] =
    try
      r.resolve() match {
        case p: ScReferencePattern =>
          ScalaPsiUtil.inNameContext.unapply(p).collect {
            case d: ScPatternDefinition if d.isLocal && d.bindings.size == 1 && !d.hasModifierProperty("lazy") => d
          }
        case _ => None
      }
    catch { case NonFatal(_) => None }
}
