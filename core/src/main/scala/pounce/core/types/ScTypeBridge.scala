package pounce.core.types

import com.intellij.psi.{PsiClass, PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAlias
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{FunctionType, ParameterizedType, TupleType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, ScTypeExt}

import scala.util.control.NonFatal

/** Converts the Scala plugin's [[ScType]] into Pounce's [[Ty]]. */
object ScTypeBridge {

  /**
   * @param dealias expand type aliases first (needed for instance lookups: `ValidatedNel[E, A]` → `Validated[...]`)
   */
  def toTy(t: ScType, place: PsiElement, dealias: Boolean = true): Ty = {
    given Context = Context(place)
    val tpe       = if (dealias) safe(t.removeAliasDefinitions()).getOrElse(t) else t
    convert(tpe.widen, depth = 0, dealias)
  }

  private def safe[T](f: => T): Option[T] = try Some(f) catch { case NonFatal(_) => None }

  private def convert(t: ScType, depth: Int, dealias: Boolean)(using ctx: Context): Ty =
    if (depth > 12) Ty.Con("…", Nil)
    else
      t match {
        case FunctionType(ret, params) =>
          Ty.Fun(params.map(convert(_, depth + 1, dealias)).toList, convert(ret, depth + 1, dealias))
        case TupleType(elems) if elems.size > 1 =>
          Ty.Tuple(elems.map(convert(_, depth + 1, dealias)).toList)
        case tpt: TypeParameterType =>
          Ty.Con(tpt.name, Nil, rigid = true)
        case ParameterizedType(designator, args) =>
          val convertedArgs = args.map(convert(_, depth + 1, dealias)).toList
          designator match {
            case tpt: TypeParameterType => Ty.Con(tpt.name, convertedArgs, rigid = true)
            case other =>
              val (name, fqn) = nameAndFqn(other)
              Ty.Con(name, convertedArgs, fqn)
          }
        case other =>
          val (name, fqn) = nameAndFqn(other)
          Ty.Con(name, Nil, fqn)
      }

  /** Short name and FQN of a type constructor / designator. */
  def nameAndFqn(t: ScType)(using Context): (String, String) = {
    val designated: Option[PsiNamedElement] = t match {
      case ScDesignatorType(e)       => Some(e)
      case p: ScProjectionType       => Some(p.actualElement)
      case _                         => safe(t.extractDesignated(expandAliases = false)).flatten
    }
    designated match {
      case Some(c: PsiClass) =>
        val fqn = Option(c.getQualifiedName).getOrElse("")
        (c.getName, fqn.stripSuffix("$"))
      case Some(a: ScTypeAlias) =>
        val fqn = Option(a.containingClass).flatMap(c => Option(c.getQualifiedName)).map(_.stripSuffix("$") + "." + a.name).getOrElse(a.name)
        (a.name, fqn)
      case Some(n) => (n.getName, "")
      case None =>
        val text = safe(t.canonicalText).getOrElse(t.toString).stripPrefix("_root_.")
        val noArgs = text.takeWhile(_ != '[')
        (noArgs.split('.').last, if (noArgs.contains('.')) noArgs else "")
    }
  }

  /** Strips the last `n` type arguments, giving the constructor that a typeclass is indexed by (`Either[E, *]`). */
  def constructorOf(t: Ty, holes: Int = 1): Option[Ty] = t match {
    case Ty.Con(n, args, fqn, rigid) if args.size >= holes => Some(Ty.Lambda(n, args.dropRight(holes), fqn, rigid))
    case Ty.Tuple(es) if es.size >= 2 && holes == 1       => Some(Ty.Lambda(s"Tuple${es.size}", es.init, s"scala.Tuple${es.size}"))
    case Ty.Fun(ps, _) if holes == 1                       => Some(Ty.Lambda(s"Function${ps.size}", ps, s"scala.Function${ps.size}"))
    case _                                                 => None
  }

  /** Short human presentation of a real type, keeping aliases (`App[A]` stays `App[A]`). */
  def present(t: ScType, place: PsiElement): String =
    toTy(t, place, dealias = false).show
}
