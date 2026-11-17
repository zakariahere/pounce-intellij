package pounce.core

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.types.{ScTypeBridge, Ty}

import scala.util.control.NonFatal

/**
 * "Is this value a lazy effect description?" - the question behind the discarded-effect, nested-effect and
 * mixed-effects inspections, and the for-comprehension lens.
 */
object Effects {

  /** Lazy effect types: discarding one of these means the effect never runs. */
  val lazyEffectFqns: Set[String] = Set(
    "cats.effect.IO",
    "cats.effect.SyncIO",
    "cats.effect.kernel.Resource",
    "cats.data.EitherT",
    "cats.data.OptionT",
    "cats.data.Kleisli",
    "cats.data.IndexedStateT",
    "cats.data.WriterT",
    "cats.data.IorT",
    "cats.free.Free",
    "fs2.Stream",
    "fs2.Pull",
    "doobie.free.connection.ConnectionIO"
  )

  /** Short display names for the for-comprehension lens. */
  def displayName(t: Ty): String = t match {
    case Ty.Con(name, args, fqn, _) =>
      Ty.canonicalFqn(fqn) match {
        case "cats.data.EitherT" | "cats.data.Kleisli" | "cats.data.IndexedStateT" | "cats.data.WriterT" |
            "cats.data.IorT" | "cats.effect.kernel.Resource" | "fs2.Stream" if args.nonEmpty =>
          Ty.show(Ty.Lambda(name, args.init))
        case _ if args.size > 1 => Ty.show(Ty.Lambda(name, args.init))
        case _                  => name
      }
    case other => other.show
  }

  enum Kind {
    /** IO, Resource, EitherT, Stream, ConnectionIO, ... */
    case Lazy
    /** A rigid `F[_]` whose bounds include at least Functor (tagless final). */
    case Tagless
    /** Future: eager, so discarding it doesn't stop it from running (but loses errors). */
    case Eager
    case NotAnEffect
  }

  def kindOf(t: ScType, place: PsiElement): Kind =
    try kindOf(ScTypeBridge.toTy(t, place), place)
    catch { case NonFatal(_) => Kind.NotAnEffect }

  def kindOf(t: Ty, place: PsiElement): Kind = t match {
    case Ty.Con(_, args, fqn, false) if args.nonEmpty && lazyEffectFqns.contains(Ty.canonicalFqn(fqn)) => Kind.Lazy
    case Ty.Con(_, _, "scala.concurrent.Future", false)                                                 => Kind.Eager
    case Ty.Con(name, args, _, true) if args.nonEmpty =>
      val bs = Bounds.forParam(place, name)
      if (bs.exists(b => Typeclasses.implied(b).contains("Functor"))) Kind.Tagless else Kind.NotAnEffect
    case _ => Kind.NotAnEffect
  }

  def isEffect(t: ScType, place: PsiElement): Boolean = kindOf(t, place) match {
    case Kind.Lazy | Kind.Tagless => true
    case _                        => false
  }

  def exprKind(expr: ScExpression): Kind =
    expr.`type`().toOption.map(kindOf(_, expr)).getOrElse(Kind.NotAnEffect)

  /** The effect's type constructor (`IO`, `EitherT[IO, Err, *]`, `F`) for comparing for-comprehension steps. */
  def constructor(t: Ty): Option[Ty] = ScTypeBridge.constructorOf(t)
}
