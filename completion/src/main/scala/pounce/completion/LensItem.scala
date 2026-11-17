package pounce.completion

import pounce.core.StaticInstances.Answer
import pounce.core.catalog.{Op, OpKind}
import pounce.core.types.Ty

/** Where a Pounce lookup item comes from (decides presentation and the quick-doc "why" line). */
enum ItemSource {

  /** `qualifier.op` — syntax / lift / tuple extension. */
  case Extension

  /** `IO.op`, `Sync[F].op` — a curated companion / summoner member. */
  case Companion

  /** A whole expression built from the expected type (smart completion): `IO.pure(|)`, `user.some`. */
  case Smart
}

/**
 * Everything a Pounce lookup element needs: what to show, what to insert, which imports to add, and why it applies
 * (shown by the quick documentation, see [[doc.LensDocumentation]]).
 *
 * @param lookupString  what the prefix matcher sees (`traverse`, `IO.pure`, `user.some`)
 * @param insertText    replaces the lookup string on insert; `caret` is the caret offset inside it
 * @param infix         symbolic operator (`*>`, `|+|`): the dot before it is removed (`fa *> |`)
 * @param syntaxImport  add the op's cats syntax import (in the project's style) unless something provides it
 * @param classImports  FQNs to import unless their short name already resolves (`cats.effect.IO`)
 * @param why           human-readable reasons, e.g. `Traverse[List] (known instance)`
 * @param moreOps       further catalog ops the inserted text uses (`user.some.pure[IO]`), for their syntax imports
 */
final case class LensItem(
  op: Op,
  source: ItemSource,
  lookupString: String,
  insertText: String,
  caret: Int,
  answer: Answer,
  subst: Ty.Subst,
  fit: Option[Lens.Fit],
  receiverText: String,
  typeText: String,
  why: List[String],
  syntaxImport: Boolean,
  classImports: List[String],
  priority: Double,
  infix: Boolean,
  moreOps: List[Op] = Nil
) {
  def isUnknown: Boolean = answer == Answer.Unknown

  /** Parameters as shown after the name: `(A => G[B])`. */
  def paramsText: String =
    if (op.params.isEmpty) ""
    else op.params.map(p => fit.map(_.subst).getOrElse(subst)(p).show).mkString("(", ", ", ")")

  /** Grey tail: the instances that make this applicable and the library: ` Traverse[List] · cats`. */
  def constraintsText: String = {
    val bound = op.requires.flatMap { case (v, tc) =>
      fit.map(_.subst).getOrElse(subst).get(v).map(t => s"$tc[${Ty.show(t)}]")
    }
    val lib = op.library.displayName
    val q   = if (isUnknown) "?" else ""
    (if (bound.isEmpty) "" else bound.mkString(", ") + q + " · ") + lib
  }
}

object LensItem {

  def isSymbolic(name: String): Boolean = name.nonEmpty && !Character.isJavaIdentifierStart(name.head)

  /** Insert text for an extension op; tuple ops get an arity-correct lambda: `mapN((a, b) => |)`. */
  def extensionInsert(op: Op, receiver: Ty): (String, Int) = (op.kind, receiver, op.params) match {
    case (OpKind.Tuple, Ty.Tuple(es), List(Ty.Fun(_, _))) =>
      val names = lambdaNames(es.size)
      val text  = s"${op.name}((${names.mkString(", ")}) => )"
      (text, text.length - 1)
    case _ if isSymbolic(op.name) =>
      // `fa.*>` → `fa *> |` (the handler removes the dot)
      val text = s"${op.name} "
      (text, text.length)
    case _ => op.insertTextAndCaret
  }

  private def lambdaNames(n: Int): List[String] =
    (0 until n).toList.map(i => if (i < 26) ('a' + i).toChar.toString else s"a$i")

  /** Reasons, for the quick doc: which instance was found for which type, and what is left to the argument. */
  def reasons(op: Op, subst: Ty.Subst, answer: Answer, fit: Option[Lens.Fit], oracle: LensOracle): List[String] = {
    val fromReceiver = op.requires.flatMap { case (v, tc) =>
      subst.get(v).map { t =>
        val rigid = t match {
          case Ty.Lambda(n, _, _, true) => Some(n)
          case Ty.Con(n, _, _, true)    => Some(n)
          case _                        => None
        }
        val how = rigid match {
          case Some(n) =>
            val bs = oracle.boundsOf(n)
            if (bs.isEmpty) "no bound on " + n + " in scope" else s"from the bound ${bs.toSeq.sorted.mkString(", ")} on $n"
          case None => if (answer == Answer.Unknown) "could not be verified" else "instance found"
        }
        s"$tc[${t.show}]: $how"
      }
    }
    val fromExpected = fit.toList.flatMap(_.evidence.map(e => s"$e: required by the expected type"))
    val leftOpen = op.requires.collect {
      case (v, tc) if subst.get(v).isEmpty && !fit.exists(_.subst.get(v).isDefined) => s"$tc[$v]: decided by the argument"
    }
    fromReceiver ++ fromExpected ++ leftOpen
  }
}
