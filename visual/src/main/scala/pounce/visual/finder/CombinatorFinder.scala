package pounce.visual.finder

import pounce.core.StaticInstances
import pounce.core.StaticInstances.Answer
import pounce.core.Typeclasses
import pounce.core.catalog.{Catalog, Op, OpKind}
import pounce.core.types.Ty

/**
 * D6 model: a Hoogle for the catalog. "I have `List[A]`, I want `IO[List[B]]`" → `xs.traverse(???)`.
 *
 * Every op is viewed as a function (`Op.asFunction`: receiver first, then parameters). Its result is unified with
 * the wanted type, then each value we have is placed on one of its inputs (any order); inputs left over are holes
 * (`???`). One-step compositions (`have.op1(…).op2(…)`) are tried too, ranked below direct matches. Typeclass
 * constraints on bound variables are checked with `has` (the instance oracle in the IDE).
 *
 * Variables typed by the user (`A`, `B`, `F`) are rigid: a match must work for every `A`.
 */
object CombinatorFinder {

  /** A value we have: its name (for rendering) and type. */
  final case class Have(name: String, ty: Ty)

  final case class Step(op: Op, args: List[Option[String]])

  final case class Match(steps: List[Step], holes: List[Ty], usedHave: Int, answer: Answer, score: Int, expression: String) {
    def ops: List[Op] = steps.map(_.op)
    def signature: String = steps.map(_.op.signature).mkString("  ∘  ")
  }

  type Has = (String, Ty) => Answer

  /** Static-table instance check, for use without a PSI context. */
  val staticHas: Has = (tc, t) => {
    val c = Typeclasses.canonical(tc).getOrElse(tc)
    Typeclasses.byName.get(c).map(_.kind) match {
      case Some(Typeclasses.Kind.Proper) => StaticInstances.proper(c, t)
      case _                             => StaticInstances.higherKinded(c, t)
    }
  }

  /** Parses user input; single upper-case letters are (rigid) type variables. */
  def parseQuery(text: String): Either[String, Ty] =
    Ty.parse(text.trim).left.map(e => e.getMessage).map(rigidify)

  def rigidify(t: Ty): Ty = t match {
    case Ty.Var(n)                     => Ty.Con(n, Nil, rigid = true)
    case Ty.App(f, args)               => Ty.Con(f.name, args.map(rigidify), rigid = true)
    case c @ Ty.Con(_, args, _, _)     => c.copy(args = args.map(rigidify))
    case Ty.Fun(ps, r)                 => Ty.Fun(ps.map(rigidify), rigidify(r))
    case Ty.Tuple(es)                  => Ty.Tuple(es.map(rigidify))
    case l @ Ty.Lambda(_, fixed, _, _) => l.copy(fixed = fixed.map(rigidify))
  }

  private val MaxHoles = 2

  def search(have: List[Have], want: Ty, catalog: Catalog = Catalog.bundled, has: Has = staticHas, limit: Int = 40, compositions: Boolean = true): Seq[Match] = {
    val ops    = catalog.ops.filter(_.asFunction.isDefined)
    val direct = ops.flatMap(op => directMatches(op, have, want, has, mustUseAll = have.nonEmpty, maxHoles = MaxHoles))
    val composed =
      if (!compositions || have.isEmpty) Nil
      else ops.flatMap(op2 => composedMatches(op2, ops, have, want, has))
    (direct ++ composed)
      .groupBy(_.expression)
      .values
      .map(_.maxBy(_.score))
      .toSeq
      .sortBy(m => (-m.score, m.expression.length, m.expression))
      .take(limit)
  }

  private def fresh(op: Op, suffix: String): (List[Ty], Ty) = {
    val (ins, out) = op.asFunction.get
    (ins.map(_.renameVars(_ + suffix)), out.renameVars(_ + suffix))
  }

  /** Is the result just a bare variable (`A`)? Such ops match anything and only add noise. */
  private def tooGeneral(out: Ty): Boolean = out.isInstanceOf[Ty.Var]

  /**
   * `F[A] => F[B]` with `B` determined by nothing (`widen`, `foreverM`): the same effect comes back with a value
   * type that fits any wanted type (widen really needs `B >: A`, which the catalog cannot express). Free positions
   * in a different constructor are real polymorphism and stay: `x.asLeft[B]`, `x.asRight[E]`, `EitherT.liftF`.
   */
  private def resultPolymorphic(op: Op): Boolean =
    op.asFunction.exists { case (ins, out) =>
      val (head, last) = out match {
        case Ty.Con(n, args, _, _)     => (Some(n), args.lastOption)
        case Ty.App(Ty.Var(f), args)   => (Some(f), args.lastOption)
        case _                         => (None, None)
      }
      def sameHead(in: Ty): Boolean = (in, head) match {
        case (Ty.Con(n, _, _, _), Some(h))          => n == h
        case (Ty.App(Ty.Var(f), _), Some(h))        => f == h
        case _                                      => false
      }
      last.exists {
        case Ty.Var(v) => !ins.exists(_.vars.contains(v)) && ins.exists(sameHead)
        case _         => false
      }
    }

  /** The first input is a bare variable (`pure`, `some`, `IO.pure`, `Sync.delay`): it accepts any value. */
  private def acceptsAnything(op: Op): Boolean =
    op.asFunction.exists { case (ins, _) => ins.headOption.exists(_.isInstanceOf[Ty.Var]) }

  /**
   * Like [[search]] but the values are optional candidates (the hole filler: any of the values in scope may be used,
   * at most one hole left), without compositions.
   */
  def searchValues(values: List[Have], want: Ty, catalog: Catalog, has: Has, limit: Int = 40): Seq[Match] =
    catalog.ops
      .filter(_.asFunction.isDefined)
      .flatMap(op => directMatches(op, values, want, has, mustUseAll = false, maxHoles = 1))
      .groupBy(_.expression)
      .values
      .map(_.maxBy(_.score))
      .toSeq
      .sortBy(m => (-m.score, m.expression.length, m.expression))
      .take(limit)

  private def directMatches(op: Op, have: List[Have], want: Ty, has: Has, mustUseAll: Boolean, maxHoles: Int): Seq[Match] = {
    val (ins, out) = fresh(op, "'")
    if (tooGeneral(out) || resultPolymorphic(op)) return Nil
    Ty.unify(out, want).toSeq.flatMap { s0 =>
      assignments(ins, have, s0, mustUseAll, minUsed = if (have.isEmpty) 0 else 1, maxHoles).flatMap { case (args, s) =>
        val holes = ins.zip(args).collect { case (t, None) => s(t) }
        constraints(op, "'", s, has).map { answer =>
          val used  = args.count(_.isDefined)
          val score = used * 100 - holes.size * 25 + op.rank + (if (answer == Answer.Yes) 10 else 0)
          Match(List(Step(op, args)), holes, used, answer, score, render(op, args, s, "'"))
        }
      }
    }
  }

  private def composedMatches(op2: Op, ops: Seq[Op], have: List[Have], want: Ty, has: Has): Seq[Match] = {
    val (ins2, out2) = fresh(op2, "²")
    if (tooGeneral(out2) || ins2.isEmpty || op2.kind == OpKind.Companion || resultPolymorphic(op2)) return Nil
    Ty.unify(out2, want).toSeq.flatMap { s2 =>
      ops.flatMap { op1 =>
        // lifting the value first (`xs.pure.traverse(???)`) or a result-polymorphic first step only adds noise
        if ((op1 eq op2) || acceptsAnything(op1) || resultPolymorphic(op1) || op1.name.startsWith("raise")) Nil
        else {
          val (ins1, out1) = fresh(op1, "¹")
          if (tooGeneral(out1)) Nil
          else
            Ty.unify(out1, ins2.head, s2).toSeq.flatMap { s1 =>
              assignments(ins1, have, s1, mustUseAll = true, minUsed = 1, MaxHoles).flatMap { case (args1, s1b) =>
                val holes1 = ins1.zip(args1).collect { case (t, None) => s1b(t) }
                val rest   = ins2.tail
                val holes2 = rest.map(s1b(_))
                // a chain starts from a value we have (`xs.sequence.map(???)`), never from a hole (`???.as(x).map(???)`)
                if (holes1.size + holes2.size > MaxHoles || args1.headOption.flatten.isEmpty) Nil
                else
                  for {
                    a1 <- constraints(op1, "¹", s1b, has)
                    a2 <- constraints(op2, "²", s1b, has)
                  } yield {
                    val answer = a1 && a2
                    val expr1  = render(op1, args1, s1b, "¹")
                    val args2  = Some(paren(expr1, op1)) :: rest.map(_ => None)
                    val holes  = holes1 ++ holes2
                    val score  = have.size * 100 - holes.size * 25 + (op1.rank + op2.rank) / 2 - 60 + (if (answer == Answer.Yes) 10 else 0)
                    Match(List(Step(op1, args1), Step(op2, args2)), holes, have.size, answer, score, render(op2, args2, s1b, "²"))
                  }
              }
            }
        }
      }
    }
  }

  /**
   * Places each `have` on at most one input (injective), unifying as we go. Inputs left over become holes.
   * Returns the argument names per input (None = hole) with the final substitution.
   */
  def assignments(
    ins: List[Ty],
    have: List[Have],
    s0: Ty.Subst,
    mustUseAll: Boolean,
    minUsed: Int,
    maxHoles: Int
  ): List[(List[Option[String]], Ty.Subst)] = {
    def go(rest: List[Ty], unused: List[Have], s: Ty.Subst, acc: List[Option[String]], holes: Int): List[(List[Option[String]], Ty.Subst, List[Have])] =
      rest match {
        case Nil => List((acc.reverse, s, unused))
        case t :: tail =>
          val placed = unused.flatMap { h =>
            Ty.unify(t, h.ty, s).toList.flatMap(s1 => go(tail, unused.filterNot(_ eq h), s1, Some(h.name) :: acc, holes))
          }
          val hole = if (holes < maxHoles) go(tail, unused, s, None :: acc, holes + 1) else Nil
          placed ++ hole
      }
    go(ins, have, s0, Nil, 0).collect {
      case (args, s, unused) if (!mustUseAll || unused.isEmpty) && args.count(_.isDefined) >= minUsed => (args, s)
    }
  }

  /** Checks `requires` for variables bound by the match. `No` rejects; unbound variables are not checked. */
  private def constraints(op: Op, suffix: String, s: Ty.Subst, has: Has): Option[Answer] = {
    val answers = op.requires.flatMap { case (v, tc) =>
      val bound = s(Ty.Var(v + suffix))
      bound match {
        case Ty.Var(_) => None
        case t =>
          val kind = Typeclasses.byName.get(Typeclasses.canonical(tc).getOrElse(tc)).map(_.kind)
          val subject = kind match {
            case Some(Typeclasses.Kind.Proper) => t
            case _                              => asConstructor(t)
          }
          Some(has(tc, subject))
      }
    }
    val answer = (answers :+ errorType(op, suffix, s)).foldLeft(Answer.Yes)(_ && _)
    Option.when(answer != Answer.No)(answer)
  }

  // ---------------------------------------------------------------------------------------- error types (E)

  private val ErrorClass  = "ApplicativeError"
  private val ThrowableTy = Ty.Con("Throwable", Nil, "java.lang.Throwable")
  private val UnitTy      = Ty.Con("Unit", Nil, "scala.Unit")
  private val Throwing    = Set("IO", "SyncIO", "Future", "Try", "Resource", "Stream")
  private val Transformers = Set("Kleisli", "OptionT", "StateT", "IndexedStateT", "WriterT", "ReaderT")

  /** The variable naming the effect whose error type `E` must be: `F` of `requires { F = ApplicativeError }`. */
  private def errorEffectVar(op: Op): Option[String] =
    op.requires
      .collectFirst { case (v, tc) if Typeclasses.implied(tc).contains(ErrorClass) => v }
      .orElse(Option.when(Typeclasses.canonical(op.owner).exists(o => Typeclasses.implied(o).contains(ErrorClass)))("F"))

  /**
   * `raiseError` / `liftTo` / `attempt` / `handleError`: `ApplicativeError[F, E]` only exists for F's own error type
   * (`Throwable` for IO, `L` for `Either[L, *]` / `EitherT[M, L, *]`). Without this check `xs.raiseError` would
   * match every wanted `F[A]`, since its receiver is a bare `E`.
   */
  private def errorType(op: Op, suffix: String, s: Ty.Subst): Answer =
    errorEffectVar(op) match {
      case None => Answer.Yes
      case Some(fVar) =>
        (s(Ty.Var(fVar + suffix)), s(Ty.Var("E" + suffix))) match {
          case (Ty.Var(_), _) | (_, Ty.Var(_)) => Answer.Yes
          case (f, e) =>
            errorTypeOf(asConstructor(f)) match {
              case Some(ThrowableTy)                                  => throwable(e)
              case Some(expected) if Ty.unify(expected, e).isDefined => Answer.Yes
              case Some(_)                                            => Answer.No
              case None                                               => if (isRigid(e)) Answer.Unknown else throwable(e)
            }
        }
    }

  private def errorTypeOf(f: Ty): Option[Ty] = f match {
    case Ty.Lambda(n, _, _, false) if Throwing(n)                => Some(ThrowableTy)
    case Ty.Lambda("Option", Nil, _, false)                      => Some(UnitTy)
    case Ty.Lambda("Either", List(l), _, false)                  => Some(l)
    case Ty.Lambda("EitherT", List(_, l), _, false)              => Some(l)
    case Ty.Lambda(n, m :: _, _, false) if Transformers(n)       => errorTypeOf(asConstructor(m))
    case _                                                       => None
  }

  private def isRigid(t: Ty): Boolean = t match {
    case Ty.Con(_, Nil, _, true) => true
    case _                       => false
  }

  /** Names that are never Throwables (typed queries carry no FQN). */
  private val NotThrowable = Set(
    "List", "Vector", "Seq", "Set", "Map", "Option", "Either", "Chain", "NonEmptyList", "NonEmptyChain", "String",
    "Int", "Long", "Double", "Float", "Boolean", "Char", "Byte", "Short", "Unit", "BigInt", "BigDecimal", "IO",
    "SyncIO", "Resource", "Stream", "EitherT", "OptionT", "Kleisli", "Validated", "Ior", "Future", "Try", "Nothing"
  )

  /** Is `e` a Throwable? Yes for the usual names, No for types that certainly are not, Unknown for user classes. */
  private def throwable(e: Ty): Answer = e match {
    case Ty.Con(n, _, _, false) if n == "Throwable" || n == "Error" || n.endsWith("Exception") => Answer.Yes
    case Ty.Con(_, _, _, true)                                                                  => Answer.No
    case Ty.Con(n, _, _, false) if NotThrowable(n)                                              => Answer.No
    case Ty.Con(_, _, fqn, false) if Seq("scala.", "java.", "cats.", "fs2.").exists(fqn.startsWith) => Answer.No
    case Ty.Con(_, _, _, false)                                                                 => Answer.Unknown
    case _                                                                                       => Answer.No
  }

  /** A bound higher-kinded variable is a constructor already (`Lambda`/`Con` without args); keep as is. */
  private def asConstructor(t: Ty): Ty = t match {
    case Ty.Con(n, Nil, fqn, r) => Ty.Lambda(n, Nil, fqn, r)
    case other                  => other
  }

  private def paren(expr: String, op: Op): String =
    if (op.kind == OpKind.Companion || !expr.contains(' ')) expr else s"($expr)"

  /** `xs.traverse(f)`, `IO.blocking(???)`, `Sync[F].delay(???)`, `(fa, fb).mapN(???)`. */
  def render(op: Op, args: List[Option[String]], s: Ty.Subst, suffix: String): String = {
    val shown = args.map(_.getOrElse("???"))
    op.kind match {
      case OpKind.Companion =>
        val owner = op.requires.collectFirst { case (v, tc) if tc == op.owner => s(Ty.Var(v + suffix)) } match {
          case Some(t) if !t.isInstanceOf[Ty.Var] => s"${op.owner}[${t.show}]"
          case _                                  => op.owner
        }
        s"$owner.${op.name}${paramLists(op, shown)}"
      case _ =>
        val recv = shown.headOption.getOrElse("???")
        val r    = if (recv.contains(' ') && !recv.startsWith("(")) s"($recv)" else recv
        s"$r.${op.name}${paramLists(op, shown.drop(1))}"
    }
  }

  private def paramLists(op: Op, params: List[String]): String =
    if (params.isEmpty) {
      val (text, _) = op.insertTextAndCaret
      if (text.endsWith("()")) "()" else ""
    } else {
      val groups = op.insert.map(_.count(_ == '(')).getOrElse(1)
      if (groups == params.size && groups > 1) params.map(p => s"($p)").mkString
      else params.mkString("(", ", ", ")")
    }
}
