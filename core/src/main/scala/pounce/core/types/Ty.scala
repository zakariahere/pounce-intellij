package pounce.core.types

import scala.annotation.tailrec

/**
 * A tiny type language, independent of IntelliJ, used for:
 *  - catalog shapes (`receiver = "F[A]"`, `result = "G[F[B]]"`),
 *  - the Combinator Finder (a Hoogle-style search: `List[A] => (A => F[B]) => F[List[B]]`),
 *  - hole filling / smart completion after converting real Scala types with [[ScTypeBridge]].
 *
 * Convention when parsing: single upper-case letters (optionally followed by digits) are type variables
 * (`A`, `B`, `E`, `F`, `G`, `A1`); everything else is a named type constructor (`List`, `IO`, `Either`, `Unit`).
 */
enum Ty {

  /** A type variable. Higher-kinded when used as the function part of [[Ty.App]]. */
  case Var(name: String)

  /**
   * A named type constructor applied to arguments: `List[Int]`, `IO[A]`, `Unit`.
   *
   * @param fqn   fully qualified name when known (from real Scala types); empty for catalog shapes
   * @param rigid true for a type *parameter* of user code (e.g. the `F` in `def f[F[_]: Sync]`): it must not be
   *              bound to anything else, and the instance oracle answers for it from the bounds in scope
   */
  case Con(name: String, args: List[Ty], fqn: String = "", rigid: Boolean = false)

  /** Application of a type variable: `F[A]`, `G[F[B]]`. */
  case App(fn: Var, args: List[Ty])

  case Fun(params: List[Ty], result: Ty)

  case Tuple(elems: List[Ty])

  /** A partially applied constructor, only ever produced as the binding of a higher-kinded variable: `Either[E, *]`. */
  case Lambda(name: String, fixed: List[Ty], fqn: String = "", rigid: Boolean = false)

  def show: String = Ty.show(this)

  def vars: Set[String] = this match {
    case Var(n)                => Set(n)
    case Con(_, args, _, _)    => args.flatMap(_.vars).toSet
    case App(f, args)          => args.flatMap(_.vars).toSet + f.name
    case Fun(ps, r)            => ps.flatMap(_.vars).toSet ++ r.vars
    case Tuple(es)             => es.flatMap(_.vars).toSet
    case Lambda(_, fixed, _, _) => fixed.flatMap(_.vars).toSet
  }

  /** Outer constructor name, if any (`List` for `List[Int]`, `F` for a rigid `F[A]`). */
  def headName: Option[String] = this match {
    case Con(n, _, _, _)    => Some(n)
    case Lambda(n, _, _, _) => Some(n)
    case Tuple(es)         => Some(s"Tuple${es.size}")
    case Fun(ps, _)        => Some(s"Function${ps.size}")
    case _                 => None
  }

  def headFqn: Option[String] = this match {
    case Con(_, _, fqn, _) if fqn.nonEmpty    => Some(fqn)
    case Lambda(_, _, fqn, _) if fqn.nonEmpty => Some(fqn)
    case _                                    => None
  }

  def renameVars(f: String => String): Ty = this match {
    case Var(n)                    => Var(f(n))
    case c @ Con(_, args, _, _)    => c.copy(args = args.map(_.renameVars(f)))
    case App(fn, args)             => App(Var(f(fn.name)), args.map(_.renameVars(f)))
    case Fun(ps, r)                => Fun(ps.map(_.renameVars(f)), r.renameVars(f))
    case Tuple(es)                 => Tuple(es.map(_.renameVars(f)))
    case l @ Lambda(_, fixed, _, _) => l.copy(fixed = fixed.map(_.renameVars(f)))
  }
}

object Ty {

  def isVarName(s: String): Boolean =
    s.nonEmpty && s.head.isUpper && s.length <= 3 && s.tail.forall(c => c.isDigit || c == '_') ||
      (s.length == 1 && s.head.isUpper)

  /** Names that look like variables but are well-known concrete types. */
  private val notVars = Set("IO")

  def show(t: Ty): String = t match {
    case Var(n)                      => n
    case Con(n, Nil, _, _)           => n
    case Con(n, args, _, _)          => s"$n[${args.map(show).mkString(", ")}]"
    case App(f, args)                => s"${f.name}[${args.map(show).mkString(", ")}]"
    case Fun(List(p @ (Fun(_, _))), r) => s"(${show(p)}) => ${show(r)}"
    case Fun(List(p), r)             => s"${showParam(p)} => ${show(r)}"
    case Fun(ps, r)                  => s"(${ps.map(show).mkString(", ")}) => ${show(r)}"
    case Tuple(es)                   => s"(${es.map(show).mkString(", ")})"
    case Lambda(n, Nil, _, _)        => n
    case Lambda(n, fixed, _, _)      => s"$n[${(fixed.map(show) :+ "*").mkString(", ")}]"
  }

  private def showParam(p: Ty): String = p match {
    case _: Tuple => s"(${show(p)})"
    case _        => show(p)
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Parser
  // ---------------------------------------------------------------------------------------------------------------

  final case class ParseError(msg: String, at: Int) extends Exception(s"$msg at $at")

  def parse(text: String, isVar: String => Boolean = n => isVarName(n) && !notVars(n)): Either[ParseError, Ty] =
    try {
      val p   = new Parser(text, isVar)
      val res = p.parseType()
      p.skipWs()
      if (!p.atEnd) Left(ParseError(s"Unexpected '${p.peekChar}'", p.pos)) else Right(res)
    } catch {
      case e: ParseError => Left(e)
    }

  def parseUnsafe(text: String): Ty = parse(text).fold(e => throw new IllegalArgumentException(s"$text: $e"), identity)

  private final class Parser(s: String, isVar: String => Boolean) {
    var pos = 0

    def atEnd: Boolean   = pos >= s.length
    def peekChar: Char   = s.charAt(pos)
    def skipWs(): Unit   = while (!atEnd && s.charAt(pos).isWhitespace) pos += 1
    def lookingAt(t: String): Boolean = { skipWs(); s.startsWith(t, pos) }
    def expect(t: String): Unit = {
      skipWs()
      if (!s.startsWith(t, pos)) throw ParseError(s"Expected '$t'", pos)
      pos += t.length
    }

    def parseType(): Ty = {
      val left = parseSimple()
      if (lookingAt("=>")) {
        expect("=>")
        val right = parseType()
        val params = left match {
          case ParamList(ps) => ps
          case other         => List(other)
        }
        Fun(params, right)
      } else
        left match {
          case ParamList(List(single)) => single
          case ParamList(ps)           => Tuple(ps)
          case other                   => other
        }
    }

    /** Marker for a parenthesised list, which becomes either function params or a tuple. */
    private object ParamList {
      def unapply(t: Ty): Option[List[Ty]] = t match {
        case Con("\u0000paren", args, _, _) => Some(args)
        case _                              => None
      }
    }

    def parseSimple(): Ty = {
      skipWs()
      if (atEnd) throw ParseError("Unexpected end of input", pos)
      if (lookingAt("(")) {
        expect("(")
        val elems =
          if (lookingAt(")")) Nil
          else {
            val b = List.newBuilder[Ty]
            b += parseType()
            while (lookingAt(",")) { expect(","); b += parseType() }
            b.result()
          }
        expect(")")
        Con("\u0000paren", elems)
      } else {
        val name = parseName()
        val args =
          if (lookingAt("[")) {
            expect("[")
            val b = List.newBuilder[Ty]
            b += parseType()
            while (lookingAt(",")) { expect(","); b += parseType() }
            expect("]")
            b.result()
          } else Nil
        if (isVar(name)) {
          if (args.isEmpty) Var(name) else App(Var(name), args.map(normalizeParen))
        } else Con(name, args.map(normalizeParen))
      }
    }

    private def normalizeParen(t: Ty): Ty = t match {
      case ParamList(List(x)) => x
      case ParamList(xs)      => Tuple(xs)
      case other              => other
    }

    def parseName(): String = {
      skipWs()
      val start = pos
      if (!atEnd && s.charAt(pos) == '*') { pos += 1; return "*" }
      while (!atEnd && (s.charAt(pos).isLetterOrDigit || s.charAt(pos) == '_' || s.charAt(pos) == '.')) pos += 1
      if (start == pos) throw ParseError(s"Expected a type name", pos)
      s.substring(start, pos)
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Substitution & unification
  // ---------------------------------------------------------------------------------------------------------------

  final case class Subst(map: Map[String, Ty]) {
    def get(v: String): Option[Ty] = map.get(v)

    def bind(v: String, t: Ty): Subst = Subst(map.updated(v, t))

    /** Fully applies the substitution, resolving chains and applying lambdas. */
    def apply(t: Ty): Ty = t match {
      case Var(n) =>
        map.get(n) match {
          case Some(Var(m)) if m == n => t
          case Some(bound)            => apply(bound)
          case None                   => t
        }
      case c @ Con(_, args, _, _) => c.copy(args = args.map(apply))
      case App(fn, args) =>
        val newArgs = args.map(apply)
        apply(fn) match {
          case Lambda(n, fixed, fqn, rigid) => Con(n, fixed.map(apply) ++ newArgs, fqn, rigid)
          case Var(m)                       => App(Var(m), newArgs)
          case Con(n, Nil, fqn, rigid)      => Con(n, newArgs, fqn, rigid)
          case other                        => App(Var(Ty.show(other)), newArgs)
        }
      case Fun(ps, r)                  => Fun(ps.map(apply), apply(r))
      case Tuple(es)                   => Tuple(es.map(apply))
      case l @ Lambda(_, fixed, _, _)  => l.copy(fixed = fixed.map(apply))
    }

    def show: String = map.toSeq.sortBy(_._1).map { case (k, v) => s"$k := ${Ty.show(v)}" }.mkString(", ")
  }

  object Subst {
    val empty: Subst = Subst(Map.empty)
  }

  /** Short-name equality, refined by FQN when both sides know theirs. */
  private def sameCon(n1: String, f1: String, n2: String, f2: String): Boolean =
    if (f1.nonEmpty && f2.nonEmpty) f1 == f2 || canonicalFqn(f1) == canonicalFqn(f2) else n1 == n2

  /** Scala's predef aliases point to the same class. */
  def canonicalFqn(fqn: String): String = fqn match {
    case "scala.List" | "scala.package.List"     => "scala.collection.immutable.List"
    case "scala.Vector" | "scala.package.Vector" => "scala.collection.immutable.Vector"
    case "scala.Seq" | "scala.package.Seq"       => "scala.collection.immutable.Seq"
    case "scala.Either" | "scala.package.Either" => "scala.util.Either"
    case "scala.Predef.String"                   => "java.lang.String"
    case "scala.Predef.Map"                      => "scala.collection.immutable.Map"
    case "scala.Predef.Set"                      => "scala.collection.immutable.Set"
    // cats.effect re-exports kernel types as aliases (`cats.effect.Resource` = `cats.effect.kernel.Resource`)
    case s"cats.effect.package.$n" if CatsEffectKernelAliases(n) => s"cats.effect.kernel.$n"
    case s"cats.effect.$n" if CatsEffectKernelAliases(n)         => s"cats.effect.kernel.$n"
    case other                                   => other
  }

  private val CatsEffectKernelAliases: Set[String] = Set(
    "Resource", "Ref", "Deferred", "Fiber", "Outcome", "Poll", "Clock", "Unique", "MonadCancel", "Spawn",
    "Concurrent", "GenTemporal", "Temporal", "Sync", "Async", "Cont", "ParallelF"
  )

  def unify(a: Ty, b: Ty, s: Subst = Subst.empty): Option[Subst] = {
    val a1 = resolveHead(a, s)
    val b1 = resolveHead(b, s)
    (a1, b1) match {
      case (Var(x), Var(y)) if x == y => Some(s)
      case (Var(x), t)                => bindVar(x, t, s)
      case (t, Var(y))                => bindVar(y, t, s)

      case (App(f, pargs), App(g, targs)) if pargs.size == targs.size =>
        unify(f, g, s).flatMap(unifyAll(pargs, targs, _))
      case (App(f, pargs), App(g, targs)) if targs.size > pargs.size =>
        None // would need partial application of a variable; not needed in practice

      case (App(f, pargs), c: Con) => unifyApp(f, pargs, c, s)
      case (c: Con, App(g, targs)) => unifyApp(g, targs, c, s)

      case (App(f, List(parg)), Tuple(es)) if es.nonEmpty =>
        bindVar(f.name, Lambda(s"Tuple${es.size}", es.init), s).flatMap(unify(parg, es.last, _))
      case (Tuple(es), App(g, List(targ))) if es.nonEmpty =>
        bindVar(g.name, Lambda(s"Tuple${es.size}", es.init), s).flatMap(unify(es.last, targ, _))

      case (App(f, List(parg)), Fun(ps, r)) =>
        bindVar(f.name, Lambda(s"Function${ps.size}", ps), s).flatMap(unify(parg, r, _))
      case (Fun(ps, r), App(g, List(targ))) =>
        bindVar(g.name, Lambda(s"Function${ps.size}", ps), s).flatMap(unify(r, targ, _))

      case (Con(n1, a1s, f1, _), Con(n2, a2s, f2, _)) if sameCon(n1, f1, n2, f2) && a1s.size == a2s.size =>
        unifyAll(a1s, a2s, s)
      case (Lambda(n1, a1s, f1, _), Lambda(n2, a2s, f2, _)) if sameCon(n1, f1, n2, f2) && a1s.size == a2s.size =>
        unifyAll(a1s, a2s, s)
      case (Lambda(n1, Nil, f1, _), Con(n2, Nil, f2, _)) if sameCon(n1, f1, n2, f2) => Some(s)
      case (Con(n1, Nil, f1, _), Lambda(n2, Nil, f2, _)) if sameCon(n1, f1, n2, f2) => Some(s)

      case (Fun(p1, r1), Fun(p2, r2)) if p1.size == p2.size =>
        unifyAll(p1, p2, s).flatMap(unify(r1, r2, _))
      case (Tuple(e1), Tuple(e2)) if e1.size == e2.size =>
        unifyAll(e1, e2, s)
      case _ => None
    }
  }

  private def unifyApp(f: Var, pargs: List[Ty], c: Con, s: Subst): Option[Subst] =
    if (c.args.size < pargs.size) None
    else {
      val (fixed, rest) = c.args.splitAt(c.args.size - pargs.size)
      bindVar(f.name, Lambda(c.name, fixed, c.fqn, c.rigid), s).flatMap(unifyAll(pargs, rest, _))
    }

  def unifyAll(as: List[Ty], bs: List[Ty], s: Subst): Option[Subst] =
    as.zip(bs).foldLeft(Option(s)) { case (acc, (x, y)) => acc.flatMap(unify(x, y, _)) }

  @tailrec
  private def resolveHead(t: Ty, s: Subst): Ty = t match {
    case Var(n) =>
      s.get(n) match {
        case Some(Var(m)) if m == n => t
        case Some(bound)            => resolveHead(bound, s)
        case None                   => t
      }
    case App(fn, args) =>
      s.get(fn.name) match {
        case Some(Lambda(n, fixed, fqn, rigid)) => Con(n, fixed ++ args, fqn, rigid)
        case Some(Var(m)) if m != fn.name       => resolveHead(App(Var(m), args), s)
        case Some(Con(n, Nil, fqn, rigid))      => Con(n, args, fqn, rigid)
        case _                                  => t
      }
    case other => other
  }

  private def bindVar(v: String, t: Ty, s: Subst): Option[Subst] =
    s.get(v) match {
      case Some(existing) => unify(existing, t, s)
      case None =>
        val resolved = s(t)
        if (resolved == Var(v)) Some(s)
        else if (resolved.vars.contains(v)) None // occurs check
        else Some(s.bind(v, resolved))
    }

  /**
   * Matches a pattern against a concrete type: like [[unify]] but variables of `target` are treated as rigid
   * constants (renamed apart so they can never capture pattern variables).
   */
  def matchShape(pattern: Ty, target: Ty): Option[Subst] = {
    val rigidTarget = rigidify(target)
    unify(pattern, rigidTarget)
  }

  private def rigidify(t: Ty): Ty = t match {
    case Var(n)                    => Con(n, Nil, rigid = true)
    case App(f, args)              => Con(f.name, args.map(rigidify), rigid = true)
    case c @ Con(_, args, _, _)    => c.copy(args = args.map(rigidify))
    case Fun(ps, r)                => Fun(ps.map(rigidify), rigidify(r))
    case Tuple(es)                 => Tuple(es.map(rigidify))
    case l @ Lambda(_, fixed, _, _) => l.copy(fixed = fixed.map(rigidify))
  }
}
