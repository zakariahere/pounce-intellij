package pounce.visual.util

import com.intellij.openapi.util.Key
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager, PsiModificationTracker}
import com.intellij.psi.{PsiClass, PsiElement, PsiMember}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import pounce.core.types.{ScTypeBridge, Ty}

import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NonFatal

/** Small PSI / type helpers shared by the visual pillar. */
object VisualPsi {

  def safe[T](f: => T): Option[T] =
    try Option(f)
    catch {
      case e: com.intellij.openapi.progress.ProcessCanceledException => throw e
      case NonFatal(_)                                               => None
    }

  def typeOf(expr: ScExpression): Option[ScType] = safe(expr.`type`().toOption).flatten

  /** Dealiased (`full`) and alias-preserving (`raw`) views of an expression's type. */
  def tysOf(expr: ScExpression): Option[(Ty, Ty)] =
    typeOf(expr).flatMap { t =>
      for {
        full <- safe(ScTypeBridge.toTy(t, expr, dealias = true))
        raw  <- safe(ScTypeBridge.toTy(t, expr, dealias = false))
      } yield (full, raw)
    }

  def tyOf(expr: ScExpression): Option[Ty] = tysOf(expr).map(_._1)

  /** Fully qualified name of the class / object that declares what `ref` resolves to (`cats.effect.IO`). */
  def resolvedOwnerFqn(ref: ScReferenceExpression): Option[String] =
    safe(ref.resolve()).flatMap(ownerFqn)

  def ownerFqn(resolved: PsiElement): Option[String] = resolved match {
    case m: PsiMember =>
      Option(m.getContainingClass).flatMap {
        case o: ScObject => Option(o.qualifiedName)
        case c: PsiClass => Option(c.getQualifiedName).map(_.stripSuffix("$"))
      }
    case _ => None
  }

  /** Is `fqn` (an owner of a resolved member) part of cats / cats-effect / fs2? */
  def isTypelevel(fqn: String): Boolean =
    fqn.startsWith("cats.") || fqn.startsWith("fs2.")

  def shorten(text: String, max: Int = 40): String =
    if (text.length <= max) text else text.take(max - 1) + "…"

  /** Rigid type parameters (tagless `F`) mentioned in a type. */
  def rigidNames(t: Ty): List[String] = t match {
    case Ty.Con(n, args, _, true)      => n :: args.flatMap(rigidNames)
    case Ty.Con(_, args, _, false)     => args.flatMap(rigidNames)
    case Ty.Lambda(n, fixed, _, true)  => n :: fixed.flatMap(rigidNames)
    case Ty.Lambda(_, fixed, _, false) => fixed.flatMap(rigidNames)
    case Ty.App(_, args)               => args.flatMap(rigidNames)
    case Ty.Fun(ps, r)                 => ps.flatMap(rigidNames) ++ rigidNames(r)
    case Ty.Tuple(es)                  => es.flatMap(rigidNames)
    case Ty.Var(_)                     => Nil
  }

  /** Same head constructor (by canonical FQN, or rigid name)? Used to spot a step in another effect. */
  def sameHead(a: Ty, b: Ty): Boolean = (a, b) match {
    case (Ty.Con(n1, _, f1, r1), Ty.Con(n2, _, f2, r2)) =>
      if (r1 || r2) r1 == r2 && n1 == n2
      else if (f1.nonEmpty && f2.nonEmpty) Ty.canonicalFqn(f1) == Ty.canonicalFqn(f2)
      else n1 == n2
    case _ => true // unknown shapes: never claim a mismatch
  }

  private val FileCacheKey: Key[com.intellij.psi.util.CachedValue[ConcurrentHashMap[String, AnyRef]]] =
    Key.create("pounce.visual.fileCache")

  /**
   * Per-file memo dropped on any PSI change. The provider lambda captures only the project (capturing PSI leaks it,
   * which the test framework reports); the value is computed outside the lambda.
   */
  def fileCached[T <: AnyRef](place: PsiElement, key: String)(compute: => T): T = {
    val project = place.getProject
    val file    = place.getContainingFile
    if (file == null) return compute
    val holder = file.getOriginalFile
    val map = CachedValuesManager.getManager(project).getCachedValue(
      holder,
      FileCacheKey,
      () => CachedValueProvider.Result.create(new ConcurrentHashMap[String, AnyRef](), PsiModificationTracker.getInstance(project)),
      false
    )
    val existing = map.get(key)
    if (existing != null) existing.asInstanceOf[T]
    else {
      val v = compute
      map.put(key, v)
      v
    }
  }
}
