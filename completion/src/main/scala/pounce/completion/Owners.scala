package pounce.completion

import com.intellij.psi.{PsiClass, PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import pounce.core.Typeclasses

import scala.util.control.NonFatal

/**
 * Catalog owners (`IO`, `Resource`, `EitherT`, `Sync`, ...) ↔ the Scala symbols they stand for: which companion
 * object FQNs mean "this owner" (for `IO.` completion) and which import brings the owner's name into scope.
 */
object Owners {

  /** @param objects companion object / typeclass FQNs (as the Scala plugin reports them) @param importPath what to import */
  final case class OwnerInfo(owner: String, objects: Set[String], importPath: String)

  private def ce(name: String)    = OwnerInfo(name, Set(s"cats.effect.kernel.$name", s"cats.effect.$name"), s"cats.effect.$name")
  private def std(name: String)   = OwnerInfo(name, Set(s"cats.effect.std.$name"), s"cats.effect.std.$name")
  private def data(name: String)  = OwnerInfo(name, Set(s"cats.data.$name"), s"cats.data.$name")
  private def cats(name: String)  = OwnerInfo(name, Set(s"cats.$name"), s"cats.$name")

  val all: List[OwnerInfo] = List(
    OwnerInfo("IO", Set("cats.effect.IO"), "cats.effect.IO"),
    OwnerInfo("SyncIO", Set("cats.effect.SyncIO"), "cats.effect.SyncIO"),
    ce("Resource"),
    ce("Ref"),
    ce("Deferred"),
    ce("Fiber"),
    ce("Clock"),
    ce("Sync"),
    ce("Async"),
    OwnerInfo("Temporal", Set("cats.effect.kernel.GenTemporal", "cats.effect.kernel.Temporal", "cats.effect.Temporal"), "cats.effect.Temporal"),
    OwnerInfo("Concurrent", Set("cats.effect.kernel.GenConcurrent", "cats.effect.kernel.Concurrent", "cats.effect.Concurrent"), "cats.effect.Concurrent"),
    OwnerInfo("Spawn", Set("cats.effect.kernel.GenSpawn", "cats.effect.kernel.Spawn", "cats.effect.Spawn"), "cats.effect.Spawn"),
    ce("MonadCancel"),
    std("Queue"),
    std("Semaphore"),
    std("Supervisor"),
    std("Dispatcher"),
    std("Random"),
    std("Console"),
    std("CountDownLatch"),
    std("CyclicBarrier"),
    std("AtomicCell"),
    std("Mutex"),
    std("Hotswap"),
    std("UUIDGen"),
    data("EitherT"),
    data("OptionT"),
    data("Kleisli"),
    data("NonEmptyList"),
    data("NonEmptyVector"),
    OwnerInfo("NonEmptyChain", Set("cats.data.NonEmptyChainImpl", "cats.data.NonEmptyChain"), "cats.data.NonEmptyChain"),
    data("Validated"),
    data("Ior"),
    data("Chain"),
    data("State"),
    OwnerInfo("Stream", Set("fs2.Stream"), "fs2.Stream"),
    OwnerInfo("CompileOps", Set("fs2.Stream.CompileOps"), "fs2.Stream"),
    cats("Applicative"),
    cats("ApplicativeError"),
    cats("MonadError"),
    cats("Monad"),
    cats("Functor"),
    cats("Traverse"),
    cats("Parallel")
  )

  private val byOwner: Map[String, OwnerInfo] = all.map(o => o.owner -> o).toMap
  private val byObject: Map[String, String]   = all.flatMap(o => o.objects.map(_ -> o.owner)).toMap

  def info(owner: String): Option[OwnerInfo] = byOwner.get(owner)

  /** Owner for a companion object / class FQN (`cats.effect.IO` → `IO`, `cats.effect.kernel.GenTemporal` → `Temporal`). */
  def ownerOf(fqn: String): Option[String] = byObject.get(fqn.stripSuffix("$"))

  /** Typeclass owner names (canonical) implied by a typeclass FQN: `Async` → Async, Sync, Temporal, Concurrent, ... */
  def typeclassOwners(fqn: String): Set[String] =
    if (!fqn.startsWith("cats.")) Set.empty
    else Typeclasses.canonical(fqn).map(Typeclasses.implied).getOrElse(Set.empty)

  /** Is `shortName` usable at `place` (resolves to anything)? Then importing it again could clash, so we don't. */
  def isResolvable(shortName: String, place: PsiElement): Boolean =
    try {
      ScalaPsiElementFactory.createExpressionWithContextFromText(shortName, place, null) match {
        case ref: ScReferenceExpression => ref.resolve() != null
        case _                          => false
      }
    } catch {
      case NonFatal(_) => false
    }

  /** Adds `import <fqn>` unless the short name already resolves at `place`. Call inside a write action. */
  def ensureImported(fqn: String, place: PsiElement): Unit = {
    val short = fqn.split('.').last
    if (!isResolvable(short, place)) {
      val holder = ScImportsHolder.forNewImportInsertion(place)
      holder.addImportForPath(fqn, place)
    }
  }

  /** FQN of a resolved companion-like element: an object, a class, or a `val X = cats.effect.kernel.X` alias. */
  def fqnOfResolved(e: PsiNamedElement): Option[String] = e match {
    case o: ScObject => Option(o.qualifiedName)
    case c: PsiClass => Option(c.getQualifiedName).map(_.stripSuffix("$"))
    case _           => None
  }

  /** Text that refers to the owner in source: `IO`, or `Sync[F]` for a typeclass summoner. */
  def summonerText(owner: String, typeParam: Option[String]): String =
    typeParam.filter(_ => Typeclasses.byName.contains(owner)).map(f => s"$owner[$f]").getOrElse(owner)

  private[completion] def isTypeclass(owner: String): Boolean = Typeclasses.byName.contains(owner)
}
