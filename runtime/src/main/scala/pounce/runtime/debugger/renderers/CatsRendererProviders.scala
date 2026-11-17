package pounce.runtime.debugger.renderers

import com.intellij.debugger.impl.DebuggerUtilsAsync
import com.intellij.debugger.ui.tree.render.{ChildrenRenderer, CompoundRendererProvider, ValueLabelRenderer}
import com.sun.jdi.Type
import pounce.runtime.debugger.renderers.CatsLayouts.Fqn
import pounce.runtime.settings.PounceRuntimeSettings

import java.util.concurrent.CompletableFuture
import java.util.function.Function

/**
 * Debugger renderers for cats data types, registered through `com.intellij.debugger.compoundRendererProvider`.
 * They show up (and can be switched off one by one) in Settings → Build, Execution, Deployment → Debugger →
 * Data Views → Java Type Renderers; the Pounce "Runtime & Debugger" page switches them all.
 */
abstract class CatsRendererProvider(name: String, className: String) extends CompoundRendererProvider {

  protected def newRenderer(): CatsRendererBase

  private lazy val renderer = newRenderer()

  override protected def getName: String = s"Pounce: $name"

  override protected def getClassName: String = className

  override protected def getValueLabelRenderer: ValueLabelRenderer = renderer

  override protected def getChildrenRenderer: ChildrenRenderer = renderer

  override protected def isEnabled: Boolean = PounceRuntimeSettings().catsRenderers

  /** For tests / settings UI. */
  def displayName: String = getName

  def targetClassName: String = getClassName

  def enabledByDefault: Boolean = isEnabled
}

final class NonEmptyListRendererProvider extends CatsRendererProvider("cats.data.NonEmptyList", Fqn.NonEmptyList) {
  override protected def newRenderer(): CatsRendererBase = new NonEmptyListRenderer
}

final class NonEmptyVectorRendererProvider extends CatsRendererProvider("cats.data.NonEmptyVector", Fqn.NonEmptyVector) {
  override protected def newRenderer(): CatsRendererBase = new NonEmptyVectorRenderer
}

final class ChainRendererProvider extends CatsRendererProvider("cats.data.Chain / NonEmptyChain", Fqn.Chain) {
  override protected def newRenderer(): CatsRendererBase = new ChainRenderer
}

final class ValidatedRendererProvider extends CatsRendererProvider("cats.data.Validated", Fqn.Validated) {
  override protected def newRenderer(): CatsRendererBase = new PayloadRenderer(Fqn.Validated)
}

final class IorRendererProvider extends CatsRendererProvider("cats.data.Ior", Fqn.Ior) {
  override protected def newRenderer(): CatsRendererBase = new PayloadRenderer(Fqn.Ior)
}

final class EitherTRendererProvider extends CatsRendererProvider("cats.data.EitherT", Fqn.EitherT) {
  override protected def newRenderer(): CatsRendererBase = new PayloadRenderer(Fqn.EitherT)
}

final class OptionTRendererProvider extends CatsRendererProvider("cats.data.OptionT", Fqn.OptionT) {
  override protected def newRenderer(): CatsRendererBase = new PayloadRenderer(Fqn.OptionT)
}

/** `Ref.of[IO, A]` / `IO.ref` / `Ref[F].of` create a `SyncRef` (a plain AtomicReference); other Refs keep the default. */
final class RefRendererProvider extends CatsRendererProvider("cats.effect.Ref (current value)", Fqn.SyncRef) {
  override protected def newRenderer(): CatsRendererBase = new PayloadRenderer(Fqn.SyncRef)

  override protected def getIsApplicableChecker: Function[Type, CompletableFuture[java.lang.Boolean]] =
    tpe =>
      DebuggerUtilsAsync
        .instanceOf(tpe, Fqn.SyncRef)
        .thenCompose(isNew =>
          if (isNew) CompletableFuture.completedFuture(java.lang.Boolean.TRUE)
          else DebuggerUtilsAsync.instanceOf(tpe, Fqn.LegacySyncRef)
        )
}

object CatsRendererProviders {
  /** Every provider class registered in pounce-runtime.xml (kept in sync by RendererRegistrationTest). */
  val all: List[Class[? <: CatsRendererProvider]] = List(
    classOf[NonEmptyListRendererProvider],
    classOf[NonEmptyVectorRendererProvider],
    classOf[ChainRendererProvider],
    classOf[ValidatedRendererProvider],
    classOf[IorRendererProvider],
    classOf[EitherTRendererProvider],
    classOf[OptionTRendererProvider],
    classOf[RefRendererProvider]
  )
}
