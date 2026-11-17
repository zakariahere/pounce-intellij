package pounce.runtime.debugger

import com.intellij.ui.classFilter.{ClassFilter, DebuggerClassFilterProvider}
import pounce.runtime.settings.PounceRuntimeSettings

import java.util
import scala.jdk.CollectionConverters.*

/**
 * Keeps Step Into out of library internals so it lands in user code (lambdas passed to `flatMap`, `map`,
 * `evalMap`, ...), not in the IO run loop. The filters become JDI class-exclusion filters of every step request.
 *
 * Lambdas the user writes compile into user classes (`com.acme.Foo$$Lambda...`, `Foo.$anonfun$...`), so they are
 * never matched by these patterns.
 *
 * Mirrors zio-intellij's `ZioSteppingFilterProvider` (Apache-2.0), with a curated pattern list.
 */
final class CatsSteppingFilterProvider extends DebuggerClassFilterProvider {
  override def getFilters: util.List[ClassFilter] =
    if (PounceRuntimeSettings().steppingFilters) CatsSteppingFilterProvider.filters.asJava
    else util.Collections.emptyList()
}

object CatsSteppingFilterProvider {

  final case class Pattern(pattern: String, why: String)

  /**
   * `cats.*` covers cats-core / cats-kernel (instances, syntax, data types) and all of cats-effect
   * (`cats.effect.IOFiber`, `cats.effect.kernel.*`, `cats.effect.unsafe.*`, `cats.effect.std.*`).
   * The Scala plugin already filters `scala.*` by default ("Do not step into specific Scala methods"); the explicit
   * function-wrapper patterns keep Step Into useful when users switch that option off.
   */
  val Patterns: List[Pattern] = List(
    Pattern("cats.*", "cats-core, cats-kernel and cats-effect: run loop, instances, syntax, data types"),
    Pattern("fs2.*", "fs2 streams: Pull interpreter, fs2.internal, chunk machinery"),
    Pattern("scala.runtime.*", "Scala runtime: boxing, lambda deserialization, function specializations"),
    Pattern("scala.Function*", "Scala function wrappers (andThen, compose, tupled, curried)"),
    Pattern("scala.PartialFunction*", "Scala partial function wrappers (orElse, lift, applyOrElse)")
  )

  private[debugger] def filters: List[ClassFilter] = Patterns.map(p => new ClassFilter(p.pattern))

  /** Same matching rule as IntelliJ's ClassFilter (`*` is a prefix/suffix wildcard): for tests and docs. */
  def isFiltered(className: String): Boolean = filters.exists(_.matches(className))
}
