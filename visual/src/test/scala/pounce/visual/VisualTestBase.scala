package pounce.visual

import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import pounce.testing.PounceFixtureTestBase

/**
 * The core fixture loads cats-core and cats-effect non-transitively, so `cats.kernel.*` (Monoid, Eq, ...) and
 * `cats.effect.kernel.*` (Sync, Async, the IO instances' types) do not resolve. The visual features read resolved
 * types and implicits, so these tests add the kernel / std modules a real sbt project has on its classpath.
 */
abstract class VisualTestBase extends PounceFixtureTestBase {

  // cats-kernel / cats-effect-kernel / cats-effect-std now come from PounceFixtureTestBase.
}
