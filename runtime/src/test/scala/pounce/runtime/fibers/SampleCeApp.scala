package pounce.runtime.fibers

import java.lang.invoke.{MethodHandles, MethodType}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/**
 * Child JVM main for [[FiberSnapshotRoundTripTest]]: starts cats-effect's global `IORuntime` with a few parked
 * fibers, prints `READY`, then exits on its own after the given number of milliseconds (a safety net: the test also
 * destroys it). cats-effect is not on the plugin build's classpath, so it is driven through reflection.
 */
object SampleCeApp {

  private def module(name: String): AnyRef = Class.forName(name + "$").getField("MODULE$").get(null)

  // MethodHandles resolve one method; Class.getMethod would load every signature of IO$ (incl. optional cats-mtl)
  private val lookup = MethodHandles.publicLookup()

  def main(args: Array[String]): Unit = {
    val lifetime = args.headOption.flatMap(_.toLongOption).getOrElse(60000L)

    val ioClass      = Class.forName("cats.effect.IO")
    val runtimeClass = Class.forName("cats.effect.unsafe.IORuntime")
    val runtimeObj   = module("cats.effect.unsafe.IORuntime")
    val io           = module("cats.effect.IO")

    val runtime = lookup.findVirtual(runtimeObj.getClass, "global", MethodType.methodType(runtimeClass)).invokeWithArguments(runtimeObj)
    val sleepMh = lookup.findVirtual(io.getClass, "sleep", MethodType.methodType(ioClass, classOf[FiniteDuration]))
    def sleep(): AnyRef = sleepMh.invokeWithArguments(io, FiniteDuration(1, TimeUnit.HOURS))
    val delay   = lookup.findVirtual(io.getClass, "apply", MethodType.methodType(ioClass, classOf[Function0[?]]))
    val flatMap = lookup.findVirtual(ioClass, "flatMap", MethodType.methodType(ioClass, classOf[Function1[?, ?]]))
    val runAndForget = lookup.findVirtual(ioClass, "unsafeRunAndForget", MethodType.methodType(Void.TYPE, runtimeClass))

    // sleeping fibers stay reachable through the runtime's timers, so the snapshot reports them as WAITING;
    // the delay + flatMap one carries trace events whose call site is this file
    val traced = flatMap.invokeWithArguments(delay.invokeWithArguments(io, () => 42), (_: Any) => sleep())
    Seq(sleep(), sleep(), traced).foreach(fiber => runAndForget.invokeWithArguments(fiber, runtime))

    Thread.sleep(500) // let the fibers park
    println("READY")
    System.out.flush()
    Thread.sleep(lifetime)
    System.exit(0)
  }
}
