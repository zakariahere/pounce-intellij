package pounce.runtime.fibers

import com.sun.tools.attach.{VirtualMachine, VirtualMachineDescriptor}

import javax.management.ObjectName
import javax.management.remote.{JMXConnectorFactory, JMXServiceURL}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Fetches a cats-effect live fiber snapshot from a local JVM: Attach API → local JMX agent → MBean.
 * Plain JDK code (`jdk.attach`, `java.management`), no IntelliJ dependency, so it can be exercised outside the IDE.
 *
 * Verified against cats-effect 3.6.3 class files: every `IORuntime(...)` (IOApp's runtime, `IORuntime.global`)
 * calls `IORuntimeCompanionPlatform.registerFiberMonitorMBean`, which registers a
 * `cats.effect.unsafe.metrics.LiveFiberSnapshotTrigger` standard MBean (operation `liveFiberSnapshot(): String[]`)
 * under `cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-<n>` — but only when
 * `TracingConstants.isStackTracing`, i.e. system property `cats.effect.tracing.mode` is `cached` (the default) or
 * `full`. With `-Dcats.effect.tracing.mode=none` (or cats-effect < 3.6) there is nothing to call.
 */
object FiberSnapshotClient {

  val Domain: String = "cats.effect.unsafe.metrics"

  /** The ObjectName key prefix CE uses; the suffix is a per-runtime counter (`-0`, `-1`, ...). */
  val TriggerTypePrefix: String = "LiveFiberSnapshotTrigger-"

  val Operation: String = "liveFiberSnapshot"

  /** Pattern matching every runtime's trigger in a JVM. */
  val TriggerPattern: ObjectName = new ObjectName(s"$Domain:type=$TriggerTypePrefix*")

  def isTrigger(name: ObjectName): Boolean =
    name.getDomain == Domain && Option(name.getKeyProperty("type")).exists(_.startsWith(TriggerTypePrefix))

  final case class Jvm(pid: String, displayName: String)

  final case class RuntimeSnapshot(objectName: String, chunks: Seq[String])

  enum Problem(val message: String) {
    case AttachFailed(pid: String, cause: String)
        extends Problem(s"Could not attach to JVM $pid: $cause")
    case AgentFailed(pid: String, cause: String)
        extends Problem(s"Could not start the local JMX agent in JVM $pid: $cause")
    case NoMBean(pid: String)
        extends Problem(s"JVM $pid has no cats-effect fiber snapshot MBean ($Domain:type=$TriggerTypePrefix*)")
    case CallFailed(pid: String, cause: String)
        extends Problem(s"Fiber snapshot call failed in JVM $pid: $cause")
  }

  /** Local JVMs visible to the Attach API (same user), minus the calling process. */
  def localJvms(): List[Jvm] =
    try {
      val self = ProcessHandle.current().pid().toString
      VirtualMachine.list().asScala.toList.filterNot(_.id() == self).map(d => Jvm(d.id(), describe(d)))
    } catch { case NonFatal(_) => Nil }

  private def describe(d: VirtualMachineDescriptor): String = Option(d.displayName()).filter(_.nonEmpty).getOrElse("<unknown>")

  /** Attaches, reads every runtime's snapshot, detaches. Blocking: call from a background thread. */
  def fetch(pid: String): Either[Problem, List[RuntimeSnapshot]] = {
    val vm =
      try VirtualMachine.attach(pid)
      catch { case NonFatal(e) => return Left(Problem.AttachFailed(pid, describeError(e))) }
    try {
      val address =
        try vm.startLocalManagementAgent()
        catch { case NonFatal(e) => return Left(Problem.AgentFailed(pid, describeError(e))) }
      fetchFromJmx(pid, address)
    } finally
      try vm.detach()
      catch { case NonFatal(_) => () }
  }

  /** Reads the snapshots through an already known JMX service URL. */
  def fetchFromJmx(pid: String, serviceUrl: String): Either[Problem, List[RuntimeSnapshot]] =
    try {
      val connector = JMXConnectorFactory.connect(new JMXServiceURL(serviceUrl))
      try {
        val connection = connector.getMBeanServerConnection
        val names      = connection.queryNames(TriggerPattern, null).asScala.toList.filter(isTrigger).sortBy(_.toString)
        if (names.isEmpty) Left(Problem.NoMBean(pid))
        else
          Right(names.map { name =>
            val result = connection.invoke(name, Operation, Array.empty[AnyRef], Array.empty[String])
            val chunks = result match {
              case arr: Array[String] => arr.toSeq
              case other              => Seq(String.valueOf(other))
            }
            RuntimeSnapshot(name.toString, chunks)
          })
      } finally connector.close()
    } catch { case NonFatal(e) => Left(Problem.CallFailed(pid, describeError(e))) }

  private def describeError(e: Throwable): String =
    Iterator.iterate(e)(_.getCause).takeWhile(_ != null).take(4).map(t => Option(t.getMessage).getOrElse(t.getClass.getSimpleName)).distinct.mkString(": ")

  /** Advice shown when the MBean is missing. */
  val EnableAdvice: String =
    "The fiber snapshot MBean is registered by cats-effect 3.6+ for every IORuntime when stack tracing is on " +
      "(the default, -Dcats.effect.tracing.mode=cached; 'full' also works). Check that the application uses " +
      "cats-effect 3.6 or newer, does not run with -Dcats.effect.tracing.mode=none, and has already created its " +
      "IORuntime (IOApp.run started, or IORuntime.global touched)."
}
