package pounce.runtime.fibers

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.runtime.fibers.FiberSnapshotClient.Problem

import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import javax.management.remote.JMXConnectorServerFactory
import javax.management.{MBeanServerFactory, ObjectName, StandardMBean}
import javax.management.remote.JMXServiceURL

/** Standard MBean with cats-effect's `LiveFiberSnapshotTrigger` shape (interface name = class name + `MBean`). */
trait FakeSnapshotTriggerMBean {
  def liveFiberSnapshot(): Array[String]
}

final class FakeSnapshotTrigger(chunks: Array[String]) extends FakeSnapshotTriggerMBean {
  override def liveFiberSnapshot(): Array[String] = chunks
}

final class FailingSnapshotTrigger extends FakeSnapshotTriggerMBean {
  override def liveFiberSnapshot(): Array[String] = throw new IllegalStateException("runtime shut down")
}

/**
 * F3 round trips: JMX client code against an in-process MBean server, and the whole Attach → local JMX agent → MBean
 * path against a real cats-effect 3.6.3 runtime in a short-lived child JVM (destroyed in `finally`).
 */
class FiberSnapshotRoundTripTest extends TestCase {

  private def withJmx(beans: (String, AnyRef)*)(body: String => Unit): Unit = {
    val mbs = MBeanServerFactory.newMBeanServer()
    beans.foreach((name, bean) => mbs.registerMBean(bean, new ObjectName(name)))
    // RMI connector without a registry: the stub is encoded in the URL, port chosen by the OS
    val server = JMXConnectorServerFactory.newJMXConnectorServer(new JMXServiceURL("service:jmx:rmi://127.0.0.1"), null, mbs)
    server.start()
    try body(server.getAddress.toString)
    finally server.stop()
  }

  def testFetchesEveryRuntimeThroughJmx(): Unit =
    withJmx(
      "cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-1" -> new FakeSnapshotTrigger(Array("cats.effect.IOFiber@1 WAITING\n")),
      "cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-0" -> new FakeSnapshotTrigger(Array("cats.effect.IOFiber@a RUN", "NING\n")),
      "cats.effect.unsafe.metrics:type=ComputePoolSampler-0"       -> new FakeSnapshotTrigger(Array("not a trigger"))
    ) { url =>
      FiberSnapshotClient.fetchFromJmx("test", url) match {
        case Right(snapshots) =>
          assertEquals(
            List("cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-0", "cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-1"),
            snapshots.map(_.objectName)
          )
          assertEquals(Seq("cats.effect.IOFiber@a RUN", "NING\n"), snapshots.head.chunks)
          assertEquals(List(FiberSnapshot.State.Running), FiberSnapshot.parse(snapshots.head.chunks).fibers.map(_.state))
        case Left(p) => fail(p.message)
      }
    }

  def testReportsMissingMBean(): Unit =
    withJmx("cats.effect.unsafe.metrics:type=ComputePoolSampler-0" -> new FakeSnapshotTrigger(Array.empty)) { url =>
      assertEquals(Left(Problem.NoMBean("test")), FiberSnapshotClient.fetchFromJmx("test", url))
    }

  def testReportsFailingCall(): Unit =
    withJmx("cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-0" -> new StandardMBean(new FailingSnapshotTrigger, classOf[FakeSnapshotTriggerMBean])) { url =>
      FiberSnapshotClient.fetchFromJmx("test", url) match {
        case Left(p: Problem.CallFailed) => assertTrue(p.message, p.message.contains("runtime shut down"))
        case other                       => fail(s"expected CallFailed, got $other")
      }
    }

  def testAttachToUnknownPidFails(): Unit =
    FiberSnapshotClient.fetch("999999999") match {
      case Left(_: Problem.AttachFailed) => ()
      case other                         => fail(s"expected AttachFailed, got $other")
    }

  // ------------------------------------------------------------------ real cats-effect runtime in a child JVM

  private val CeJars = Seq(
    "cats-kernel"        -> "2.13.0",
    "cats-core"          -> "2.13.0",
    "cats-effect-kernel" -> "3.6.3",
    "cats-effect-std"    -> "3.6.3",
    "cats-effect"        -> "3.6.3"
  )

  /** The jars the fixture tests already resolve through Ivy (`~/.ivy2/cache`). */
  private def ceClasspath: Option[Seq[Path]] = {
    val cache = Path.of(System.getProperty("user.home"), ".ivy2", "cache", "org.typelevel")
    val jars  = CeJars.map((a, v) => cache.resolve(s"${a}_2.13").resolve("jars").resolve(s"${a}_2.13-$v.jar"))
    Option.when(jars.forall(Files.isRegularFile(_)))(jars)
  }

  private def locationOf(c: Class[?]): Option[Path] =
    Option(c.getProtectionDomain.getCodeSource).flatMap(cs => Option(cs.getLocation)).map(u => Path.of(u.toURI))

  def testRealCatsEffectRuntimeRoundTrip(): Unit = {
    val classpath = for {
      ce     <- ceClasspath
      own    <- locationOf(classOf[FakeSnapshotTrigger])
      scala2 <- locationOf(classOf[scala.Option[?]])
      scala3 <- locationOf(classOf[scala.deriving.Mirror])
    } yield (own +: scala2 +: scala3 +: ce).distinct.mkString(File.pathSeparator)
    if (classpath.isEmpty) {
      println("[pounce-roundtrip] SKIPPED: cats-effect 3.6.3 jars not in ~/.ivy2/cache or class locations unknown")
      return
    }
    val java = Path.of(System.getProperty("java.home"), "bin", if (File.separatorChar == '\\') "java.exe" else "java")
    val process = new ProcessBuilder(
      java.toString,
      "-Xmx128m",
      "-Dcats.effect.tracing.mode=cached",
      "-cp",
      classpath.get,
      "pounce.runtime.fibers.SampleCeApp",
      "90000"
    ).redirectErrorStream(true).start()
    val output = new LinkedBlockingQueue[String]()
    val reader = new Thread(() => {
      val in = new BufferedReader(new InputStreamReader(process.getInputStream))
      try Iterator.continually(in.readLine()).takeWhile(_ != null).foreach(output.put)
      catch { case _: Exception => () }
    })
    reader.setDaemon(true)
    reader.start()
    try {
      val seen     = new StringBuilder
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60)
      var ready    = false
      while (!ready && System.nanoTime() < deadline && (process.isAlive || !output.isEmpty)) {
        Option(output.poll(200, TimeUnit.MILLISECONDS)).foreach { line =>
          seen.append(line).append('\n')
          ready = line == "READY"
        }
      }
      assertTrue(s"child JVM did not get ready:\n$seen", ready)

      val pid = process.pid().toString
      assertTrue("Attach API should list the child JVM", FiberSnapshotClient.localJvms().exists(_.pid == pid))

      FiberSnapshotClient.fetch(pid) match {
        case Left(p) => fail(p.message)
        case Right(snapshots) =>
          assertFalse("no LiveFiberSnapshotTrigger MBean", snapshots.isEmpty)
          snapshots.foreach(s => assertTrue(s.objectName, FiberSnapshotClient.isTrigger(new ObjectName(s.objectName))))
          val snapshot = FiberSnapshot.parse(snapshots.head.chunks)
          println(s"[pounce-roundtrip] ${snapshots.map(_.objectName).mkString(", ")}")
          println(FiberSnapshot.render(snapshot, Seq("[pounce-roundtrip] child JVM fiber snapshot")).linesIterator.map("[pounce-roundtrip] " + _).mkString("\n"))
          assertTrue(s"expected the parked fibers, got:\n${snapshots.head.chunks.mkString}", snapshot.fibers.size >= 3)
          assertTrue(snapshot.fibers.exists(_.state == FiberSnapshot.State.Waiting))
          assertTrue("worker threads of the global compute pool", snapshot.workers.nonEmpty)
          // trace frames point back at the child's own code (cached tracing, the default)
          val frames = snapshot.fibers.flatMap(_.frames)
          assertTrue(s"no frame in SampleCeApp: $frames", frames.exists(f => f.className.contains("SampleCeApp") && f.file.contains("SampleCeApp.scala")))
      }
    } finally {
      process.destroyForcibly()
      process.waitFor(20, TimeUnit.SECONDS)
    }
  }
}
