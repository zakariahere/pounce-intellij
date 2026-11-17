package pounce.runtime.fibers

import junit.framework.TestCase
import org.junit.Assert.*
import pounce.runtime.fibers.FiberSnapshot.*

import javax.management.ObjectName

/** F3: parsing, grouping and rendering of cats-effect live fiber snapshots; the MBean coordinates. */
class FiberSnapshotTest extends TestCase {

  /** The shape `FiberMonitor.liveFiberSnapshot` produces (cats-effect 3.6.3), with a JDK 9+ loader prefix frame. */
  private val Sample =
    """cats.effect.IOFiber@56824a14 RUNNING
      | ├ flatMap @ com.acme.Main$.$anonfun$run$1(Main.scala:12)
      | ╰ delay @ com.acme.Main$.run(Main.scala:10)
      |
      |Thread[#31,io-compute-3,5,main] (#3): 2 enqueued
      |cats.effect.IOFiber@1a2b3c YIELDING
      | ╰ sleep @ app//com.acme.Worker$.loop(Worker.scala:20)
      |
      |cats.effect.IOFiber@ff00ff WAITING
      | ├ async_ @ com.acme.Server$.serve(Server.scala:33)
      | ╰ map @ com.acme.Server$.<init>(Server.scala:5)
      |
      |cats.effect.IOFiber@ABCDEF BLOCKED
      | ╰ blocking @ com.acme.Db$.query(Db.scala:7)
      |
      |cats.effect.IOFiber@def0 WAITING
      |
      |Thread[io-compute-0,5,main] (#0): 0 enqueued
      |Global: enqueued 0, foreign 1, waiting 2
      |""".stripMargin

  private lazy val snapshot = parseText(Sample)

  def testParsesEveryFiberWithItsState(): Unit = {
    assertEquals(List("56824a14", "1a2b3c", "ff00ff", "abcdef", "def0"), snapshot.fibers.map(_.id))
    assertEquals(
      List(State.Running, State.Yielding, State.Waiting, State.Blocked, State.Waiting),
      snapshot.fibers.map(_.state)
    )
  }

  def testParsesFrames(): Unit = {
    val first = snapshot.fibers.head
    assertEquals(
      List(
        Frame(Some("flatMap"), "com.acme.Main$", "$anonfun$run$1", Some("Main.scala"), Some(12)),
        Frame(Some("delay"), "com.acme.Main$", "run", Some("Main.scala"), Some(10))
      ),
      first.frames
    )
    assertEquals(2, first.rawTrace.size)
    // `app//` (class loader / module prefix of StackTraceElement.toString) is skipped
    assertEquals(Some("com.acme.Worker$"), snapshot.fibers(1).frames.headOption.map(_.className))
    assertEquals(Some("<init>"), snapshot.fibers(2).frames.lift(1).map(_.method))
    assertTrue(snapshot.fibers.last.frames.isEmpty)
  }

  def testParsesWorkersAndGlobalCounters(): Unit = {
    assertEquals(List(Worker("Thread[#31,io-compute-3,5,main]", 3, 2), Worker("Thread[io-compute-0,5,main]", 0, 0)), snapshot.workers)
    assertEquals(Some(Global(0, 1, 2)), snapshot.global)
    assertEquals("io-compute-3", threadName("Thread[#31,io-compute-3,5,main]"))
    assertEquals("io-compute-0", threadName("Thread[io-compute-0,5,main]"))
  }

  def testGroupsByStateInSeverityOrder(): Unit = {
    assertEquals(List(State.Running, State.Blocked, State.Yielding, State.Waiting), snapshot.byState.map(_._1))
    assertEquals(List("ff00ff", "def0"), snapshot.byState.last._2.map(_.id))
    assertEquals("RUNNING 1, BLOCKED 1, YIELDING 1, WAITING 2", snapshot.counts)
  }

  def testWindowsLineSeparatorsAndChunkBoundaries(): Unit = {
    val crlf = Sample.replace("\n", "\r\n")
    assertEquals(snapshot, parseText(crlf))
    // the MBean returns the report in chunks that may split a line
    assertEquals(snapshot, parse(Seq(crlf.take(37), crlf.slice(37, 120), crlf.drop(120))))
  }

  def testEmptyAndForeignInput(): Unit = {
    assertTrue(parseText("").isEmpty)
    assertTrue(parseText("no fibers here\n ╰ not a fiber frame\n").isEmpty)
  }

  def testParseFrameEdgeCases(): Unit = {
    assertEquals(Some(Frame(None, "com.acme.X", "y", Some("X.scala"), Some(1))), parseFrame("com.acme.X.y(X.scala:1)"))
    assertEquals(Some(Frame(Some("println"), "Foo", "bar", None, None)), parseFrame("println @ Foo.bar(Unknown Source)"))
    assertEquals(Some(Frame(Some("map"), "a.B", "c", Some("B.scala"), None)), parseFrame("map @ a.B.c(B.scala)"))
    assertEquals(Some("org.acme.Lib"), parseFrame("map @ acme@2.1/org.acme.Lib.test(Lib.java:80)").map(_.className))
    assertEquals(None, parseFrame("hello world"))
  }

  def testRendersJvmStackTraceSyntax(): Unit = {
    val text = render(snapshot, Seq("Cats Effect fiber snapshot: pid 42"))
    assertTrue(text, text.startsWith("Cats Effect fiber snapshot: pid 42\nFibers: 5 (RUNNING 1, BLOCKED 1, YIELDING 1, WAITING 2)\n"))
    assertTrue(text, text.contains("Global: enqueued 0, foreign 1, waiting 2\n"))
    assertTrue(text, text.contains("Workers:\n  io-compute-0 (#0): 0 enqueued\n  io-compute-3 (#3): 2 enqueued\n"))
    assertTrue(text, text.contains("\n=== WAITING (2) ===\n"))
    assertTrue(text, text.contains("\"IOFiber@56824a14\" RUNNING\n\tat com.acme.Main$.$anonfun$run$1(Main.scala:12) [flatMap]\n"))
    assertTrue(text, text.contains("\"IOFiber@def0\" WAITING\n\t(no trace"))
    // grouped: RUNNING section before WAITING section
    assertTrue(text.indexOf("=== RUNNING") < text.indexOf("=== BLOCKED") && text.indexOf("=== BLOCKED") < text.indexOf("=== WAITING"))
  }

  def testFiberTitleAndStackTrace(): Unit = {
    val f = snapshot.fibers.head
    assertEquals("IOFiber@56824a14  (flatMap @ Main.scala:12)", f.title)
    assertEquals("IOFiber@def0", snapshot.fibers.last.title)
    assertEquals(
      "\"IOFiber@56824a14\" RUNNING\n\tat com.acme.Main$.$anonfun$run$1(Main.scala:12) [flatMap]\n\tat com.acme.Main$.run(Main.scala:10) [delay]",
      fiberStackTrace(f)
    )
    assertEquals("\tat a.B.c(Unknown Source)", Frame(None, "a.B", "c", None, None).jvmLine)
  }

  // ------------------------------------------------------------------ MBean coordinates

  def testObjectNameConstantsMatchCatsEffect(): Unit = {
    // cats-effect 3.6.3: IORuntimeCompanionPlatform.registerFiberMonitorMBean
    assertEquals("cats.effect.unsafe.metrics", FiberSnapshotClient.Domain)
    assertEquals("liveFiberSnapshot", FiberSnapshotClient.Operation)
    assertTrue(FiberSnapshotClient.TriggerPattern.isPattern)
    assertEquals("cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-*", FiberSnapshotClient.TriggerPattern.toString)
  }

  def testTriggerNamesAreRecognised(): Unit = {
    def on(s: String) = new ObjectName(s)
    Seq("cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-0", "cats.effect.unsafe.metrics:type=LiveFiberSnapshotTrigger-17")
      .foreach { n =>
        assertTrue(n, FiberSnapshotClient.isTrigger(on(n)))
        assertTrue(n, FiberSnapshotClient.TriggerPattern.apply(on(n)))
      }
    Seq(
      "cats.effect.unsafe.metrics:type=ComputePoolSampler-0",
      "cats.effect.unsafe.metrics:type=CpuStarvation-0",
      "com.acme:type=LiveFiberSnapshotTrigger-0"
    ).foreach(n => assertFalse(n, FiberSnapshotClient.isTrigger(on(n))))
  }

  // ------------------------------------------------------------------ JVM chooser ordering

  def testCandidatesPutSessionJvmsFirstAndToolingLast(): Unit = {
    import FiberSnapshotClient.Jvm
    val jvms = List(
      Jvm("1", "org.jetbrains.jps.cmdline.Launcher"),
      Jvm("2", "com.acme.OtherApp"),
      Jvm("3", "xsbt.boot.Boot"),
      Jvm("4", "com.acme.Main"),
      Jvm("5", "org.jetbrains.idea.maven.server.RemoteMavenServer")
    )
    val ordered = DumpCatsEffectFibersAction.candidates(jvms, Map("4" -> "Main"))
    assertEquals(List("4", "2"), ordered.take(2).map(_.jvm.pid))
    assertEquals(Some("Main"), ordered.head.session)
    assertEquals(Set("1", "3", "5"), ordered.drop(2).map(_.jvm.pid).toSet)
    assertEquals("Main: com.acme.Main  (pid 4)", ordered.head.label)
  }
}
