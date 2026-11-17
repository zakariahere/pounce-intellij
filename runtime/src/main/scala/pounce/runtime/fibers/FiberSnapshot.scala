package pounce.runtime.fibers

import scala.collection.mutable

/**
 * A parsed cats-effect live fiber snapshot (the text `LiveFiberSnapshotTrigger.liveFiberSnapshot()` returns, the
 * same report CE prints on SIGUSR1/SIGINFO). Format, verified against cats-effect 3.6.3
 * (`FiberMonitor.liveFiberSnapshot`, `FiberMonitorShared.fiberString`, `Tracing.prettyPrint`):
 *
 * {{{
 * cats.effect.IOFiber@56824a14 WAITING
 *  ├ flatMap @ com.acme.Main$.$anonfun$run$1(Main.scala:12)
 *  ╰ delay @ com.acme.Main$.run(Main.scala:10)
 *
 * Thread[#31,io-compute-3,5,main] (#3): 0 enqueued
 * Global: enqueued 0, foreign 0, waiting 1
 * }}}
 */
object FiberSnapshot {

  /** Fiber states CE reports (`ACTIVE` = foreign fibers when the runtime has no work-stealing pool). */
  enum State(val label: String, val javaThreadState: String, val order: Int) {
    case Running  extends State("RUNNING", "RUNNABLE", 0)
    case Blocked  extends State("BLOCKED", "BLOCKED", 1)
    case Active   extends State("ACTIVE", "RUNNABLE", 2)
    case Yielding extends State("YIELDING", "RUNNABLE", 3)
    case Waiting  extends State("WAITING", "WAITING", 4)
  }

  object State {
    def parse(s: String): Option[State] = State.values.find(_.label == s.trim.toUpperCase)
  }

  /** One trace frame: `flatMap @ com.acme.Main$.$anonfun$run$1(Main.scala:12)`. */
  final case class Frame(op: Option[String], className: String, method: String, file: Option[String], line: Option[Int]) {

    /** JVM stack-trace syntax, so IntelliJ's exception filters turn it into a source link. */
    def jvmLine: String = {
      val location = (file, line) match {
        case (Some(f), Some(l)) => s"$f:$l"
        case (Some(f), None)    => f
        case _                  => "Unknown Source"
      }
      val suffix = op.fold("")(o => s" [$o]")
      s"\tat $className.$method($location)$suffix"
    }

    def userCodeHint: String = (file, line) match {
      case (Some(f), Some(l)) => op.fold(s"$f:$l")(o => s"$o @ $f:$l")
      case _                  => op.getOrElse(method)
    }
  }

  final case class Fiber(id: String, state: State, frames: List[Frame], rawTrace: List[String]) {
    def title: String = frames.headOption.fold(s"IOFiber@$id")(f => s"IOFiber@$id  (${f.userCodeHint})")
  }

  final case class Worker(thread: String, index: Int, enqueued: Int)

  final case class Global(enqueued: Int, foreign: Int, waiting: Int)

  final case class Snapshot(fibers: List[Fiber], workers: List[Worker], global: Option[Global]) {
    def isEmpty: Boolean = fibers.isEmpty && workers.isEmpty && global.isEmpty

    def byState: List[(State, List[Fiber])] =
      State.values.toList.sortBy(_.order).map(s => s -> fibers.filter(_.state == s)).filter(_._2.nonEmpty)

    def counts: String = byState.map((s, fs) => s"${s.label} ${fs.size}").mkString(", ")
  }

  private val FiberHeader = """^cats\.effect\.IOFiber@([0-9a-fA-F]+)\s+([A-Z]+)\s*$""".r
  private val TraceLine   = """^\s*[├╰|`+\\-]\s*(.*)$""".r
  private val WorkerLine  = """^(.*)\s+\(#(\d+)\):\s+(\d+)\s+enqueued\s*$""".r
  private val GlobalLine  = """^Global:\s+enqueued\s+(\d+),\s+foreign\s+(\d+),\s+waiting\s+(\d+)\s*$""".r
  // `StackTraceElement.toString` may prefix `loader/module@ver/` (JDK 9+): skipped by the optional `[^\s(]*/`
  private val FrameText   = """^(?:(.+?)\s+@\s+)?(?:[^\s(]*/)?([\w$.]+)\.([\w$<>]+)\((.*)\)\s*$""".r

  def parseFrame(text: String): Option[Frame] = text.trim match {
    case FrameText(op, cls, method, loc) =>
      val (file, line) = loc.lastIndexOf(':') match {
        case -1 => (Option(loc).filter(l => l.nonEmpty && l != "Unknown Source"), None)
        case i  => (Some(loc.substring(0, i)), loc.substring(i + 1).toIntOption)
      }
      Some(Frame(Option(op).map(_.trim).filter(_.nonEmpty), cls, method, file, line))
    case _ => None
  }

  /** Parses the MBean result: the chunks are concatenated exactly as CE prints them. */
  def parse(chunks: Seq[String]): Snapshot = parseText(chunks.mkString)

  def parseText(text: String): Snapshot = {
    val fibers  = mutable.ListBuffer.empty[Fiber]
    val workers = mutable.ListBuffer.empty[Worker]
    var global  = Option.empty[Global]

    var current: Option[(String, State)] = None
    val frames                           = mutable.ListBuffer.empty[Frame]
    val raw                              = mutable.ListBuffer.empty[String]

    def flush(): Unit = {
      current.foreach((id, state) => fibers += Fiber(id, state, frames.toList, raw.toList))
      current = None
      frames.clear()
      raw.clear()
    }

    text.linesIterator.foreach { line =>
      line match {
        case FiberHeader(id, st) =>
          flush()
          current = State.parse(st).map(id.toLowerCase -> _)
        case GlobalLine(e, f, w) =>
          flush()
          global = Some(Global(e.toInt, f.toInt, w.toInt))
        case WorkerLine(thread, idx, n) if !line.trim.startsWith("├") && !line.trim.startsWith("╰") =>
          flush()
          workers += Worker(thread.trim, idx.toInt, n.toInt)
        case TraceLine(frame) if current.isDefined =>
          raw += frame
          parseFrame(frame).foreach(frames += _)
        case blank if blank.trim.isEmpty => flush()
        case _                           => ()
      }
    }
    flush()
    Snapshot(fibers.toList, workers.toList, global)
  }

  /** Thread name out of `Thread[#31,io-compute-3,5,main]` (JDK 19+) or `Thread[io-compute-3,5,main]`. */
  def threadName(workerThread: String): String = {
    val inner = workerThread.stripPrefix("Thread[").stripSuffix("]")
    inner.split(',').toList match {
      case id :: name :: _ if id.startsWith("#") => name
      case name :: _                             => name
      case Nil                                   => workerThread
    }
  }

  /**
   * Plain-text report grouped by state, with frames in JVM stack-trace syntax (clickable in IntelliJ consoles).
   *
   * @param header lines describing the source (process, runtime MBean, time)
   */
  def render(snapshot: Snapshot, header: Seq[String]): String = {
    val sb = new StringBuilder
    header.foreach(h => sb.append(h).append('\n'))
    sb.append(s"Fibers: ${snapshot.fibers.size}")
    if (snapshot.fibers.nonEmpty) sb.append(s" (${snapshot.counts})")
    sb.append('\n')
    snapshot.global.foreach(g => sb.append(s"Global: enqueued ${g.enqueued}, foreign ${g.foreign}, waiting ${g.waiting}\n"))
    if (snapshot.workers.nonEmpty) {
      sb.append("Workers:\n")
      snapshot.workers.sortBy(_.index).foreach(w => sb.append(s"  ${threadName(w.thread)} (#${w.index}): ${w.enqueued} enqueued\n"))
    }
    snapshot.byState.foreach { (state, fibers) =>
      sb.append(s"\n=== ${state.label} (${fibers.size}) ===\n")
      fibers.foreach { f =>
        sb.append(s"\n\"IOFiber@${f.id}\" ${state.label}\n")
        f.frames.foreach(fr => sb.append(fr.jvmLine).append('\n'))
        if (f.frames.isEmpty) sb.append("\t(no trace: no traced operation recorded, or cats.effect.tracing.mode=none)\n")
      }
    }
    sb.toString
  }

  /** The stack trace of one fiber, for the thread-dump style viewer. */
  def fiberStackTrace(fiber: Fiber): String =
    (s"\"IOFiber@${fiber.id}\" ${fiber.state.label}" :: fiber.frames.map(_.jvmLine)).mkString("\n")
}
