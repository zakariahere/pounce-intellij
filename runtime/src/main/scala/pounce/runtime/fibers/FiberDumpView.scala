package pounce.runtime.fibers

import com.intellij.icons.AllIcons
import com.intellij.notification.{NotificationGroupManager, NotificationType}
import com.intellij.openapi.project.Project
import com.intellij.threadDumpParser.ThreadState
import com.intellij.unscramble.{AnalyzeStacktraceUtil, ThreadDumpConsoleFactory}
import pounce.runtime.fibers.FiberSnapshot.{Fiber, Snapshot}

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters.*

/**
 * Shows a fiber snapshot in the Run tool window, like "Analyze Stack Trace" shows a thread dump: a list of fibers
 * (state icons, filter, sorting) and the selected fiber's trace with frames linked to sources by IntelliJ's JVM
 * exception filters.
 */
object FiberDumpView {

  /** One entry of the thread-dump viewer per fiber. */
  def threadStates(snapshot: Snapshot): List[ThreadState] =
    snapshot.byState.flatMap(_._2).map(threadState)

  def threadState(f: Fiber): ThreadState = {
    val ts = new ThreadState(f.title, f.state.label.toLowerCase)
    ts.setJavaThreadState(f.state.javaThreadState)
    ts.setStackTrace(FiberSnapshot.fiberStackTrace(f), f.frames.isEmpty)
    ts
  }

  def header(target: String, runtime: String): Seq[String] = Seq(
    s"Cats Effect fiber snapshot: $target at ${LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"))}",
    s"Runtime MBean: $runtime"
  )

  def show(project: Project, target: String, runtime: String, snapshot: Snapshot): Unit = {
    val text    = FiberSnapshot.render(snapshot, header(target, runtime))
    val states  = threadStates(snapshot)
    val factory = if (states.size > 1) new ThreadDumpConsoleFactory(project, states.asJava) else null
    AnalyzeStacktraceUtil.addConsole(project, factory, s"Fibers: $target", text, AllIcons.Actions.Dump)
  }

  def notify(project: Project, title: String, content: String, kind: NotificationType = NotificationType.WARNING): Unit =
    NotificationGroupManager
      .getInstance()
      .getNotificationGroup("Pounce")
      .createNotification(title, content, kind)
      .notify(project)
}
