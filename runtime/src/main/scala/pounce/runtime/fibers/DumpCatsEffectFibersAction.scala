package pounce.runtime.fibers

import com.intellij.execution.ExecutionManager
import com.intellij.execution.process.{BaseProcessHandler, ProcessHandler}
import com.intellij.execution.ui.RunContentManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.{ActionUpdateThread, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.progress.{ProgressIndicator, Task}
import com.intellij.openapi.project.{DumbAwareAction, Project}
import com.intellij.openapi.ui.popup.JBPopupFactory
import pounce.runtime.fibers.FiberSnapshotClient.{Jvm, Problem}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Tools → "Pounce: Dump Cats Effect Fibers". Lists local JVMs (those started from this project's Run/Debug
 * sessions first), attaches, starts the JDK's local JMX agent and calls cats-effect's
 * `LiveFiberSnapshotTrigger.liveFiberSnapshot` MBean: the SIGUSR1 fiber dump, but working on Windows.
 */
final class DumpCatsEffectFibersAction extends DumbAwareAction {

  override def getActionUpdateThread: ActionUpdateThread = ActionUpdateThread.BGT

  override def update(e: AnActionEvent): Unit = e.getPresentation.setEnabledAndVisible(e.getProject != null)

  override def actionPerformed(e: AnActionEvent): Unit = {
    val project = e.getProject
    if (project == null) return
    val sessions = DumpCatsEffectFibersAction.sessionProcesses(project)
    new Task.Backgroundable(project, "Listing local JVMs", true) {
      private var jvms: List[DumpCatsEffectFibersAction.Candidate] = Nil
      override def run(indicator: ProgressIndicator): Unit =
        jvms = DumpCatsEffectFibersAction.candidates(FiberSnapshotClient.localJvms(), sessions)
      override def onSuccess(): Unit = DumpCatsEffectFibersAction.choose(project, jvms)
    }.queue()
  }
}

object DumpCatsEffectFibersAction {

  /** A JVM offered in the chooser; `session` is the Run/Debug tab it belongs to. */
  final case class Candidate(jvm: Jvm, session: Option[String]) {
    def label: String = session.fold(s"${jvm.displayName}  (pid ${jvm.pid})")(s => s"$s: ${jvm.displayName}  (pid ${jvm.pid})")
  }

  /** pid → Run/Debug tab name, including child processes (e.g. `run` forked by sbt). */
  def sessionProcesses(project: Project): Map[String, String] =
    try {
      val descriptors = RunContentManager.getInstance(project).getAllDescriptors.asScala.toList
      def nameOf(h: ProcessHandler): String =
        descriptors.find(_.getProcessHandler == h).map(_.getDisplayName).getOrElse("Run session")
      ExecutionManager
        .getInstance(project)
        .getRunningProcesses
        .toList
        .filterNot(h => h.isProcessTerminated || h.isProcessTerminating)
        .flatMap {
          case h: BaseProcessHandler[?] =>
            try {
              val p    = h.getProcess
              val name = nameOf(h)
              val root = p.pid()
              (root :: p.descendants().iterator().asScala.map(_.pid()).toList).map(pid => pid.toString -> name)
            } catch { case NonFatal(_) => Nil }
          case _ => Nil
        }
        .toMap
    } catch { case NonFatal(_) => Map.empty }

  /** Session JVMs first, then the rest by display name; IntelliJ-family JVMs and build daemons last. */
  def candidates(jvms: List[Jvm], sessions: Map[String, String]): List[Candidate] = {
    val all = jvms.map(j => Candidate(j, sessions.get(j.pid)))
    def isTooling(c: Candidate) = {
      val n = c.jvm.displayName.toLowerCase
      n.contains("com.intellij") || n.contains("jetbrains") || n.contains("org.jetbrains.jps") ||
      n.contains("xsbt.boot") || n.contains("sbt-launch") || n.contains("org.gradle") || n.contains("bloop") ||
      n.contains("kotlin.daemon") || n.contains("nailgun")
    }
    val (inSession, rest) = all.partition(_.session.isDefined)
    val (tooling, apps)   = rest.partition(isTooling)
    inSession.sortBy(_.label) ++ apps.sortBy(_.label) ++ tooling.sortBy(_.label)
  }

  def choose(project: Project, candidates: List[Candidate]): Unit =
    candidates match {
      case Nil =>
        FiberDumpView.notify(project, "No JVMs found", "The Attach API lists no other local JVM running as this user.")
      // exactly one JVM belongs to a Run/Debug session of this project: that is the one the user means
      case _ if candidates.count(_.session.isDefined) == 1 => dump(project, candidates.find(_.session.isDefined).get)
      case _ =>
        JBPopupFactory
          .getInstance()
          .createPopupChooserBuilder(candidates.asJava)
          .setTitle("Dump Cats Effect Fibers Of")
          .setRenderer(new com.intellij.ui.SimpleListCellRenderer[Candidate] {
            override def customize(list: javax.swing.JList[? <: Candidate], value: Candidate, index: Int, selected: Boolean, hasFocus: Boolean): Unit =
              if (value != null) setText(value.label)
          })
          .setItemChosenCallback(c => dump(project, c))
          .createPopup()
          .showCenteredInCurrentWindow(project)
    }

  def dump(project: Project, candidate: Candidate): Unit = {
    val target = s"pid ${candidate.jvm.pid} (${candidate.session.getOrElse(candidate.jvm.displayName)})"
    new Task.Backgroundable(project, s"Fetching Cats Effect fibers from $target", true) {
      private var result: Either[Problem, List[FiberSnapshotClient.RuntimeSnapshot]] = Right(Nil)
      override def run(indicator: ProgressIndicator): Unit = result = FiberSnapshotClient.fetch(candidate.jvm.pid)
      override def onSuccess(): Unit = report(project, target, result)
    }.queue()
  }

  def report(project: Project, target: String, result: Either[Problem, List[FiberSnapshotClient.RuntimeSnapshot]]): Unit =
    result match {
      case Left(p: Problem.NoMBean) =>
        FiberDumpView.notify(project, "Cats Effect fiber snapshot unavailable", s"${p.message}. ${FiberSnapshotClient.EnableAdvice}")
      case Left(p) =>
        FiberDumpView.notify(
          project,
          "Cats Effect fiber snapshot failed",
          s"${p.message}. The target must be a local JVM (9+) of the same user, without -XX:+DisableAttachMechanism.",
          NotificationType.ERROR
        )
      case Right(snapshots) =>
        snapshots.foreach { rs =>
          val snapshot = FiberSnapshot.parse(rs.chunks)
          if (snapshot.isEmpty)
            FiberDumpView.notify(
              project,
              "Empty fiber snapshot",
              s"${rs.objectName} returned no fibers. ${FiberSnapshotClient.EnableAdvice}",
              NotificationType.INFORMATION
            )
          else ApplicationManager.getApplication.invokeLater(() => FiberDumpView.show(project, target, rs.objectName, snapshot))
        }
    }
}
