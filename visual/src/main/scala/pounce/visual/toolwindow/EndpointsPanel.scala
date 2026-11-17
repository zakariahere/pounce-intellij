package pounce.visual.toolwindow

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.pom.Navigatable
import com.intellij.ui.components.{JBLabel, JBScrollPane}
import com.intellij.ui.table.JBTable
import com.intellij.util.concurrency.AppExecutorUtil
import com.intellij.util.ui.JBUI
import pounce.visual.endpoints.{Endpoint, Endpoints}

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.table.AbstractTableModel
import javax.swing.{JButton, JPanel, ListSelectionModel}

/** D7: HTTP endpoints (http4s, Play routes, tapir). Refresh rescans; double-click navigates. */
final class EndpointsPanel(project: Project) extends JPanel(new BorderLayout) with Disposable {

  private var rows: Seq[Endpoint] = Nil
  private val status              = new JBLabel(" ")

  private val model = new AbstractTableModel {
    private val columns                                     = Array("Method", "Path", "Framework", "Handler", "Location")
    override def getRowCount: Int                           = rows.size
    override def getColumnCount: Int                        = columns.length
    override def getColumnName(column: Int): String         = columns(column)
    override def getValueAt(row: Int, column: Int): AnyRef = {
      val e = rows(row)
      column match {
        case 0 => e.method
        case 1 => e.path
        case 2 => e.framework
        case 3 => e.handler
        case _ => e.location
      }
    }
  }

  private val table = new JBTable(model)

  locally {
    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    table.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        if (e.getClickCount == 2 && table.getSelectedRow >= 0) navigate(rows(table.convertRowIndexToModel(table.getSelectedRow)))
    })
    val refresh = new JButton("Refresh")
    refresh.addActionListener(_ => reload())
    val top = new JPanel(new BorderLayout)
    top.add(refresh, BorderLayout.WEST)
    top.add(status, BorderLayout.CENTER)
    top.setBorder(JBUI.Borders.empty(4))
    add(top, BorderLayout.NORTH)
    add(new JBScrollPane(table), BorderLayout.CENTER)
    reload()
  }

  def reload(): Unit = {
    status.setText("Scanning…")
    ReadAction
      .nonBlocking[Seq[Endpoint]](() => Endpoints.scan(project))
      .inSmartMode(project)
      .expireWith(this)
      .finishOnUiThread(ModalityState.any(), (found: Seq[Endpoint]) => {
        rows = found
        model.fireTableDataChanged()
        status.setText(s"  ${found.size} endpoint(s)")
      })
      .submit(AppExecutorUtil.getAppExecutorService)
  }

  private def navigate(e: Endpoint): Unit =
    ReadAction
      .nonBlocking[Option[Navigatable]](() => e.target(project).collect { case n: Navigatable => n })
      .expireWith(this)
      .finishOnUiThread(
        ModalityState.any(),
        (target: Option[Navigatable]) => target.getOrElse(new OpenFileDescriptor(project, e.file, e.offset)).navigate(true)
      )
      .submit(AppExecutorUtil.getAppExecutorService)

  override def dispose(): Unit = ()
}
