package pounce.visual.toolwindow

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.event.{CaretEvent, CaretListener}
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow
import com.intellij.psi.{PsiElement, SmartPointerManager, SmartPsiElementPointer}
import com.intellij.ui.components.{JBLabel, JBList, JBScrollPane, JBTextArea, JBTextField}
import com.intellij.ui.treeStructure.Tree
import com.intellij.ui.{JBSplitter, SimpleListCellRenderer}
import com.intellij.util.Alarm
import com.intellij.util.concurrency.AppExecutorUtil
import com.intellij.util.ui.JBUI
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import pounce.core.StaticInstances.Answer
import pounce.visual.explorer.TypeclassExplorer
import pounce.visual.explorer.TypeclassExplorer.{Node, OpRow, Report}

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel}
import javax.swing.{DefaultListModel, JButton, JPanel, ListSelectionModel}

/**
 * D5: the Typeclass Explorer. Follows the caret (debounced, computed in a background read action while the tool
 * window is visible) or explores a type typed into the field. Double-click an operation to append it to the
 * explored expression (`ids` → `ids.traverse(|)`), adding the cats syntax import if needed.
 */
final class ExplorerPanel(project: Project, toolWindow: ToolWindow) extends JPanel(new BorderLayout) with Disposable {

  private val alarm      = new Alarm(Alarm.ThreadToUse.SWING_THREAD, this)
  private val typeField  = new JBTextField()
  private val subject    = new JBLabel("Move the caret onto an expression, or type a type and press Enter.")
  private val treeModel  = new DefaultTreeModel(new DefaultMutableTreeNode("Typeclasses"))
  private val tree       = new Tree(treeModel)
  private val opsModel   = new DefaultListModel[OpRow]()
  private val opsList    = new JBList[OpRow](opsModel)
  private val details    = new JBTextArea()
  @volatile private var target: Option[SmartPsiElementPointer[ScExpression]] = None

  locally {
    typeField.getEmptyText.setText("Type to explore, e.g. Either[String, Int]")
    typeField.addActionListener(_ => exploreTyped())
    val explore = new JButton("Explore")
    explore.addActionListener(_ => exploreTyped())
    val top = new JPanel(new BorderLayout(JBUI.scale(4), 0))
    top.add(typeField, BorderLayout.CENTER)
    top.add(explore, BorderLayout.EAST)
    top.add(subject, BorderLayout.SOUTH)
    top.setBorder(JBUI.Borders.empty(4))

    opsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    opsList.setCellRenderer(SimpleListCellRenderer.create[OpRow]((label, row, _) => {
      val mark = if (row.answer == Answer.Yes) "" else "  (instance unknown)"
      label.setText(s"${row.label}   ${row.op.signature}$mark")
    }))
    opsList.addListSelectionListener(_ => Option(opsList.getSelectedValue).foreach(showDetails))
    opsList.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        if (e.getClickCount == 2) Option(opsList.getSelectedValue).foreach(insert)
    })
    details.setEditable(false)
    details.setLineWrap(true)
    details.setWrapStyleWord(true)

    val right = new JBSplitter(true, 0.65f)
    right.setFirstComponent(new JBScrollPane(opsList))
    right.setSecondComponent(new JBScrollPane(details))
    val split = new JBSplitter(false, 0.4f)
    split.setFirstComponent(new JBScrollPane(tree))
    split.setSecondComponent(right)

    add(top, BorderLayout.NORTH)
    add(split, BorderLayout.CENTER)

    EditorFactory.getInstance().getEventMulticaster.addCaretListener(
      new CaretListener {
        override def caretPositionChanged(event: CaretEvent): Unit =
          if (event.getEditor.getProject == project && toolWindow.isVisible) schedule()
      },
      this
    )
  }

  private def schedule(): Unit = {
    alarm.cancelAllRequests()
    alarm.addRequest(() => exploreAtCaret(), 350)
  }

  private def exploreAtCaret(): Unit =
    EditorInsert.currentEditor(project).foreach { editor =>
      val offset = editor.getCaretModel.getOffset
      ReadAction
        .nonBlocking[Option[(Report, Option[SmartPsiElementPointer[ScExpression]])]](() =>
          EditorInsert.fileOf(project, editor).collect { case f: ScalaFile => f }.flatMap(f => TypeclassExplorer.subjectAt(f, offset)).map { s =>
            val pointer = s.expression.map(e => SmartPointerManager.getInstance(project).createSmartPsiElementPointer(e))
            (TypeclassExplorer.explore(s.scType, s.place), pointer)
          }
        )
        .coalesceBy(this)
        .expireWith(this)
        .finishOnUiThread(ModalityState.any(), result => result.foreach { case (r, p) => target = p; show(r) })
        .submit(AppExecutorUtil.getAppExecutorService)
    }

  private def exploreTyped(): Unit = {
    val text = typeField.getText.trim
    if (text.nonEmpty)
      EditorInsert.currentEditor(project).foreach { editor =>
        val offset = editor.getCaretModel.getOffset
        ReadAction
          .nonBlocking[Option[Report]](() =>
            EditorInsert.fileOf(project, editor).flatMap { f =>
              val context: PsiElement = Option(f.findElementAt(offset)).getOrElse(f)
              TypeclassExplorer.parseType(text, context).map(TypeclassExplorer.explore(_, context))
            }
          )
          .expireWith(this)
          .finishOnUiThread(ModalityState.any(), r => r.fold(subject.setText(s"Cannot resolve type: $text")) { rep => target = None; show(rep) })
          .submit(AppExecutorUtil.getAppExecutorService)
      }
  }

  /** Visible for tests: renders a report into the tree / list models. */
  def show(r: Report): Unit = {
    subject.setText(s"${r.typeText}    shape: ${r.shape}")
    val root = new DefaultMutableTreeNode(s"Instances of ${r.typeText}")
    val hierarchy = new DefaultMutableTreeNode("Hierarchy (strongest first)")
    r.tree.foreach(n => hierarchy.add(toTreeNode(n)))
    val all = new DefaultMutableTreeNode("All typeclasses")
    r.rows.foreach { row =>
      val mark = row.answer match {
        case Answer.Yes     => "✔"
        case Answer.No      => "✘"
        case Answer.Unknown => "?"
      }
      all.add(new DefaultMutableTreeNode(s"$mark ${row.label} — ${row.tc.summary}"))
    }
    root.add(hierarchy)
    root.add(all)
    treeModel.setRoot(root)
    tree.expandRow(0)
    tree.expandRow(1)
    opsModel.clear()
    r.ops.foreach(opsModel.addElement)
    details.setText("")
  }

  private def toTreeNode(n: Node): DefaultMutableTreeNode = {
    val node = new DefaultMutableTreeNode(n.name)
    n.children.foreach(c => node.add(toTreeNode(c)))
    node
  }

  private def showDetails(row: OpRow): Unit = {
    val op = row.op
    val sb = new StringBuilder
    sb ++= s"${op.signature}\n"
    if (op.requires.nonEmpty) sb ++= s"requires: ${op.constraintsText}${if (row.bindings.nonEmpty) s"   (here: ${row.bindings})" else ""}\n"
    op.syntax.foreach(s => sb ++= s"import: ${pounce.core.Imports.rootFor(s, pounce.core.settings.PounceSettings().style)}._\n")
    if (op.doc.nonEmpty) sb ++= s"\n${op.doc}\n"
    if (op.example.nonEmpty) sb ++= s"\nExample:\n${op.example}\n"
    if (op.seeAlso.nonEmpty) sb ++= s"\nSee also: ${op.seeAlso.mkString(", ")}\n"
    sb ++= "\nDouble-click to insert."
    details.setText(sb.toString)
    details.setCaretPosition(0)
  }

  private def insert(row: OpRow): Unit =
    for {
      editor  <- EditorInsert.currentEditor(project)
      pointer <- target
    } EditorInsert.appendOp(project, editor, pointer, row.op)

  override def dispose(): Unit = ()
}
