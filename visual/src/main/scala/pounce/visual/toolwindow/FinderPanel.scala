package pounce.visual.toolwindow

import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.project.Project
import com.intellij.ui.components.{JBLabel, JBList, JBScrollPane, JBTextArea, JBTextField}
import com.intellij.ui.{JBSplitter, SimpleListCellRenderer}
import com.intellij.util.concurrency.AppExecutorUtil
import com.intellij.util.ui.{FormBuilder, JBUI}
import pounce.core.InstanceOracle
import pounce.core.StaticInstances.Answer
import pounce.visual.finder.CombinatorFinder.{Have, Match}
import pounce.visual.finder.{CombinatorFinder, FinderPrefill, HoleFiller}
import pounce.visual.util.VisualPsi

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{DefaultListModel, JButton, JPanel, ListSelectionModel}

/**
 * D6: Combinator Finder tab. "I have `xs: List[A]`, I want `IO[List[B]]`" → `xs.traverse(???)`, ranked. Several
 * values can be given, separated by `;`. Double-click a result to insert it at the caret (replacing the selection
 * or the `???` under the caret).
 */
final class FinderPanel(project: Project) extends JPanel(new BorderLayout) with Disposable {

  private val haveField = new JBTextField()
  private val wantField = new JBTextField()
  private val status    = new JBLabel(" ")
  private val model     = new DefaultListModel[Match]()
  private val results   = new JBList[Match](model)
  private val details   = new JBTextArea()

  locally {
    haveField.getEmptyText.setText("xs: List[A]   (several: a: A; f: A => IO[B])")
    wantField.getEmptyText.setText("IO[List[B]]")
    val search = new JButton("Search")
    search.addActionListener(_ => runSearch())
    haveField.addActionListener(_ => runSearch())
    wantField.addActionListener(_ => runSearch())
    val form = FormBuilder
      .createFormBuilder()
      .addLabeledComponent("I have:", haveField)
      .addLabeledComponent("I want:", wantField)
      .addComponent(search)
      .addComponent(status)
      .getPanel
    form.setBorder(JBUI.Borders.empty(4))

    results.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    results.setCellRenderer(SimpleListCellRenderer.create[Match]((label, m, _) => {
      val unknown = if (m.answer == Answer.Unknown) "   (instances unverified)" else ""
      label.setText(s"${m.expression}      ${m.signature}$unknown")
    }))
    results.addListSelectionListener(_ => Option(results.getSelectedValue).foreach(showDetails))
    results.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        if (e.getClickCount == 2) Option(results.getSelectedValue).foreach(insert)
    })
    details.setEditable(false)
    details.setLineWrap(true)
    details.setWrapStyleWord(true)
    val split = new JBSplitter(true, 0.65f)
    split.setFirstComponent(new JBScrollPane(results))
    split.setSecondComponent(new JBScrollPane(details))
    add(form, BorderLayout.NORTH)
    add(split, BorderLayout.CENTER)
  }

  /** Fills the query (from the Find Combinator action) and searches. */
  def prefill(p: FinderPrefill.Prefill): Unit = {
    haveField.setText(if (p.have.isEmpty) "" else if (p.haveName.nonEmpty) s"${p.haveName}: ${p.have}" else p.have)
    wantField.setText(p.want)
    runSearch()
  }

  private def runSearch(): Unit = {
    val haveText = haveField.getText
    val wantText = wantField.getText
    FinderPanel.parse(haveText, wantText) match {
      case Left(err) => status.setText(err)
      case Right((have, want)) =>
        status.setText("Searching…")
        val editor = EditorInsert.currentEditor(project)
        ReadAction
          .nonBlocking[Seq[Match]](() => {
            val place = for {
              e <- editor
              f <- EditorInsert.fileOf(project, e)
              l <- Option(f.findElementAt(e.getCaretModel.getOffset))
            } yield l
            val has: CombinatorFinder.Has = place match {
              case Some(p) =>
                val oracle = InstanceOracle(p)
                (tc, t) => VisualPsi.safe(oracle.has(tc, t)).getOrElse(CombinatorFinder.staticHas(tc, t))
              case None => CombinatorFinder.staticHas
            }
            CombinatorFinder.search(have, want, has = has)
          })
          .expireWith(this)
          .finishOnUiThread(ModalityState.any(), (found: Seq[Match]) => {
            model.clear()
            found.foreach(model.addElement)
            status.setText(if (found.isEmpty) "No combinator found." else s"${found.size} result(s). Double-click to insert.")
          })
          .submit(AppExecutorUtil.getAppExecutorService)
    }
  }

  private def showDetails(m: Match): Unit = {
    val sb = new StringBuilder
    m.ops.foreach { op =>
      sb ++= s"${op.signature}\n"
      if (op.requires.nonEmpty) sb ++= s"requires: ${op.constraintsText}\n"
      if (op.doc.nonEmpty) sb ++= s"${op.doc}\n"
      if (op.example.nonEmpty) sb ++= s"Example: ${op.example}\n"
      sb ++= "\n"
    }
    if (m.holes.nonEmpty) sb ++= s"Still needed (???): ${m.holes.map(_.show).mkString(", ")}\n"
    details.setText(sb.toString)
    details.setCaretPosition(0)
  }

  private def insert(m: Match): Unit =
    EditorInsert.currentEditor(project).foreach { editor =>
      // replace a `???` under the caret
      EditorInsert.fileOf(project, editor).flatMap(f => Option(f.findElementAt(editor.getCaretModel.getOffset))).flatMap(HoleFiller.holeAt).foreach { hole =>
        if (!editor.getSelectionModel.hasSelection)
          editor.getSelectionModel.setSelection(hole.getTextRange.getStartOffset, hole.getTextRange.getEndOffset)
      }
      EditorInsert.insertAtCaret(project, editor, m.expression, m.ops)
    }

  override def dispose(): Unit = ()
}

object FinderPanel {

  /** `xs: List[A]; f: A => IO[B]` + `IO[List[B]]` → the finder query. */
  def parse(haveText: String, wantText: String): Either[String, (List[Have], pounce.core.types.Ty)] = {
    val names = Iterator("x", "y", "z", "w")
    val entries = haveText.split(';').map(_.trim).filter(_.nonEmpty).toList
    val have = entries.map { e =>
      val colon = topLevelColon(e)
      val (n, t) = if (colon > 0) (e.substring(0, colon).trim, e.substring(colon + 1).trim) else (names.next(), e)
      CombinatorFinder.parseQuery(t).map(Have(n, _)).left.map(err => s"have '$e': $err")
    }
    for {
      hs   <- have.foldRight[Either[String, List[Have]]](Right(Nil))((h, acc) => h.flatMap(x => acc.map(x :: _)))
      want <- if (wantText.trim.isEmpty) Left("Enter the type you want.") else CombinatorFinder.parseQuery(wantText).left.map(e => s"want: $e")
    } yield (hs, want)
  }

  /** `f: A => B` has its name colon before any bracket / arrow. */
  private def topLevelColon(s: String): Int = {
    val i = s.indexOf(':')
    if (i > 0 && s.substring(0, i).trim.matches("[a-zA-Z_][\\w]*")) i else -1
  }
}
