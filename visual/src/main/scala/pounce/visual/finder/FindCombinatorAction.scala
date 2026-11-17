package pounce.visual.finder

import com.intellij.openapi.actionSystem.{ActionUpdateThread, AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.project.DumbAware
import com.intellij.util.concurrency.AppExecutorUtil
import pounce.visual.toolwindow.{FinderPanel, PounceToolWindowFactory}

/**
 * D6: "Pounce: Find Combinator" (Ctrl+Alt+Shift+K). Opens the Combinator Finder with `have` = the type of the
 * selection / expression at the caret and `want` = the expected type there.
 */
final class FindCombinatorAction extends AnAction with DumbAware {

  override def getActionUpdateThread: ActionUpdateThread = ActionUpdateThread.BGT

  override def update(e: AnActionEvent): Unit = e.getPresentation.setEnabled(e.getProject != null)

  override def actionPerformed(e: AnActionEvent): Unit = {
    val project = e.getProject
    if (project == null) return
    val editor = Option(e.getData(CommonDataKeys.EDITOR))
    val file   = Option(e.getData(CommonDataKeys.PSI_FILE))
    ReadAction
      .nonBlocking[FinderPrefill.Prefill](() =>
        (for (ed <- editor; f <- file) yield FinderPrefill.at(f, ed)).getOrElse(FinderPrefill.Prefill("", "", ""))
      )
      .finishOnUiThread(
        ModalityState.defaultModalityState(),
        (p: FinderPrefill.Prefill) => PounceToolWindowFactory.showTab[FinderPanel](project, PounceToolWindowFactory.FinderTab)(_.prefill(p))
      )
      .submit(AppExecutorUtil.getAppExecutorService)
  }
}
