package pounce.play

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import pounce.core.settings.PounceSettings
import pounce.play.config.{ConfIndex, ConfKeyReference}
import pounce.play.i18n.{MessageKeyReference, MessagesIndex}
import pounce.play.util.{TextFiles, TextTarget, TextTargetUsages}

/**
 * The reverse direction: Ctrl+B / Ctrl+click on a key in `application.conf` or `conf/messages*` lists the Scala
 * code that reads it (`config.get[String]("app.name")`, `messages("user.greeting")`).
 */
final class KeyUsagesGotoHandler extends GotoDeclarationHandler {

  override def getGotoDeclarationTargets(source: PsiElement, offset: Int, editor: Editor): Array[PsiElement] =
    if (source == null) null
    else {
      val file = source.getContainingFile
      val vf   = Option(file).flatMap(f => Option(f.getOriginalFile.getVirtualFile)).orNull
      if (vf == null) null
      else {
        val project = source.getProject
        val target  = KeyUsagesGotoHandler.targetAt(project, vf, offset)
        target match {
          case Some(t) =>
            val usages = TextTargetUsages.literals(t, GlobalSearchScope.projectScope(project))
            if (usages.isEmpty) null else usages.toArray
          case None => null
        }
      }
    }
}

object KeyUsagesGotoHandler {

  /** The config / message key defined at `offset` of a project conf or messages file. */
  def targetAt(project: com.intellij.openapi.project.Project, vf: VirtualFile, offset: Int): Option[TextTarget] = {
    val settings = PounceSettings()
    lazy val text = TextFiles.text(vf).getOrElse("")
    lazy val line = TextFiles.lineOf(text, offset)
    val conf =
      if (settings.playConfigKeys && ConfIndex.isProjectConfFile(project, vf)) {
        val entries = ConfIndex.snapshot(project).entriesAt(vf, line)
        val underCaret = entries.filter { e =>
          val start = TextFiles.wordOffsetOnLine(text, line, e.lastSegment)
          offset >= start && offset <= start + e.lastSegment.length
        }
        (if (underCaret.nonEmpty) underCaret else entries).maxByOption(_.path.length).map(ConfKeyReference.target(project, _))
      } else None
    conf.orElse {
      if (settings.playMessages) {
        val snapshot = MessagesIndex.snapshot(project)
        snapshot.project.find(_.file == vf).flatMap { mf =>
          mf.entries.find(_.line == line).map(e => MessageKeyReference.target(project, mf, e))
        }
      } else None
    }
  }
}
