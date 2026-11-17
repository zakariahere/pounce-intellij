package pounce.play.config

import com.intellij.openapi.util.TextRange
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.*
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.play.util.{PlayPsi, TextTarget}

/** Ctrl+click on a config path literal → its definition in `application.conf` / a library `reference.conf`. */
final class ConfKeyReferenceContributor extends PsiReferenceContributor {
  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit =
    registrar.registerReferenceProvider(PlatformPatterns.psiElement(classOf[ScStringLiteral]), new ConfKeyReferenceProvider)
}

final class ConfKeyReferenceProvider extends PsiReferenceProvider {
  override def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] =
    PlayPsi.plainLiteral(element) match {
      case Some(lit) if lit.getValue.nonEmpty && ConfCalls.enabled(lit) =>
        ConfCalls.use(lit) match {
          case Some(use) => Array(new ConfKeyReference(lit, use.prefix))
          case None      => PsiReference.EMPTY_ARRAY
        }
      case _ => PsiReference.EMPTY_ARRAY
    }
}

/** Soft: unknown keys are reported by [[UnknownConfigKeyInspection]] with its own rules, not as red references. */
final class ConfKeyReference(lit: ScStringLiteral, val prefix: ConfPrefix)
    extends PsiPolyVariantReferenceBase[ScStringLiteral](lit, PlayPsi.contentRange(lit), true) {

  def key: String = lit.getValue

  /** With an unknown prefix we still try the key as an absolute path. */
  def candidatePaths: Seq[String] = prefix match {
    case ConfPrefix.Unknown => Seq(key)
    case p                  => p.resolve(key).toSeq
  }

  def targets: Seq[TextTarget] = {
    val snapshot = ConfIndex.snapshot(lit.getProject)
    for {
      path  <- candidatePaths
      entry <- snapshot.definitions(path)
    } yield ConfKeyReference.target(lit.getProject, entry)
  }

  override def multiResolve(incompleteCode: Boolean): Array[ResolveResult] =
    targets.map(t => new PsiElementResolveResult(t): ResolveResult).toArray

  /** The definition that wins at runtime: the project's own (application.conf) before library defaults. */
  override def resolve(): PsiElement = targets.headOption.orNull

  override def isReferenceTo(element: PsiElement): Boolean = element match {
    case t: TextTarget => t.kind == ConfKeyReference.Kind && candidatePaths.contains(t.name)
    case _             => false
  }

  override def handleElementRename(newElementName: String): PsiElement = lit

  override def getVariants: Array[AnyRef] = Array.empty
}

object ConfKeyReference {
  val Kind = "config key"

  def target(project: com.intellij.openapi.project.Project, entry: ConfEntry): TextTarget =
    new TextTarget(project, entry.file, entry.line, entry.path, entry.lastSegment, Kind, entry.value)
}
