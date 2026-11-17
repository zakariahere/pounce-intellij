package pounce.core

import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootModificationTracker
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.util.{CachedValueProvider, CachedValuesManager}
import com.intellij.psi.{JavaPsiFacade, PsiElement}

/**
 * Per-module library detection. Every Pounce feature checks this first, so modules without Cats / Play get zero
 * noise and zero overhead.
 */
object Libraries {

  def of(module: Module): Set[Lib] =
    if (module == null || module.isDisposed) Set.empty
    else
      CachedValuesManager.getManager(module.getProject).getCachedValue(
        module,
        () => {
          val scope  = GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module, /* includeTests = */ true)
          val facade = JavaPsiFacade.getInstance(module.getProject)
          val found = Lib.values.filter(lib => lib.markerClasses.exists(fqn => facade.findClass(fqn, scope) != null)).toSet
          CachedValueProvider.Result.create(found, ProjectRootModificationTracker.getInstance(module.getProject))
        }
      )

  def of(element: PsiElement): Set[Lib] =
    if (element == null || !element.isValid) Set.empty
    else Option(ModuleUtilCore.findModuleForPsiElement(element)).map(of).getOrElse(projectWide(element.getProject))

  /** For files outside modules (scratch files, library sources) fall back to "any module has it". */
  def projectWide(project: Project): Set[Lib] =
    CachedValuesManager.getManager(project).getCachedValue(
      project,
      () => {
        val modules = com.intellij.openapi.module.ModuleManager.getInstance(project).getModules.toSeq
        val found   = modules.flatMap(of).toSet
        CachedValueProvider.Result.create(found, ProjectRootModificationTracker.getInstance(project))
      }
    )

  def has(element: PsiElement, lib: Lib): Boolean = of(element).contains(lib)

  def hasCats(element: PsiElement): Boolean = has(element, Lib.CatsCore)

  def hasCatsEffect(element: PsiElement): Boolean = has(element, Lib.CatsEffect)

  def hasPlay(element: PsiElement): Boolean = has(element, Lib.Play)

  /** Play 3 = Play on Pekko. */
  def isPlay3(element: PsiElement): Boolean = {
    val libs = of(element)
    libs.contains(Lib.Play) && libs.contains(Lib.Pekko)
  }
}
