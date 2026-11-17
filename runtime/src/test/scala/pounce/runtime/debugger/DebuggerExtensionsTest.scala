package pounce.runtime.debugger

import com.intellij.debugger.settings.NodeRendererSettings
import com.intellij.debugger.ui.tree.render.{CompoundReferenceRenderer, CompoundRendererProvider}
import com.intellij.openapi.roots.OrderEnumerator
import com.intellij.ui.classFilter.DebuggerClassFilterProvider
import org.jetbrains.org.objectweb.asm.{ClassReader, ClassVisitor, FieldVisitor, Opcodes}
import org.jetbrains.plugins.scala.DependencyManagerBase.*
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert.*
import pounce.runtime.debugger.renderers.CatsLayouts.Fqn
import pounce.runtime.debugger.renderers.{CatsRendererProvider, CatsRendererProviders}
import pounce.runtime.settings.PounceRuntimeSettings
import pounce.testing.PounceFixtureTestBase

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * F1: the stepping filter and renderer providers are registered on the Java debugger's extension points (which also
 * proves the plugin class loader reaches the debugger content module), follow the Pounce settings, and the field
 * layouts the renderers read exist in the real cats / cats-effect / Scala library jars.
 */
class DebuggerExtensionsTest extends PounceFixtureTestBase {

  override protected def extraLibraries: Seq[LibraryLoader] = {
    given ScalaVersion = version
    Seq(
      IvyManagedLoader(
        "org.typelevel" %% "cats-kernel"        % catsVersion,
        "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion,
        "org.typelevel" %% "cats-effect-std"    % catsEffectVersion
      )
    )
  }

  private def settings = PounceRuntimeSettings()

  // ------------------------------------------------------------------ stepping filters

  private def ourFilterProvider: CatsSteppingFilterProvider =
    DebuggerClassFilterProvider.EP_NAME.getExtensionList.asScala.collectFirst { case p: CatsSteppingFilterProvider => p }
      .getOrElse(fail("CatsSteppingFilterProvider is not registered").asInstanceOf[Nothing])

  def testSteppingFilterIsRegisteredAndOnByDefault(): Unit = {
    assertTrue(settings.steppingFilters)
    val patterns = ourFilterProvider.getFilters.asScala.map(_.getPattern).toSet
    assertEquals(Set("cats.*", "fs2.*", "scala.runtime.*", "scala.Function*", "scala.PartialFunction*"), patterns)
    assertTrue(ourFilterProvider.getFilters.asScala.forall(_.isEnabled))
  }

  def testSteppingFilterSkipsLibraryInternals(): Unit =
    Seq(
      "cats.effect.IOFiber",
      "cats.effect.IO$",
      "cats.effect.kernel.Resource$Bind",
      "cats.effect.unsafe.WorkerThread",
      "cats.instances.ListInstances$$anon$1",
      "cats.data.Kleisli",
      "fs2.internal.Scope",
      "fs2.Stream$",
      "scala.runtime.AbstractFunction1",
      "scala.runtime.java8.JFunction1$mcII$sp",
      "scala.Function1",
      "scala.Function2$$anon$1",
      "scala.PartialFunction$Lifted"
    ).foreach(c => assertTrue(c, CatsSteppingFilterProvider.isFiltered(c)))

  def testSteppingFilterKeepsUserAndOtherLibraryCode(): Unit =
    Seq(
      "com.acme.Main$",
      "com.acme.cats.Service",
      "catsx.Tool",
      "fs2x.Pipe",
      "play.api.mvc.Action",
      "scala.collection.immutable.List",
      "scala.Option",
      "org.http4s.server.Router$"
    ).foreach(c => assertFalse(c, CatsSteppingFilterProvider.isFiltered(c)))

  // (no by-name helper here: JUnit 3 would report the synthetic `test...$$anonfun` thunk as a non-public test)
  def testSteppingFilterFollowsTheSetting(): Unit = {
    settings.steppingFilters = false
    try assertTrue(ourFilterProvider.getFilters.isEmpty)
    finally settings.steppingFilters = true
  }

  // ------------------------------------------------------------------ renderers

  private def ourProviders: List[CatsRendererProvider] =
    CompoundRendererProvider.EP_NAME.getExtensionList.asScala.toList.collect { case p: CatsRendererProvider => p }

  def testEveryRendererProviderIsRegistered(): Unit = {
    assertEquals(CatsRendererProviders.all.toSet, ourProviders.map(_.getClass).toSet)
    assertEquals(CatsRendererProviders.all.size, ourProviders.size)
    val targets = ourProviders.map(_.targetClassName).toSet
    assertEquals(
      Set(Fqn.NonEmptyList, Fqn.NonEmptyVector, Fqn.Chain, Fqn.Validated, Fqn.Ior, Fqn.EitherT, Fqn.OptionT, Fqn.SyncRef),
      targets
    )
    assertTrue(ourProviders.forall(_.displayName.startsWith("Pounce: ")))
  }

  private def ourRenderers: List[CompoundReferenceRenderer] =
    NodeRendererSettings.getInstance().getAllRenderers(getProject).asScala.toList.collect {
      case r: CompoundReferenceRenderer if r.getName.startsWith("Pounce: ") => r
    }

  def testRenderersReachTheDebuggerOnByDefault(): Unit = {
    val renderers = ourRenderers
    assertEquals(renderers.map(_.getName).toString, CatsRendererProviders.all.size, renderers.size)
    assertTrue(renderers.forall(_.isEnabled))
    val nel = renderers.find(_.getName == "Pounce: cats.data.NonEmptyList").getOrElse(fail().asInstanceOf[Nothing])
    assertEquals(Fqn.NonEmptyList, nel.getClassName)
    assertTrue(nel.getLabelRenderer.isInstanceOf[pounce.runtime.debugger.renderers.NonEmptyListRenderer])
    assertSame(nel.getLabelRenderer, nel.getChildrenRenderer)
  }

  def testRenderersFollowTheSetting(): Unit = {
    settings.catsRenderers = false
    try {
      val renderers = ourRenderers
      assertFalse(renderers.isEmpty)
      assertTrue(renderers.map(r => r.getName -> r.isEnabled).toString, renderers.forall(!_.isEnabled))
    } finally settings.catsRenderers = true
  }

  def testRendererClonesKeepTheirKind(): Unit =
    ourRenderers.foreach { r =>
      val label = r.getLabelRenderer
      assertEquals(label.getClass, label.clone().getClass)
      assertEquals(label.getUniqueId, label.clone().getUniqueId)
    }

  // ------------------------------------------------------------------ layouts vs the real jars (bytecode only)

  private def classBytes(fqn: String): Option[Array[Byte]] = {
    val roots = OrderEnumerator.orderEntries(getModule).librariesOnly().classes().getRoots.toList
    roots.view.flatMap(r => Option(r.findFileByRelativePath(fqn.replace('.', '/') + ".class"))).headOption.map(_.contentsToByteArray())
  }

  /** Field names declared by `fqn` and its superclasses, read with ASM from the module's library jars. */
  private def fieldsOf(fqn: String): Set[String] = {
    if (classBytes(fqn).isEmpty) fail(s"$fqn not found in the fixture's libraries")
    val out                  = mutable.Set.empty[String]
    var next: Option[String] = Some(fqn)
    while (next.isDefined) {
      next = classBytes(next.get).flatMap { b =>
        var superName: String = null
        new ClassReader(b).accept(
          new ClassVisitor(Opcodes.ASM9) {
            override def visit(v: Int, a: Int, n: String, s: String, sup: String, is: Array[String]): Unit = superName = sup
            override def visitField(a: Int, n: String, d: String, s: String, value: Any): FieldVisitor = { out += n; null }
          },
          ClassReader.SKIP_CODE
        )
        Option(superName).map(_.replace('/', '.')).filterNot(_ == "java.lang.Object")
      }
    }
    out.toSet
  }

  def testRendererLayoutsMatchTheLibraries(): Unit = {
    val expected = Seq(
      Fqn.NonEmptyList                                -> Seq("head", "tail"),
      Fqn.NonEmptyVector                              -> Seq("toVector"),
      Fqn.ChainSingleton                              -> Seq("a"),
      Fqn.ChainAppend                                 -> Seq("leftNE", "rightNE"),
      Fqn.ChainWrap                                   -> Seq("seq"),
      Fqn.Valid                                       -> Seq("a"),
      Fqn.Invalid                                     -> Seq("e"),
      Fqn.IorLeft                                     -> Seq("a"),
      Fqn.IorRight                                    -> Seq("b"),
      Fqn.IorBoth                                     -> Seq("a", "b"),
      Fqn.EitherT                                     -> Seq("value"),
      Fqn.OptionT                                     -> Seq("value"),
      Fqn.SyncRef                                     -> Seq("ar"),
      Fqn.Cons                                        -> Seq("head", "next"),
      Fqn.Vector1                                     -> Seq("prefix1"),
      "scala.collection.immutable.ArraySeq$ofRef"     -> Seq("unsafeArray")
    )
    val missing = expected.flatMap { (cls, names) =>
      val fields = fieldsOf(cls)
      names.filterNot(fields).map(n => s"$cls.$n (has: ${fields.mkString(", ")})")
    }
    assertTrue(missing.mkString("\n"), missing.isEmpty)
    // Ref's current value lives in the JDK's AtomicReference
    assertNotNull(classOf[java.util.concurrent.atomic.AtomicReference[?]].getDeclaredField("value"))
  }

  def testRendererTargetClassesExist(): Unit =
    Seq(
      Fqn.NonEmptyList,
      Fqn.NonEmptyVector,
      Fqn.Chain,
      "cats.data.Chain$Empty$", // the renderer recognises an empty Chain by this name prefix
      Fqn.Validated,
      Fqn.Ior,
      Fqn.EitherT,
      Fqn.OptionT,
      Fqn.SyncRef
    ).foreach(c => assertTrue(s"$c missing from the jars", classBytes(c).isDefined))
}
