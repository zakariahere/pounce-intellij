# G1: compiler-backed completion (spike report)

Status: design only. No prototype was built, because every workable design needs library dependencies
that the build does not have (see "Build requests"). The pillar rules say a new dependency is reported,
not added.

Recommendation: **no-go for v1.** Defer G1. If accuracy problems remain after the Pounce oracle ships,
build the cheaper "G1-lite" first (SemanticDB-backed facts, see below). Then decide on a real
presentation compiler, based on measured miss rates.

---

## 1. Goal

Pounce's completion and inspections reason about types through the Scala plugin's type checker
(`ScExpression.type()`, implicit search) plus Pounce's own instance oracle. Where the Scala plugin's
checker is wrong (PLAN.md root cause #3: complex implicits, tagless-final `F[_]` bounds,
kind-projector, macro-generated instances), Pounce inherits the error. G1 asks the real compiler
for these cases, through the Metals presentation compiler (PC) interface.

## 2. The interface

Metals splits the PC into two artifacts:

| Artifact | Contents | Scala version |
|---|---|---|
| `org.scalameta:mtags-interfaces` | Java interfaces: `scala.meta.pc.PresentationCompiler`, `OffsetParams`, `VirtualFileParams`, `CancelToken`, `SymbolSearch`, `PresentationCompilerConfig` | none (Java) |
| `org.scalameta:mtags_2.13.18` | Scala 2 implementation `scala.meta.internal.pc.ScalaPresentationCompiler`, compiled against the exact scalac | must match the project, patch-exact |
| `org.scala-lang:scala3-presentation-compiler_3` | Scala 3 implementation `dotty.tools.pc.ScalaPresentationCompiler`; part of the Scala 3 release since 3.3.4 / 3.5.0 | must match the project's Scala 3 version |

The interface methods G1 would use (they return `CompletableFuture`; the result types come from lsp4j):

```java
PresentationCompiler newInstance(String buildTargetId, List<Path> classpath, List<String> options);
PresentationCompiler withConfiguration(PresentationCompilerConfig config);
PresentationCompiler withExecutorService(ExecutorService ec);
PresentationCompiler withScheduledExecutorService(ScheduledExecutorService ec);
PresentationCompiler withSearch(SymbolSearch search);      // optional: workspace symbols, auto-import
CompletableFuture<CompletionList> complete(OffsetParams params);
CompletableFuture<Optional<Hover>> hover(OffsetParams params);  // "what type is this?"
CompletableFuture<List<Diagnostic>> didChange(VirtualFileParams params);
void didClose(URI uri);
void restart();
void shutdown();
```

## 3. Design

### 3.1 Components

```
pounce.accuracy (new package, behind a setting that is OFF by default)
 ├─ PcArtifacts        resolves + caches the PC jars per Scala version (coursier)
 ├─ PcClassLoader      isolated URLClassLoader; the parent exposes only scala.meta.pc.* and org.eclipse.lsp4j.*
 ├─ PcInstances        project service: one PC per (module, scope), LRU-bounded, shut down on idle
 ├─ ModuleCompileInfo  classpath + scalac options + Scala version from IntelliJ's module model
 ├─ PcQueries          complete / typeAt with timeout and cancellation
 └─ PcCompletionContributor   merges PC items into Pounce completion (dedupe, ranking, icons)
```

### 3.2 Sketch (not compiled: the types need the missing dependencies)

```scala
final case class CompileInfo(scalaVersion: String, classpath: Seq[Path], options: Seq[String])

object ModuleCompileInfo {
  def of(module: Module): Option[CompileInfo] =
    for {
      version <- module.scalaMinorVersion.map(_.minor)            // Scala plugin: exact version of the module's SDK
      // IntelliJ model: dependencies + compiler output dirs of this module and the modules it depends on
      cp       = OrderEnumerator.orderEntries(module).recursively().withoutSdk().classes().getPathsList.getPathList.asScala
      options  = ScalaCompilerConfiguration.instanceIn(module.getProject).getSettingsForModule(module).getOptionsAsStrings.asScala
    } yield CompileInfo(version, cp.map(Path.of(_)).toSeq, options.toSeq)
}

final class PcInstances(project: Project) extends Disposable {
  private val cache = new LruCache[(Module, Boolean), PresentationCompiler](maxSize = 3) // closes the evicted PC

  def forFile(file: ScalaFile): Option[PresentationCompiler] =
    for {
      module <- Option(ModuleUtilCore.findModuleForPsiElement(file))
      info   <- ModuleCompileInfo.of(module)
      jars   <- PcArtifacts.cached(info.scalaVersion)   // never resolved on the EDT or inside completion
    } yield cache.getOrElseUpdate((module, isTest(file)), {
      val loader = PcClassLoader(jars, parent = classOf[PresentationCompiler].getClassLoader)
      val cls    = if (info.scalaVersion.startsWith("3.")) "dotty.tools.pc.ScalaPresentationCompiler"
                   else "scala.meta.internal.pc.ScalaPresentationCompiler"
      loader.loadClass(cls).getDeclaredConstructor().newInstance().asInstanceOf[PresentationCompiler]
        .withExecutorService(AppExecutorUtil.getAppExecutorService)
        .withScheduledExecutorService(AppExecutorUtil.getAppScheduledExecutorService)
        .newInstance(module.getName, info.classpath.asJava, info.options.asJava)
    })
}

/** Completion: ask the PC with a hard deadline, never block longer than the user notices. */
def pcItems(pc: PresentationCompiler, file: VirtualFile, text: String, offset: Int): Seq[CompletionItem] = {
  val token  = new IndicatorCancelToken(ProgressManager.getGlobalProgressIndicator) // cancel = PC cancel
  val params = SimpleOffsetParams(file.toNioPath.toUri, text, offset, token)      // our OffsetParams impl
  try pc.complete(params).get(400, TimeUnit.MILLISECONDS).getItems.asScala.toSeq
  catch { case _: TimeoutException | _: CancellationException => Nil }
}
```

### 3.3 Where the classpath and options come from

- Classpath: `OrderEnumerator.orderEntries(module).recursively()` gives library jars plus the compiler
  output directories of the module and of the modules it depends on. For sbt-imported projects these
  are sbt's `target/scala-2.13/classes` folders.
- scalac options: the Scala plugin's per-module compiler settings (imported from sbt `scalacOptions`).
  Compiler plugins (kind-projector, better-monadic-for) come as `-Xplugin:<jar>` options. The PC
  supports them, but the plugin jars must be on disk; sbt import already downloads them.
- Scala version: the Scala SDK of the module. It must be patch-exact, because mtags is published
  per patch version.

### 3.4 Merging with Pounce lookups

- Run as an additional completion contributor ordered after Pounce's own contributor. Call
  `result.runRemainingContributors` to see what the Scala plugin and Pounce already offer.
- Add only items that are missing (key: name plus erased signature), or that fix a wrong type
  (the PC detail says `F[Unit]` where Scala-plugin PSI says `Nothing`).
- Map the LSP `CompletionItem` to a `LookupElementBuilder`: label, `detail` as the type text,
  `kind` to an icon, `textEdit` plus `additionalTextEdits` as an `InsertHandler`. The additional
  edits carry auto-imports; apply them through the Scala plugin's import mechanism, not as raw
  text edits.
- A better first use than completion: `hover(offset)` as an oracle for "what is the type here /
  which implicit was picked". Pounce's inspections and the instance oracle can use it to confirm or
  veto a decision. One request per inspection run, cached per document stamp.

## 4. Risks

| Risk | Impact | Mitigation |
|---|---|---|
| **Stale or missing class files.** The PC compiles only the open file. Everything else comes from the output dirs. Without a recent build, symbols from the same module are missing or out of date. | Wrong or missing completions; can be worse than the Scala plugin | Use only after a build; show "compile to enable"; never use PC results to *remove* Scala-plugin items |
| **Memory.** One PC per module and scope keeps a full scalac symbol table: hundreds of MB per instance on large classpaths (Metals experience; not measured in this spike). | IDE heap pressure | LRU of 2-3 instances; idle shutdown; OFF by default |
| **Latency.** A cold start loads the classpath (seconds). A warm completion type-checks the file up to the caret: typically tens to a few hundred ms, more on large files (not measured in this spike). | Completion feels slow | Hard 300-400 ms deadline; warm up in the background when a file opens; cache per document stamp |
| **Version matrix.** mtags exists only for Scala versions that a Metals release supports. Old patch versions are dropped over time. Scala 3 needs 3.3.4+ for the in-compiler PC. | No PC for some projects | Detect and disable per module; resolve older Metals releases for older 2.13 patches |
| **Class loading.** The plugin runs on the Scala plugin's Scala 3.7.4 runtime. The PC brings its own scala-library 2.13.18 and scalac. A leak in either direction gives `LinkageError`. | Crashes | Strict parent filter: only `scala.meta.pc.*` and `org.eclipse.lsp4j.*` go through |
| **Downloads at runtime.** About 30-60 MB of compiler jars per Scala version, from Maven Central. | Corporate proxies, offline machines, supply chain | Ask the user first; respect IntelliJ proxy settings; checksums (coursier); cache in the IDE system dir; offline mode with a manual jar dir |
| **Two type checkers disagree.** Users see one answer in highlighting (Scala plugin) and another in completion (PC). | Confusion | Use the PC only to *add* items and to confirm or veto inspections, never to rewrite Scala-plugin highlighting |
| **Maintenance.** Every Scala release and every Metals interface change needs a check. | Ongoing cost | Pin a Metals version per Pounce release; integration test per supported Scala version |

## 5. Effort

| Step | Scope | Estimate |
|---|---|---|
| Spike | Hard-coded Scala 2.13.18, jar folder given in settings, one module, `hover` + `complete`, no merging polish | 1-2 weeks |
| Usable feature | coursier resolution and cache, per-module lifecycle, cancellation, merging and dedupe, settings UI, error reporting | 4-6 more weeks |
| Production | Scala 3 path, version matrix tests (2.13.x and 3.x), memory and latency budgets, proxy and offline support, docs | 3-4 more weeks |
| Ongoing | each Scala or Metals release | 1-3 days per release |

Total: about 2-3 months for one engineer before it is better than "off".

## 6. Go / no-go

**No-go for v1.** Reasons:

1. The main failure mode (stale class files without a fresh build) hits exactly the users who need
   help most: people in the middle of editing code that does not compile yet.
2. The cost (memory, downloads, version matrix) is paid by every user who turns it on. The benefit
   only shows in a minority of expressions.
3. Pounce's own oracle (static instance tables plus the Scala plugin's implicit search) has not been
   measured yet. Without a miss-rate baseline, G1 cannot show what it adds.

**Alternative: G1-lite (SemanticDB facts).** Many sbt builds already have `semanticdbEnabled := true`
(Metals and scalafix turn it on). The compiler then writes `META-INF/semanticdb/**/*.semanticdb`
next to the class files. These record, for every position of the last successful compile, the
resolved symbol, the inferred types, and the synthetics (implicit arguments and conversions that
were inserted). This gives Pounce:

- the instance the real compiler picked at a call site (to confirm or veto the oracle),
- the real type of an expression in compiled code,
- with no compiler in the IDE, no downloads, and no class-loader isolation.

It goes stale like G1 does, but it costs much less. It needs only the SemanticDB protobuf reader
(`org.scalameta:semanticdb-shared` or a small generated protobuf class), which is another build
request. Estimate: 1-2 weeks for a reader and an oracle hook.

Suggested order: ship v1 without G1 → measure oracle misses (log the expressions where the Scala
plugin's type is `Nothing` or an implicit is unresolved but the file compiles) → G1-lite → G1 only
if misses stay high in code that compiles.

## 7. Build requests (not applied; the pillar rules forbid build edits)

For a G1 spike:

```scala
// build.sbt, runtime (or a new "accuracy") project
libraryDependencies ++= Seq(
  "org.scalameta"   % "mtags-interfaces" % "<metals version that supports 2.13.18>", // Java only, small
  "io.get-coursier" % "interface"        % "<latest 1.0.x>"                           // Java API, shaded; resolves mtags at runtime
)
```

- Do not add `org.eclipse.lsp4j`. The IDE already ships `lib/eclipse.lsp4j.jar` and
  `lib/eclipse.lsp4j.jsonrpc.jar` (checked in SDK 262.9437.185). Whether a plugin can use them
  without `<depends>` on the LSP module must be checked. If not, bundle lsp4j at the version that
  mtags-interfaces expects.
- `mtags_2.13.18` itself must **not** be a build dependency: it is resolved at runtime per project
  Scala version and loaded in an isolated class loader.

For G1-lite:

```scala
libraryDependencies += "org.scalameta" %% "semanticdb-shared" % "<scalameta version>" // or a generated protobuf reader
```

Exact versions must be checked against Maven Central when the work starts. This spike had no
network access to confirm which Metals release first publishes `mtags_2.13.18`.
