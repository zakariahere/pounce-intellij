# Contributing to Pounce

Thanks for helping! The highest-value contributions need almost no IntelliJ knowledge:

1. **Catalog entries.** Teach completion, quick docs, the Typeclass Explorer and the Combinator Finder about a new operation. It takes about five lines of HOCON.
2. **Simplification rules.** Teach the inspections a new rewrite. It takes about ten lines of Scala plus one-line tests.

## Setup

- **JDK 25**: IntelliJ 2026.2's jars are Java 25 class files. Point `JAVA_HOME` at it before starting sbt.
- **sbt 1.12.x**. The first `sbt compile` downloads the IntelliJ SDK and the Scala plugin into `~/.pouncePluginIU`.
- Open the repo in IntelliJ IDEA with the Scala plugin (import as sbt). The plugin itself is written in **Scala 3**;
  it *supports* Scala 2.13 and Scala 3 user code.

```bash
sbt compile
sbt core/test                    # engine tests (fast unit tests + fixture tests)
sbt inspections/test             # one feature area
sbt "inspections/testOnly pounce.inspections.simplifications.SimplifyVoidTest"
sbt runIDE                       # sandbox IDE with the plugin installed
```

Each fixture test boots a headless IntelliJ with the Scala plugin and real cats, cats-effect and Play jars, which are
resolved through Ivy and cached. Expect 15–40 s per test class.

## Project layout

| Folder | What lives there |
|---|---|
| `core/` | The engine: type language (`types/Ty`), catalog, typeclass hierarchy, instance oracle, imports, settings, `plugin.xml` |
| `completion/` | Pillar A: completion contributors, postfix templates, quick docs |
| `inspections/` | Pillars B + C: effect-safety inspections and simplifications |
| `visual/` | Pillar D: inlay hints, gutters, Pounce tool window |
| `play/` | Pillar E: config keys, Guice, i18n, IO bridge |
| `runtime/` | Pillar F: debugger, fiber dump, weaver, live templates |
| `scala-plugin-testkit/` | Vendored Scala plugin test framework. Keep it pristine (see its `ORIGIN.md`) |
| `samples/play3-cats/` | Showcase app for manual testing |

Each pillar registers its extensions in `<pillar>/src/main/resources/META-INF/pounce-<pillar>.xml`. Core's
`plugin.xml` includes these with `xi:fallback`, so a pillar loads on its own in its tests and together with the
others in the real plugin.

---

## Add a catalog entry in 5 lines

The catalog lives in `core/src/main/resources/pounce/catalog/*.conf` (HOCON); `index.conf` lists the files.
An entry describes the operation's *shape* using single upper-case letters as type variables, and the typeclasses
those variables need:

```hocon
{ name = traverse, owner = Traverse, syntax = traverse, receiver = "F[A]", params = ["A => G[B]"], result = "G[F[B]]"
  requires { F = Traverse, G = Applicative }, rank = 95
  doc = "Map each element to an effect and collect the results inside that effect."
  example = "ids.traverse(id => fetchUser(id))   // IO[List[User]]"
  see-also = [traverse_, flatTraverse, parTraverse, sequence] }
```

| Field | Meaning |
|---|---|
| `name`, `owner` | Method name and the typeclass/companion that defines it. `id` defaults to `owner.name` |
| `kind` | `syntax` (extension on `F[A]`), `lift` (extension on any value), `tuple`, `method`, `companion`. Usually set once at the top of the file |
| `variant = summoner` | A typeclass member called through its summoner (`Sync[F].delay`, `Spawn[F].start`). `F = <owner>` is implied, so you don't write that `requires` yourself |
| `syntax` | The cats syntax module that provides it (`functor` → `cats.syntax.functor._`; `effect.spawn` → `cats.effect.syntax.spawn._`), or a full package root imported as-is (`scala.concurrent.duration` → `scala.concurrent.duration._`). Used for auto-imports |
| `receiver` / `params` / `result` | The shape. Partial unification works: `F[A]` matches `Either[String, Int]` with `F = Either[String, *]` |
| `requires` | Typeclass constraints on shape variables. Must be names known to `core/.../Typeclasses.scala` |
| `rank` | 0–100. Higher ranks are offered first |
| `insert` | Optional template; `\|` marks the caret (the **last** `\|`, so operator names like `\|+\|` are safe) |
| `doc`, `example`, `see-also`, `tags`, `since` | Shown in quick docs and the tool window |

Then run `sbt "core/testOnly pounce.core.CatalogTest"`. It checks unique ids, that every `requires` names a known
typeclass and a variable used in the shape, and that extensions have a receiver and a syntax module.

**Accuracy matters more than coverage.** A wrong `requires` produces completions that don't compile. If the
instance table in `core/.../StaticInstances.scala` doesn't know a type, Pounce falls back to the Scala plugin's
implicit search, so you don't need to list every type there.

## Add a simplification in 10 lines

Simplifications live in `inspections/src/main/scala/pounce/inspections/simplifications/`. A rule is a partial
function from an expression to a `Rewrite`, written with the Scala plugin's collection extractors:

```scala
object SimplifyVoid {
  val hint = "Replace with .void"

  val mapToUnit: Rule = Rule("fa.map(_ => ()) can be replaced with fa.void", hint) {
    case e @ fa `.map` ConstFn(UnitLit()) if isEffect(fa) =>
      Rewrite(e, s"${op(fa)}.void", syntax = Some("functor"), from = Some(fa))
  }

  val rules: Seq[Rule] = Seq(mapToUnit /*, ... */)
}
final class SimplifyVoidInspection extends SimplificationInspection(SimplifyVoid.rules)
```

The base class does the rest:
- It **validates every rewrite** before offering it. The replacement text is built as real code in the original context; it must resolve, find its implicits, and keep a compatible type (`Check.SameType`, or `Check.ExpectedType` for things like `Some(x)` → `x.some`).
- It reports **one problem per expression**: the first matching rule wins.
- Its quick fix **adds the cats syntax import** named by `syntax` when it isn't in scope, in the file's style.

Adding a rule to an existing inspection is just another `Rule` in `rules`. A new inspection also needs:

1. A `<localInspection language="Scala" shortName="PounceSimplifyXxx" … implementationClass="…"/>` entry in
   `inspections/src/main/resources/META-INF/pounce-inspections.xml`.
2. `inspections/src/main/resources/inspectionDescriptions/PounceSimplifyXxx.html` with a short before/after example.
   The platform refuses inspections without it.

Tests are one-liners:

```scala
class SimplifyVoidTest extends SimplificationTestBase[SimplifyVoidInspection] {
  import SimplifyVoid.*
  def testMapToUnitHighlight(): Unit = assertHighlight(mapToUnit, ce(s"io.${START}map(_ => ())$END"))
  def testMapToUnitFix(): Unit       = assertFix(mapToUnit, ce("io.map(_ => ())"), ce("io.void"))
  def testMapToUnitNegative(): Unit  = assertNone(ce("io.map(x => { println(x); () })"))
  def testCollectionIsNotReported(): Unit = assertNone(ce("ids.map(_ => ())"))
}
```

Always include **negative** tests. False positives are the fastest way to get a plugin uninstalled.

---

## Rules of the road

- **Gate on the project.** Bail out early with `Libraries.hasCats(element)` / `hasCatsEffect` / `hasPlay` and the
  matching `PounceSettings()` flag. Completion and inspections run constantly.
- **Never capture PSI in a `CachedValueProvider` lambda.** Capture `project`/`module` only. The test framework
  fails the test with "Provider … is retaining PSI".
- **Line markers go on leaf identifiers,** and navigation targets are computed lazily (on click).
- **No `--` inside XML comments** in `plugin.xml` / `pounce-*.xml`. That's invalid XML: IntelliJ logs a warning and
  silently drops the **whole plugin**. `PluginWiringTest` (root project) catches it, along with misspelled class names.
- **JUnit 3 test classes in Scala 3:** a by-name block inside a `testXxx` method compiles to a synthetic
  `testXxx$$anonfun$1` that JUnit reports as a broken test. Move such bodies into private helpers.
- **Test fixtures:** use `pounce.testing.PounceFixtureTestBase` / `PounceInspectionTestBase`. More libraries go in
  `extraLibraries` via `IvyManagedLoader(...)`. It is not transitive, so list every jar your test code needs.
- **Scala 3 user code:** override `defaultVersionOverride` with `Some(ScalaVersion.Latest.Scala_3)`.

### Windows packaging notes

sbt-idea-plugin repackages the plugin into `<project>/target/plugin/pounce` before each test run. On Windows, if a
previous run still holds that jar, packaging can fail with "file is being used by another process" and then keep
failing with `FileSystemAlreadyExistsException` for that path until the sbt server restarts. Its incremental
packager has also been seen to leave out newly created class files. If you hit either problem, restart sbt. Or, for the
current session only, point packaging at a fresh directory:

```
set inspections / packageOutputDir := (inspections / target).value / "plugin-alt" / "pounce"
```

Inside a `set` expression, name the project by its `build.sbt` val: `root` for the merged plugin, even though the
command line calls it `pounce` (`pounce/test`). `set pounce / …` fails with "not found: value pounce".

## Releasing

```bash
sbt "pounce/testOnly pounce.PluginWiringTest"   # the packaged jar matches the sources
sbt packageArtifactZip                          # target/pounce-<version>.zip
sbt runPluginVerifier                           # against the target IDE build
```

- sbt-idea-plugin puts the **packaged jar** on the test classpath instead of compiled classes. `PluginWiringTest`
  therefore checks the jar against the descriptors in the *source tree*. If it reports "Stale descriptors" or classes
  "missing from pounce.jar", the incremental packager reused an old jar: restart sbt and run `clean`, or package to
  a fresh directory, before building the zip.
- Unzip the artifact once and check that `META-INF/plugin.xml` has the real `<version>`, not `replaced-by-build`.
- `runPluginVerifier` fails on compatibility problems, an invalid descriptor, missing dependencies and
  override-only / non-extendable misuse. Deprecated, experimental and internal API usages are reported but not fatal.
  Many of them are compiler-generated mixin forwarders (a Scala class implementing a Java interface gets a stub per
  default method), not calls in our code.
- The plugin ID `lu.zakaria.pounce` (the reverse DNS of the project site, pounce.zakaria.lu) is permanent once
  published on the Marketplace. Marketplace IDs may not contain "intellij". IntelliJ identifies a plugin only by its
  ID, so a build with another ID installs next to the old one instead of replacing it.

## Commit and PR conventions

- One feature or fix per PR, with tests.
- Run `sbt fmt` (scalafmt) before pushing. `scala-plugin-testkit/` is excluded on purpose.
- If you adapt code from zio-intellij (Apache-2.0), add an `// Adapted from zio-intellij (Apache-2.0): <file>` comment.
