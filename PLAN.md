# Pounce — Typelevel + Play superpowers for IntelliJ IDEA

> Working name. Others to check for collisions: *Purr*, *Kitten*, *Whiskers*.
> ("Catnip" and "Meow" are already Scala library names.)

**Main target:** Scala **2.13.18** · Play **3.x** (Pekko, Guice) · cats **2.x** · cats-effect **3.x**
**Second:** Scala 3 (same features, fixed up after 2.13 works)
**IDE baseline:** IntelliJ IDEA **2026.2** (build `262.9437`) · Scala plugin **2026.2.18**. Support the latest two IDE majors, the same policy as zio-intellij.

---

## 1. Why IntelliJ feels bad for Cats today (root causes)

These causes decide what we build. We don't polish surface UX until they are fixed.

| # | Root cause | What you feel |
|---|---|---|
| 1 | **Cats syntax sits behind imports.** `.traverse`, `.mapN`, `.asRight`, `.some`, `\|+\|` and the rest are extension methods from implicit conversions in `cats.syntax.*`. Without the import, the Scala plugin has nothing to offer. | You type `xs.trav…` and get nothing. You have to remember the method name *and* the import. |
| 2 | **The fallback is generic.** Pressing Ctrl+Space twice runs `ScalaGlobalMembersCompletionContributor`. It scans every implicit conversion on the classpath. It is slow and unranked. It doesn't know your import style or whether a typeclass instance exists. | You get noise, or you give up and type it from memory. |
| 3 | **The plugin checks implicits itself, not with scalac.** Tagless-final code (`F[_]: Sync`) with syntax chains is where it diverges most from scalac. | Red code that compiles, and missing completions. |
| 4 | **The IDE has no domain model.** The Spring plugin understands beans, wiring, endpoints and config. Nothing in IntelliJ understands effects, typeclasses, tagless algebras, `Resource`, or Play DI and config. | There is no navigation, no "this IO is never run" warning, and no config-key completion. |
| 5 | **Scala 2.13 implicit errors are cryptic.** For example: `could not find implicit value for parameter ev: Functor[F]`. | You lose minutes on each missing bound or missing instance. |

## 2. What already exists (don't rebuild it)

- **JetBrains' Play support is good.** It is in the Ultimate module `scalaUltimate.play` of your installed Scala plugin. It covers the routes file language, Twirl templates, controller ↔ route ↔ template navigation and line markers, the routes URL-clash inspection, Play run configurations and rename across routes. We **do not** rebuild any of this.
  - It has **no** Guice DI navigation, **no** `application.conf` key completion from Scala code, **no** `conf/messages` key support, and **no** Endpoints tool window integration. Those gaps are ours.
- **zio-intellij** ([github.com/zio/zio-intellij](https://github.com/zio/zio-intellij), Apache-2.0) is **our blueprint**:
  - It is written in Scala 3 with sbt + `sbt-idea-plugin` 6.x.
  - Its inspections are short `SimplificationType` pattern matches, for example ``case qual `.map` `_ => ()`() => replace with qual.unit``.
  - It also ships a test runner, debugger stepping filters, a "forked code" gutter, type-alias suggestions and a vendored Scala-plugin testkit.
- **No Cats or Typelevel IntelliJ plugin exists.** The niche is open.
- **Bundled things we can use:** the `intellij-hocon` plugin (HOCON PSI), Ultimate's `microservices` Endpoints API (used by the Ktor and Micronaut plugins), and the HTTP Client.

## 3. Feature pillars

Effort: S = days · M = 1–2 weeks · L = 3+ weeks (evenings & weekends pace)
Impact: ★ to ★★★ (★★★ = you feel it every hour)

### Pillar A — Typeclass-aware completion ("Cats Lens") ★★★

The headline feature. You type `.` and see exactly the Cats methods that apply to this receiver, ranked, with the import added for you.

| ID | Feature | Example | Effort |
|---|---|---|---|
| A1 | **Syntax completion without imports.** Only methods whose required instance *resolves* for the receiver are shown. Picking one adds the import in your project's style (`cats.syntax.all._` / `cats.implicits._` / granular). | `xs: List[Either[E, A]]` → `xs.` offers `sequence`, `traverse`, `flatTraverse`, `foldMap`, `combineAll`, `separate` | L |
| A2 | **Tuple / applicative completion** | `(fa, fb).` → `mapN`, `tupled`, `parMapN`, `parTupled`, `traverseN` | M |
| A3 | **Lifting completion on plain values, ranked by expected type** | Expected type is `EitherT[F, Err, A]` and you type `err.` → `EitherT.leftT` comes first. Also: `a.pure[F]`, `.some`, `.asRight[E]`, `.validNel`, `.raiseError[F, A]` | M |
| A4 | **Smart completion by expected type** (Ctrl+Shift+Space) | Expected type is `F[List[B]]` and you have `xs: List[A]` → suggests `xs.traverse(???)` | L |
| A5 | **Curated companion completion** for `IO.`, `Sync[F].`, `Ref.`, `Resource.`, `EitherT.`, `OptionT.`. Grouped as constructors / error handling / concurrency, with the bounds in scope respected (`F[_]: Async` unlocks `Async[F].fromFuture`). | `IO.` → `blocking`, `delay`, `raiseError`, `sleep`, `ref`, `deferred`, … | M |
| A6 | **Postfix templates** | `expr.io` → `IO(expr)`, `.blocking`, `.delay`, `.pure`, `.some`, `.right`, `.eitherT`, `.optionT`, `.liftF`, `.resource`, `.attempt`, `.for` (wrap in a for-comprehension) | S |
| A7 | **Rich quick-doc (Ctrl+Q).** Shows a simplified signature, which typeclass the method comes from, a 2-line example, "see also" (`traverse_`, `parTraverse`, `flatTraverse`) and the import it needs. | — | M |

**How it works:**
- We build a **catalog** of Cats, cats-effect and fs2 operations. Section 4.2 describes it.
- At completion time, we match the receiver type from the Scala plugin's `ScExpression.type()` against each entry's receiver shape.
- Then we check the required instances. A **static instance table** covers the common types: List, Option, Either, Vector, Map, NonEmptyList, Chain, IO, EitherT, OptionT, Kleisli, Resource. That makes the check almost free.
- For unknown types, and for `F[_]` with bounds, we read the bounds in scope and walk the typeclass hierarchy (`Sync` ⇒ `MonadThrow` ⇒ `Monad` ⇒ …).
- Only as a last resort do we run the Scala plugin's implicit search on a synthetic `implicitly[Traverse[X]]`. Results are cached per (type constructor, typeclass, module).

### Pillar B — Effect-safety inspections (catch real bugs while you type) ★★★

| ID | Inspection | Severity | Quick fixes | Effort |
|---|---|---|---|---|
| B1 | **Discarded effect.** A statement of effect type (`IO`, `F[_]` with Monad, `EitherT`, `Resource`, `Stream`) is not the last expression of the block, so it **never runs**. This is the #1 real-world cats-effect bug. | Error | Chain with `*>` / `>>`, convert to a for-comprehension, `.void` | M |
| B2 | **Nested effect.** `io.map(x => IO.println(x))` gives `IO[IO[Unit]]`, so the inner effect never runs. Same for `List[IO[A]]` from `map` where `traverse` was meant. | Warning | `map` → `flatMap`, `map` → `traverse` | S |
| B3 | **Blocking call in `IO(...)` / `Sync[F].delay(...)`.** Detected calls: `Thread.sleep`, `java.sql.*`, `java.io.File*`, `Files.*`, `scala.io.Source`, `Await.result`, `HttpURLConnection` and similar. | Warning | → `IO.blocking` / `Sync[F].blocking` / `IO.interruptible`. `Thread.sleep(n)` → `IO.sleep(n.millis)` | M |
| B4 | **`unsafeRun*` outside the edge of the world.** Allowed in `IOApp`, `main`, tests, and **Play 3 controller actions**: `unsafeToFuture()` in `Action.async` is the legitimate Play bridge. Configurable allow-list. | Warning | — | S |
| B5 | **Eager `Future` captured.** `IO.fromFuture(IO.pure(fut))`, or a `Future` val built outside the `IO`, is already running. | Warning | → `IO.fromFuture(IO(fut))` | S |
| B6 | **Fiber leaks.** A `.start` result is never joined or cancelled. Also unbounded `parTraverse` on a large or unknown collection. | Weak warning | → `.background` / `Supervisor`, → `parTraverseN(n)` | M |
| B7 | **`Resource` escapes its scope.** For example `.use(IO.pure)` / `.use(x => IO(x))` returning the resource, or `.allocated` without a release. | Error | — | S |
| B8 | **Tagless bound mismatches.** `.flatMap` called on `F[A]` with only `F[_]: Functor`. Redundant bounds (`F[_]: Monad: Sync`). Unused bounds. | Error / weak | "Strengthen bound to `Monad`", "Remove redundant `Monad` (implied by `Sync`)" | M |
| B9 | **Conflicting Cats imports.** `cats.implicits._` together with `cats.syntax.all._` or `cats.syntax.functor._` causes ambiguous implicits. `cats.instances._` has been unnecessary since cats 2.2. | Warning | Normalize to the project's style | S |
| B10 | **Mixed effects in a for-comprehension.** One arrow is `Option`, the next is `IO`. Scala 2.13 reports a confusing type mismatch. | Error | `OptionT(...)`, `.liftTo[F](err)`, `EitherT.fromOption`, `F.fromOption` | M |
| B11 | **Implicit-not-found explainer (2.13).** When `Functor[F]` / `Encoder[A]` / `Meta[A]` can't be found, explain *why*: which candidates were tried, which bound is missing, which import is missing. | Annotator + popup | "Add context bound `F[_]: Functor`", "Add import `cats.syntax.all._`" | L |

### Pillar C — Simplifications (zio-intellij style, short pattern matches) ★★

Each rule is about 10 lines on the Scala plugin's `SimplificationType`. This is the easiest place for open-source contributors to add rules.

| Before | After |
|---|---|
| `fa.map(_ => ())` | `fa.void` |
| `fa.map(_ => b)` | `fa.as(b)` |
| `fa.flatMap(_ => fb)` | `fa >> fb` (or `*>` when `fb` is a val) |
| `fa.flatMap(a => F.pure(f(a)))` | `fa.map(f)` |
| `fa.map(f).flatten` / `fa.flatMap(identity)` | `fa.flatMap(f)` / `fa.flatten` |
| `xs.map(f).sequence` / `.sequence_` | `xs.traverse(f)` / `xs.traverse_(f)` |
| `xs.traverse(identity)` | `xs.sequence` |
| `xs.map(f).combineAll` / `foldLeft(Monoid.empty)(_ \|+\| _)` | `xs.foldMap(f)` / `xs.combineAll` |
| `F.pure(())` / `IO.pure(())` | `F.unit` / `IO.unit` |
| `if (c) fa else F.unit` | `fa.whenA(c)` (and `unlessA`) |
| `fa.attempt.flatMap { case Left(e) => …; case Right(a) => … }` | `fa.redeemWith(…, …)` |
| `fa.map(f).handleError(g)` | `fa.redeem(g, f)` |
| `for { a <- fa } yield a` / single generator | `fa` / `fa.map(…)` |
| `x <- F.pure(v)` inside a for-comprehension | `x = v` |
| `_ <- F.unit` inside a for-comprehension | *(removed)* |
| `(fa, fb).mapN((a, b) => (a, b))` / `fa.product(fb).map { case (a, b) => … }` | `(fa, fb).tupled` / `(fa, fb).mapN(…)` |
| `EitherT(F.pure(Right(a)))` / `EitherT(F.pure(Left(e)))` | `EitherT.rightT[F, E](a)` / `EitherT.leftT[F, A](e)` |
| `EitherT(fa.map(Right(_)))` | `EitherT.liftF(fa)` |
| `OptionT(F.pure(Some(a)))` / `OptionT(F.pure(None))` | `OptionT.some[F](a)` / `OptionT.none[F, A]` |
| `Right(x): Either[E, A]` · `Some(x)` *(opt-in style rules)* | `x.asRight[E]` · `x.some` |

### Pillar D — The visual layer: hints, gutters and panes ★★★

| ID | Feature | Effort |
|---|---|---|
| D1 | **For-comprehension lens.** An inlay at `for` shows the effect (`IO`, `EitherT[IO, Err, *]`). Each `<-` shows the bound type (`user: User`). A step whose effect differs from the rest is tinted red. That tint is where B10 fires. | M |
| D2 | **Type-alias awareness.** If you define `type App[A] = EitherT[IO, AppError, A]`, hints and "add type annotation" use `App[A]`, and an inspection suggests the alias where it fits. This mirrors zio-intellij's `SuggestTypeAlias`. | S |
| D3 | **Instance hints, scoped to Cats ops.** After `.traverse(` an optional inlay `‹Traverse[List], Applicative[IO]›` appears. Click it to go to the instance. It is quieter than the Scala plugin's global implicit hints. | M |
| D4 | **Gutter icons.** Fork points (`.start`, `.background`, `parTraverse`, `race`). `IO.blocking` (blocking pool). `Resource.make` (jumps between acquire and release). Tagless algebra → **"constructed / wired in"**: where the interpreter is instantiated, as in Spring's bean wiring. | M |
| D5 | **Typeclass Explorer tool window (the pane).** Put the caret on any expression or type to see its normalized shape `F[A]`, the instances that exist (✔ Functor ✔ Monad ✔ Traverse ✘ Alternative), the methods each one unlocks (click to insert), where each instance comes from, and the Cats hierarchy with the strongest instance highlighted. You learn while you work. | L |
| D6 | **Combinator Finder (a Hoogle for Cats).** Type a shape like `List[A] => (A => F[B]) => F[List[B]]` and get `traverse`. Also an intention on `???`: **"Fill hole with a combinator"** that uses the expected type and the values in scope. | L |
| D7 | **Endpoints.** http4s `HttpRoutes.of` patterns (`case GET -> Root / "users" / IntVar(id)`), tapir endpoints and Play 3 routes go into Ultimate's Endpoints tool window (optional dependency), with our own small pane as a fallback. "Open in HTTP Client" on each endpoint. | M |

### Pillar E — Play 3 wiring: Spring-level understanding ★★★

| ID | Feature | Effort |
|---|---|---|
| E1 | **Config keys.** Completion, navigation, rename, find-usages and an "unknown key" inspection for `config.get[String]("app.db.url")`, `getOptional`, `underlying.getString`, `ConfigFactory.load()…`. Keys come from `conf/application.conf` (with `include`s) **and from every library's `reference.conf` on the classpath** (play, pekko, slick, …), with defaults shown in the popup. Also pureconfig case classes ↔ HOCON keys (kebab-case mapping). Uses `intellij-hocon` as an optional dependency. This is the equivalent of Spring's `@Value` / `application.properties` support. | M |
| E2 | **Guice DI navigation.** From an `@Inject()(repo: UserRepo)` constructor parameter, a gutter jumps to the binding: `bind(classOf[UserRepo]).to(classOf[…])`, scala-guice `bind[UserRepo].to[…]`, `@ImplementedBy`, `@Provides` methods, or eager singletons in `Module`. The reverse also works: binding → injection points. **Inspection:** an abstract type is injected but no binding exists, which would be a Guice crash at runtime, caught while you type. | L |
| E3 | **i18n messages.** For `messages("user.greeting")`, `Messages(...)` and `@messages(...)` in Twirl: completion and navigation to `conf/messages*`, a missing-key inspection, a missing-translation inspection (`messages.fr` lacks a key that `messages` has), and rename across files. | M |
| E4 | **Play ↔ cats-effect bridge helpers.** A live template / intention that turns an `IO` action into `Action.async { io.unsafeToFuture()(runtime) }` (or the project's `Dispatcher` helper). Inspection: blocking JDBC inside a non-async `Action { … }`. | S |

**Play 3 specifics to handle:**
- The new groupId is `org.playframework:play_2.13`. The old one is `com.typesafe.play`, used by Play ≤ 2.9.
- Play 3 is on Pekko (`org.apache.pekko`), not Akka.
- Guice is the default DI. We also detect compile-time DI (`BuiltInComponentsFromContext`) and skip E2 there.

### Pillar F — Runtime & tests ★★

| ID | Feature | Effort |
|---|---|---|
| F1 | **Debugger stepping filters** for cats-effect and cats internals, so *Step Into* lands in your code and not in the IO run loop. This mirrors zio-intellij's `ZioSteppingFilterProvider`. Plus **renderers** for `NonEmptyList`, `Chain`, `Validated`, `Ior`, `EitherT` / `OptionT` (shown unwrapped) and `Ref` (current value). | M |
| F2 | **Test framework support.** Check the current state of weaver-test's IntelliJ support and add a runner and gutter icons if it is missing. munit-cats-effect already works through the Scala plugin's munit support; verify it, don't rebuild it. | M |
| F3 | **Fiber dump action.** CE3 dumps fibers on a POSIX signal (`SIGUSR1` / `SIGINFO`). **Windows has no such signal.** Investigate a JMX or agent route before we promise anything. | R&D |

### Pillar G — Accuracy engine (optional R&D, behind a setting) ★★★ if it works

**G1 — compiler-backed completion.** For files where the Scala plugin's own type checker is wrong (root cause #3), ask the real compiler through the Metals presentation-compiler interface (`mtags` for 2.13.18). The classpath comes from IntelliJ's module model. This is a big bet:
- **Memory:** a second compiler instance runs per module.
- **Latency:** each request goes through the compiler, so it is slower than the Scala plugin's own checker.
- **Versions:** mtags must match the exact Scala version.

So it is a phase-5 spike, not a dependency of anything else.

---

## 4. Architecture

### 4.1 Tech stack

- **Plugin code in Scala 3, built with sbt + `sbt-idea-plugin` 6.x.** This is exactly what zio-intellij does.
  - The Scala plugin's API is Scala-first: extractors like `` `.map` ``, `ScExpression.type()` and `SimplificationType`. Kotlin would have to fight it.
  - Pin `scalaVersion` to the runtime that the Scala plugin bundles. For Scala plugin 2026.2.18 that is **Scala 3.7.4**, and for 2026.3 it is 3.9.0.
  - *Plugin language ≠ target language.* The plugin is written in Scala 3. **The code it analyzes is your Scala 2.13.18.**
- **Hard dependency:** `org.intellij.scala`.
- **Optional dependencies** (`<depends optional="true" config-file=…>`): `org.jetbrains.plugins.hocon` (E1) and the Ultimate microservices / Endpoints API (D7). Each is enabled only when it is present.
- **One release branch per IDE build:** `idea262.x`, `idea263.x`. Run the JetBrains Plugin Verifier in CI.

### 4.2 The Catalog: the heart of A, C, D5 and D6

A **data file**, not code. That keeps the ranking in our hands and lets the community add entries without knowing IntelliJ internals.

```yaml
- name: traverse
  owner: cats.Traverse
  receiver: "F[A]"                 # shape matched against the receiver type
  params: ["A => G[B]"]
  result: "G[F[B]]"
  requires: { F: Traverse, G: Applicative }
  imports:
    all: cats.syntax.all._
    implicits: cats.implicits._
    granular: cats.syntax.traverse._
  since: { cats: "2.0" }
  scala: ["2.13", "3"]
  rank: 95                         # prior; adjusted later by local usage stats
  doc: "Map each element to an effect and collect the results inside that effect."
  example: |
    List(1, 2, 3).traverse(id => fetchUser(id))   // IO[List[User]]
  seeAlso: [traverse_, flatTraverse, parTraverse, sequence]
```

- **`catalog-gen`** is a small sbt project. It reads the cats, cats-effect and fs2 jars (TASTy and Scala signatures) and generates the skeleton: names, owners, signatures and requirements. Humans curate `doc`, `example`, `rank` and `seeAlso`.
- **v1 scope:** about 150 entries covering everyday cats-core + cats-effect `IO` / `Sync` / `Async` / `Ref` / `Resource`. Then packs: fs2, http4s, doobie, circe.

### 4.3 Runtime pipeline (completion example)

```
caret after `xs.`
  └─ receiver type        ← Scala plugin: ScExpression.type()        (cached per PSI mod count)
  └─ shape match          ← catalog entries indexed by outer type constructor
  └─ instance check       ← 1. static table (List/Option/IO/…)  2. bounds in scope + hierarchy
                             3. synthetic implicitly[TC[X]] via Scala plugin (cached, last resort)
  └─ rank                 ← catalog prior + expected type fit + your local usage history
  └─ lookup elements      ← insert handler adds the import via ScImportsHolder in your style
```

- **Performance budget:** our contributor adds **< 30 ms** to a completion. Nothing runs on the EDT. `ProgressManager.checkCanceled()` is called in every loop. Caches use `CachedValuesManager` with PSI and library modification trackers.
- **Library detection per module:** we look for `cats-core`, `cats-effect`, `fs2-core`, `play_2.13` (`org.playframework`) and `http4s-*` in each module's libraries. Each feature lights up only where its library is present, so there is **zero noise in non-Cats modules**.
- **Scala 2.13 dialect details:** `_` imports. Kind-projector `*` / `?`. better-monadic-for (cats-effect recommends it on 2.13), which changes how for-comprehensions desugar and affects B1 and B10. `-Xsource:3` projects. Scala 3 dialect support (`*` imports, `using` / `given`) comes in the second wave.
- **Import style detection:** on first run, count the project's existing imports to pick a default. You can override it in **Settings → Languages & Frameworks → Pounce**.

### 4.4 Repository layout

```
pounce/
├─ build.sbt, project/            sbt-idea-plugin, Versions (IDE build, Scala plugin, Scala 3.7.4)
├─ catalog/                       YAML catalog + JSON schema (shipped in the plugin jar)
├─ catalog-gen/                   generator that reads library jars → catalog skeleton
├─ src/main/scala/pounce/
│  ├─ core/        library detection, type shapes, instance oracle, catalog loader, caches
│  ├─ completion/  A1–A5, postfix (A6), quick-doc (A7)
│  ├─ inspections/ mistakes/ (B*), simplifications/ (C*)
│  ├─ hints/       inlays (D1–D3), line markers (D4)
│  ├─ toolwindows/ typeclass explorer (D5), combinator finder (D6), endpoints fallback (D7)
│  ├─ play/        config keys (E1), guice (E2), messages (E3), bridge (E4)
│  └─ debugger/    stepping filters, renderers (F1)
├─ src/main/resources/META-INF/   plugin.xml + optional-dependency xmls
├─ scala-plugin-testkit/          vendored Scala plugin test framework (zio-intellij approach)
└─ testdata/                      before/after fixtures, completion fixtures
```

### 4.5 Testing strategy ("fully good" = few false positives)

- **Fixture tests:** a before/after pair for every inspection and every simplification, plus completion fixtures that assert the *order* of the top lookup items. They load real libraries with the Scala plugin's library loaders, for example `cats-core_2.13`, `cats-effect_2.13` and `play_2.13`.
- **Corpus run (the credibility test):** run every inspection over real code: http4s, doobie, fs2 examples, a Play 3 + cats sample app, **and your own projects**. Track false positives per 10k lines and the latency of each inspection. Nothing ships as Warning or Error until its corpus false-positive rate is ~0. Style rules start as weak warnings.
- **CI:** GitHub Actions runs `sbt test` and the Plugin Verifier against 262 and 263.

---

## 5. Roadmap

Each phase ends with an **exit criterion**, not just a feature list.

### Phase 0 — Spikes & scaffold (≈1 week)
1. Scaffold the sbt-idea-plugin build for `262.9437` + Scala plugin `2026.2.18`, with Scala 3.7.4 as the plugin language. `sbt runIDE` opens a sandbox IDE.
2. First inspection, end to end with tests: `fa.map(_ => ())` → `fa.void`.
3. **Completion spike:** inject a `traverse` lookup on `List[A]` in a 2.13 file *without* the import, and add `cats.syntax.all._` on insert. This confirms the lookup-element and `ScImportsHolder` APIs.
4. **Instance-oracle spike:** measure the cost of a synthetic `implicitly[Traverse[X]]` against the static table.
- **Exit:** all four green, and we know the latency numbers.

### Phase 1 — MVP "Cats Lens" → v0.1 on the Marketplace (≈4–6 weeks)
- Catalog v1 (~150 entries), library detection, import-style detection, settings page.
- A1, A2, A3, A6, A7 · B1 (discarded effect), B9 (import conflicts) · ~15 C simplifications.
- **Exit:** you use it daily on your own Play 3 + cats project for a week without turning anything off.

### Phase 2 — Effect safety + visual layer (v0.2–0.3)
- B2–B8, B10 · the rest of C · D1 (for-comprehension lens), D2 (aliases), D4 (gutters) · F1 (stepping filters + renderers).
- **Exit:** corpus false positives ≈ 0 for every rule at Warning or Error.

### Phase 3 — Play 3 wiring, Spring parity (v0.4)
- E1 config keys, E2 Guice navigation + unbound-injection inspection, E3 messages, E4 bridge, D7 endpoints.
- **Exit:** in your Play app, every `config.get`, every `@Inject` parameter and every `messages(...)` call can be navigated and is checked.

### Phase 4 — The wow (v0.5+)
- B11 implicit-not-found explainer · D5 Typeclass Explorer · D6 Combinator Finder + "fill hole" · A4 smart completion · A5 companions.
- Library packs: fs2, http4s endpoints, doobie (auto SQL injection into `sql"…"` / `fr"…"`, `Read` / `Write` hints), circe.

### Phase 5 — R&D
- G1 compiler-backed completion · F3 fiber dumps · F2 weaver runner if still needed.

## 6. Open-source plan

- **License:** Apache-2.0, the same as cats, intellij-scala and zio-intellij.
- **Contributing:** "Add a catalog entry in 5 lines" and "Add a simplification in 10 lines" guides. These are the community's entry points.
- **Community:** share early builds in the Typelevel Discord (#cats, #cats-effect). Maybe propose a home under the typelevel org later. Ask the zio-intellij maintainers for tips. Talk to the JetBrains Scala team (YouTrack / Discord) when we hit API gaps.
- **Marketplace:** publish with `sbt-idea-plugin`'s `publishPlugin` (token in CI secrets), with plugin signing. Ship pre-release builds to an EAP channel.

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| The Scala plugin API is semi-internal and changes between releases | Pin the Scala plugin version per IDE branch, support only the latest 2 IDE majors, run the Plugin Verifier in CI, keep a thin `compat/` layer |
| The Scala plugin's type inference is wrong, so our features are wrong too | Static instance tables for common types. Inspections fire **only on fully resolved types**, never on error types. G1 as the long-term fix |
| Completion latency | Static tables, caches, < 30 ms budget, cancellation checks, perf tests in CI |
| False positives kill trust | Corpus testing; conservative default severities; per-rule toggles; "suppress for statement" everywhere |
| better-monadic-for / kind-projector / `-Xsource:3` change semantics | Detect them from compiler options in the sbt import and adapt (B1, B10, D1) |
| Scope creep: this plan is big | Phases with exit criteria. Phase 1 alone already removes the biggest daily pain |

## 8. What the MVP feels like

```scala
import cats.effect.IO
// no cats.syntax import yet

def loadAll(ids: List[UserId]): IO[List[User]] =
  ids.trav|                       // ← Pounce offers: traverse  (Traverse[List], Applicative[IO])
                                  //   Enter → ids.traverse(|) and `import cats.syntax.all._` is added

def save(u: User): IO[Unit] = {
  log.info(s"saving $u")          // ← ERROR B1: IO[Unit] is discarded and will never run
                                  //   Alt+Enter → `log.info(...) *> repo.insert(u)`
  repo.insert(u).map(_ => ())     // ← C: replace with `.void`
}
```

## 9. Week-1 checklist

- [ ] Pick the name; check Marketplace + GitHub + Scaladex for collisions
- [ ] `git init`, Apache-2.0 LICENSE, README with the vision (section 3 above)
- [ ] sbt-idea-plugin skeleton for 262 + Scala plugin 2026.2.18; `sbt runIDE` works
- [ ] Vendor the Scala plugin testkit (copy zio-intellij's approach, credit it in ORIGIN.md)
- [ ] First simplification (`.map(_ => ())` → `.void`) with fixture tests
- [ ] Completion spike (Phase 0, step 3) + latency notes
- [ ] Create a small Play 3 + cats-effect sample app in `samples/` as the daily test bed
