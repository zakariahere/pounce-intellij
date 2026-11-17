# Pounce sample: Play 3 + cats-effect

A small Scala 2.13.18 / Play 3.0 / cats-effect 3 app: a tagless `UserRepo[F]`, a `UserService[F]`, a Guice module
and a controller that bridges `IO` to Play with `unsafeToFuture()`. It compiles cleanly. `app/playground/Smells.scala`
is the one file that is wrong **on purpose**.

```bash
sbt run        # http://localhost:9000/users
sbt test
```

Open this folder in an IDE that has Pounce installed (from the plugin repo, `sbt runIDE` starts a sandbox IDE with it).
Then work through the checklist below.

## Completion (Pillar A)

In `app/domain/UserService.scala`:

- [ ] In `findAll`, delete `traverse(repo.find)` and type `ids.` → `traverse`, `foldMapM`, `traverseFilter`… are
      offered, each with ` Traverse[List] · cats` as tail text. Remove `import cats.syntax.all._` and pick one again:
      the import comes back.
- [ ] Type `repo.find(id).` → `flatTap`, `void`, `as`… are offered because `F[_]: Sync` is in scope. `attempt` comes
      through `Sync`'s `MonadThrow`.
- [ ] Type `(repo.find(1), repo.find(2)).` → `mapN`, `tupled`, `parMapN`.
- [ ] In a method returning `F[Int]`, press **Ctrl+Shift+Space** → `Sync[F].delay(…)`, `Sync[F].pure(…)`.
- [ ] Type `42.` → `.pure[F]`, `.some`, `.asRight[…]`, ranked by the expected type.
- [ ] Postfix templates: type `println("hi").delay` and press Tab → `Sync[F].delay(println("hi"))`. Also try
      `.blocking`, `.attempt`, `.start` and `.void` on an effect.
- [ ] Press **Ctrl+Q** on any suggestion: signature, constraints, example, and why it applies here.

## Inspections (Pillars B + C)

Open `app/playground/Smells.scala`. Every method should be highlighted:

| Method | Expect |
|---|---|
| `discarded` | Discarded effect: `IO.println` never runs |
| `nested` | `IO[IO[Int]]`: quick fix *replace `map` with `flatMap`* |
| `blocking` | `Thread.sleep` inside `IO(...)`: quick fix `IO.blocking` |
| `edge` | `unsafeRunSync()` outside the edge of the world |
| `eager` | `IO.fromFuture(IO.pure(running))`: the Future already started |
| `leak` | `.start.void`: fiber started and forgotten |
| `escaped` | `r.use(IO.pure)`: the value escapes after release |
| `simplify` | `io.map(_ => ())`: quick fix `io.void` |

These must **not** be flagged. They are legitimate edges:

- [ ] `unsafeToFuture()` in `app/controllers/UserController.scala`. Inside `Action.async` it's the intended Play bridge.
- [ ] `unsafeRunSync()` in `app/modules/AppModule.scala`'s `userRepo`. Guice modules run once at startup.
- [ ] Copy `def edge()` from `Smells` into `UserService` → it *is* flagged there.

Tagless bounds, in `UserService.scala`:

- [ ] Change `F[_]: Sync` to `F[_]: Functor`. The `for` in `rename` stops resolving, and the quick fix
      *Strengthen bound to Monad* appears.
- [ ] Change it to `F[_]: Monad: Sync` → *Redundant bound `Monad`* (implied by `Sync`).

## Visual (Pillar D)

- [ ] `UserService.rename`: each `<-` has an inlay `F · Option[User]`, and the `for` keyword shows `F: Sync`.
- [ ] `app/domain/UserRepo.scala`: the gutter on `trait UserRepo[F[_]]` lists `InMemoryUserRepo`. From there, jump to
      where it is constructed (`InMemoryUserRepo.make`, `AppModule.userRepo`).
- [ ] `Smells.leak`: fork-point gutter on `.start`. `Smells.escaped`: Resource gutter on `use`.
- [ ] **Pounce tool window → Explorer**: put the caret on `ids` in `findAll`. It shows `Traverse`, `TraverseFilter`,
      `Monad`… = Yes, `Async` = No, and what you can call.
- [ ] **Combinator Finder** (Ctrl+Alt+Shift+K): have `ids: List[A]; f: A => IO[B]`, want `IO[List[B]]` →
      `ids.traverse(f)` first. Have `xs: List[A]`, want `List[Unit]` → `xs.void`, and nothing that needs an effect
      `List` doesn't have.
- [ ] Write `def all(ids: List[Long]): F[List[Option[User]]] = ???` and use the intention
      *Pounce: fill ??? with a combinator* on `???`.
- [ ] **Endpoints**: the three routes of `conf/routes`, each navigating to its controller method.

## Play (Pillar E)

In `app/controllers/UserController.scala`:

- [ ] Inside `config.get[Int]("pounce.sample.")`, complete → `page-size`, `greeting`. Ctrl+click the key → the line
      in `conf/application.conf`.
- [ ] Change it to `"pounce.sample.page-sise"` → *Unknown config key*, with the fix *Add key to application.conf*.
- [ ] The `users: UserService[IO]` constructor parameter has a Guice gutter → `AppModule.userService` (`@Provides`).
      From `AppModule`, the gutter leads back to the injection point.
- [ ] In `Messages("users.")`, complete → `notFound`, `renamed`, `title`. Ctrl+click → `conf/messages`.
- [ ] `Messages("users.renamed", id)` → *Missing translation* for `fr` (`conf/messages.fr` lacks it on purpose), with a
      stub fix. `Messages("users.typo")` → *Missing message key*.
- [ ] Write `def health = Action { IO.pure(Ok("up")) }` in the controller. It's red, because `Action` wants a
      `Result`. Use the intention *Convert IO action to Action.async with unsafeToFuture* →
      `Action.async { IO.pure(Ok("up")).unsafeToFuture() }`, reusing the controller's implicit `IORuntime`.
- [ ] Live templates: `ioaction`, `ioactionjson`, `guicebind`, `playmodule`.

## Runtime (Pillar F)

- [ ] Put a breakpoint inside the `for` in `UserService.rename`, start `sbt run` in debug mode, and call
      `POST /users/1/name?name=Ada`. **Step Into** skips `cats.*`, `cats.effect.*` and `fs2.*` frames, so stepping
      through the `for` stays in your code.
- [ ] Evaluate a `Ref`, `NonEmptyList` or `Validated` in the debugger: Pounce renderers show the contents.
- [ ] With the app running (`sbt run`, then hit `/users` once), use **Tools → Pounce: Dump Cats Effect Fibers** and pick
      the sbt JVM: live fibers grouped by state, with clickable frames.
- [ ] Live templates in a new file: `ioapp`, `resmake`, `algebra`, `ioref`, `munitce`.

Found something that doesn't match? Please open an issue with the file, the line and what you saw.
