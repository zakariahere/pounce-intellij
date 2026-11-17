package domain

import cats.effect.IO
import munit.CatsEffectSuite

class UserServiceSpec extends CatsEffectSuite {

  test("rename updates an existing user") {
    for {
      repo <- InMemoryUserRepo.make[IO](Seed.users)
      svc   = new UserService[IO](repo)
      res  <- svc.rename(1L, "Ada L.")
    } yield assertEquals(res.map(_.name), Some("Ada L."))
  }

  test("rename of a missing user is a no-op") {
    InMemoryUserRepo.make[IO](Seed.users).flatMap(repo => new UserService[IO](repo).rename(99L, "x")).map(r => assertEquals(r, None))
  }
}
