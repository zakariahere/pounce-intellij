package modules

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.google.inject.{AbstractModule, Provides, Singleton}
import domain.{InMemoryUserRepo, Seed, UserRepo, UserService}

/** Enabled from application.conf (`play.modules.enabled`). Pounce links each @Provides to its injection points. */
class AppModule extends AbstractModule {

  @Provides @Singleton
  def ioRuntime: IORuntime = IORuntime.global

  @Provides @Singleton
  def userRepo(runtime: IORuntime): UserRepo[IO] =
    InMemoryUserRepo.make[IO](Seed.users).unsafeRunSync()(runtime)

  @Provides @Singleton
  def userService(repo: UserRepo[IO]): UserService[IO] = new UserService[IO](repo)
}
