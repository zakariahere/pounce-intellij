package domain

import cats.effect.{Ref, Sync}
import cats.syntax.all._

/** A tagless algebra: Pounce draws wiring gutters between it, its implementations and where they are built. */
trait UserRepo[F[_]] {
  def find(id: Long): F[Option[User]]
  def list(limit: Int): F[List[User]]
  def save(user: User): F[Unit]
}

final class InMemoryUserRepo[F[_]: Sync] private (ref: Ref[F, Map[Long, User]]) extends UserRepo[F] {
  def find(id: Long): F[Option[User]] = ref.get.map(_.get(id))
  def list(limit: Int): F[List[User]] = ref.get.map(_.values.toList.sortBy(_.id).take(limit))
  def save(user: User): F[Unit]       = ref.update(_ + (user.id -> user))
}

object InMemoryUserRepo {
  def make[F[_]: Sync](seed: List[User]): F[UserRepo[F]] =
    Ref.of[F, Map[Long, User]](seed.map(u => u.id -> u).toMap).map(ref => new InMemoryUserRepo[F](ref))
}
