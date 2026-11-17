package domain

import cats.effect.Sync
import cats.syntax.all._

/**
 * Try here: type `ids.` (traverse, foldMapM, ...) or `repo.find(id).` (flatTap, attempt, ...) — Pounce offers cats
 * syntax that fits the receiver and the `F[_]: Sync` bound, and adds the import if it is missing.
 */
final class UserService[F[_]: Sync](repo: UserRepo[F]) {

  def find(id: Long): F[Option[User]] = repo.find(id)

  def list(limit: Int): F[List[User]] = repo.list(limit)

  def rename(id: Long, name: String): F[Option[User]] =
    for {
      maybe   <- repo.find(id)
      updated  = maybe.map(_.copy(name = name))
      _       <- updated.traverse_(repo.save)
    } yield updated

  def findAll(ids: List[Long]): F[List[Option[User]]] = ids.traverse(repo.find)
}
