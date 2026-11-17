package playground

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Future

/** Every method here trips one Pounce inspection ON PURPOSE. Open the file and look at the highlights. */
object Smells {

  def discarded: IO[Unit] = { IO.println("never printed"); IO.unit } // discarded effect

  def nested(id: Long): IO[IO[Int]] = IO.pure(id).map(i => IO.pure(i.toInt)) // map that should be flatMap

  def blocking: IO[Unit] = IO(Thread.sleep(100)) // blocking call inside IO.apply/delay

  def edge(): Int = IO.pure(1).unsafeRunSync() // unsafeRun outside the edge of the world

  val running: Future[Int] = Future(42)(ec)
  def eager: IO[Int]       = IO.fromFuture(IO.pure(running)) // the Future already started

  def leak: IO[Unit] = IO.never[Unit].start.void // fiber started and forgotten

  def escaped(r: Resource[IO, String]): IO[String] = r.use(IO.pure) // value escapes `use` after release

  def simplify(io: IO[Int]): IO[Unit] = io.map(_ => ()) // simplification: .void
}
