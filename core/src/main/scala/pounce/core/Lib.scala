package pounce.core

/**
 * Libraries Pounce knows about. Detection is class-based (see [[Libraries]]), which works no matter how the build
 * tool names its library entries (sbt, BSP, Bazel, Maven, Gradle) and in light test fixtures.
 */
enum Lib(val displayName: String, val markerClasses: List[String]) {
  case CatsCore   extends Lib("cats-core", List("cats.Functor"))
  case CatsEffect extends Lib("cats-effect", List("cats.effect.IO", "cats.effect.kernel.Sync"))
  case Fs2        extends Lib("fs2", List("fs2.Stream"))
  case Http4s     extends Lib("http4s", List("org.http4s.HttpRoutes"))
  case Doobie     extends Lib("doobie", List("doobie.util.fragment.Fragment"))
  case Circe      extends Lib("circe", List("io.circe.Encoder"))
  case Tapir      extends Lib("tapir", List("sttp.tapir.Endpoint"))
  case Play       extends Lib("play", List("play.api.mvc.Action", "play.api.Configuration"))
  /** Play 3.x runs on Pekko; Play 2.x on Akka. */
  case Pekko      extends Lib("pekko", List("org.apache.pekko.actor.ActorSystem"))
  case Guice      extends Lib("guice", List("com.google.inject.Injector"))
  case ScalaGuice extends Lib("scala-guice", List("net.codingwell.scalaguice.ScalaModule"))
  case PlayJson   extends Lib("play-json", List("play.api.libs.json.Json"))
  case Pureconfig extends Lib("pureconfig", List("pureconfig.ConfigSource"))
  case TypesafeConfig extends Lib("config", List("com.typesafe.config.Config"))
  case Weaver     extends Lib("weaver", List("weaver.SimpleIOSuite", "weaver.IOSuite", "weaver.EffectSuite"))
  case MunitCE    extends Lib("munit-cats-effect", List("munit.CatsEffectSuite"))
}
