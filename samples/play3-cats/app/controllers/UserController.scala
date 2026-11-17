package controllers

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import domain.UserService
import play.api.Configuration
import play.api.i18n.{I18nSupport, Messages}
import play.api.libs.json.Json
import play.api.mvc._

import javax.inject.{Inject, Singleton}

/**
 * Play 3 + cats-effect: `unsafeToFuture()` inside `Action.async` is the intended bridge, so Pounce does not flag it.
 *
 * i18n uses `Messages("key", args*)`. Under `I18nSupport`, `request.messages` takes an implicit `MessagesApi`, so
 * `request.messages("key", id)` would pass the key as that argument and not compile.
 */
@Singleton
class UserController @Inject() (cc: ControllerComponents, users: UserService[IO], config: Configuration)(implicit
    runtime: IORuntime
) extends AbstractController(cc)
    with I18nSupport {

  private val pageSize: Int = config.get[Int]("pounce.sample.page-size")

  def list: Action[AnyContent] = Action.async {
    users.list(pageSize).map(us => Ok(Json.toJson(us))).unsafeToFuture()
  }

  def show(id: Long): Action[AnyContent] = Action.async { implicit request =>
    users
      .find(id)
      .map {
        case Some(user) => Ok(Json.toJson(user))
        case None       => NotFound(Messages("users.notFound", id))
      }
      .unsafeToFuture()
  }

  def rename(id: Long, name: String): Action[AnyContent] = Action.async { implicit request =>
    users
      .rename(id, name)
      .map {
        case Some(_) => Ok(Messages("users.renamed", id))
        case None    => NotFound(Messages("users.notFound", id))
      }
      .unsafeToFuture()
  }
}
