package domain

import play.api.libs.json.{Json, OFormat}

final case class User(id: Long, name: String, email: String)

object User {
  implicit val format: OFormat[User] = Json.format[User]
}

object Seed {
  val users: List[User] = List(User(1L, "Ada", "ada@example.com"), User(2L, "Alan", "alan@example.com"))
}
