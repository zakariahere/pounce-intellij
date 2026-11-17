package pounce.play.i18n

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import pounce.core.Libraries
import pounce.core.settings.PounceSettings
import pounce.play.util.PlayPsi
import pounce.play.util.PlayPsi.LiteralCall

/** A string literal used as an i18n message key. `optional`: `isDefinedAt` / `translate` (tolerate missing keys). */
final case class MessageKeyUse(literal: ScStringLiteral, call: LiteralCall, optional: Boolean) {
  def key: String = literal.getValue
}

/**
 * Recognised call patterns: `messagesApi("k")`, `messages("k")`, `Messages("k")`, `request.messages("k")` (a
 * `MessagesRequestHeader`, e.g. in a `MessagesAbstractController`), `messagesApi.preferred(request)("k")`,
 * `isDefinedAt("k")` / `translate("k", ...)` (optional), and Play's Java API `messages.at("k")`,
 * `messagesApi.get(lang, "k")`. Under `I18nSupport`, `request.messages` takes an implicit `MessagesApi`, so
 * `request.messages("k")` there passes "k" as that argument: resolution sees `messages`, not `Messages.apply`.
 */
object MessagesCalls {

  private val ScalaOwners = Set("play.api.i18n.Messages", "play.api.i18n.MessagesApi", "play.api.i18n.MessagesProvider")
  private val JavaOwners  = Set("play.i18n.Messages", "play.i18n.MessagesApi")

  def enabled(e: PsiElement): Boolean = PounceSettings().playMessages && Libraries.hasPlay(e)

  def use(lit: ScStringLiteral): Option[MessageKeyUse] =
    PlayPsi.literalCall(lit).flatMap { c =>
      val name = c.name
      if (PlayPsi.isOwner(c.owner, ScalaOwners)) name match {
        case "apply" if c.argIndex == 0                       => Some(MessageKeyUse(lit, c, optional = false))
        case "isDefinedAt" | "translate" if c.argIndex == 0   => Some(MessageKeyUse(lit, c, optional = true))
        case _                                                => None
      }
      else if (PlayPsi.isOwner(c.owner, JavaOwners)) {
        // play.i18n.Messages.at(key, args...) / play.i18n.MessagesApi.get(lang, key, args...)
        val ownerFqn  = PlayPsi.fqn(c.owner)
        val keyIndex  = if (PlayPsi.isOwner(c.owner, Set("play.i18n.MessagesApi"))) 1 else 0
        name match {
          case "at" | "get" if c.argIndex == keyIndex => Some(MessageKeyUse(lit, c, optional = false))
          case "isDefinedAt" if c.argIndex == keyIndex => Some(MessageKeyUse(lit, c, optional = true))
          case _                                       => None
        }
      } else None
    }
}
