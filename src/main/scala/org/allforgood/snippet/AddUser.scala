package org.allforgood
package snippet

import scala.xml._
import net.liftweb._
import http._
import js._
import JsCmds._
import mapper._
import util._
import Helpers._

import model._

object AddUser extends LiftScreen {
  object user extends ScreenVar(User.create.verified(true))

  _register(() => user.firstName)
  _register(() => user.lastName)
  _register(() => user.email)
  _register(() => user.superUser)

  def finish() {
    user.password(randomString(20))
    Mailer.sendMail(Mailer.From("admin@allforgood.org"),
                    Mailer.Subject("Your new All for Good management credentials"),
                    Mailer.To(user.email),
                    Mailer.PlainMailBodyType(
"""We've created an administrative account for you on All for Good.

Please click "+S.hostAndPort+"/user_mgt/lost_password to set a password
"""))
    user.is.save
  }
}
