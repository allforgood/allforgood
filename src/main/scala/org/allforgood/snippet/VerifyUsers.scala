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

object VerifyUsers {
  def render(in: NodeSeq): NodeSeq = {
    def redraw() = SetHtml("userinfo", draw)

    def draw: NodeSeq =
      for {
        user <- User.findAll(OrderBy(User.id, Ascending),
                             By(User.verified, false))
        node <- bind("user", in,
                     "name" -> (user.firstName+" "+
                                user.lastName+" "+
                                user.email),
                     "accept" -> SHtml.ajaxButton("Accept",
                                                  () => {
                                                    user.verified(true).save
                                                    redraw()
                                                  }),
                     "reject" -> SHtml.ajaxButton("Accept",
                                                  () => {
                                                    user.verified(true).save
                                                    redraw()
                                                  }))
      } yield node
   
    draw
  }
}
