package org.allforgood
package snippet

import scala.xml.{NodeSeq, Text, Node}
import net.liftweb.util._
import net.liftweb.http._
import js._
import JsCmds._
import JE._
import net.liftweb.common._
import org.allforgood.lib._
import Helpers._
import net.liftweb.json._
import JsonAST._
import net.liftweb.http.testing._

import model._
import geocode._
import java.text.SimpleDateFormat
import java.util.Date

object PopulateFind {
  def render(in: NodeSeq): NodeSeq = {
    val q = S.param("q")
    val loc = S.param("loc")
    bind("populate", in,
         AttrBindParam("class", if (q.isEmpty) "faded" else "", "class"),
         AttrBindParam("ivalue", q getOrElse "", "value"),
         AttrBindParam("lvalue", loc getOrElse "", "value"))
  }
}

