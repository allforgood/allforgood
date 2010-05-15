package org.allforgood
package snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import org.allforgood.lib._
import Helpers._
import net.liftweb.json._
import JsonAST._
import net.liftweb.http.testing._

import model._
import geocode._

object ProcessSearch {
  def render(in: NodeSeq): NodeSeq = 
    for {
      q <- S.param("q").map(_.trim).filter(_.length > 0) ?~ "No query"
    } yield {
      val loc: Option[GeoLocation] = 
        for {
          l <- S.param("loc").map(_.trim).filter(_.length > 0)
          geo <- Geocoder(l)
        } yield geo

      val store = PersistenceFactory.store.vend
      store.read(store.search(Full(q), loc = loc)) match {
        case Nil => Text("No results")
        case xs => xs.flatMap{
          case (guid, vo, _) => bind("results", in,
                                  "title" -> vo.title,
                                  "description" -> 
                                  (vo.description getOrElse "N/A"))
        } : NodeSeq
      }
    }

  implicit def bnsToNS(in: Box[NodeSeq]): NodeSeq = in match {
    case Full(i) => i
    case Failure(msg, _, _) => S.error(msg); 
      S.redirectTo(S.referer openOr "/")
    case _ =>
      S.redirectTo(S.referer openOr "/")
  }

}
