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
  import scala.xml.NodeSeq
  import org.apache.log4j.Category
  /*
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
  */

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
        case Nil => Text("No opportunities matched your search")
        case xs => {
          var volOpNum = 1
          val volNumFmtStr = "%" + xs.length.toString().length + "d"
          xs.init.zipWithIndex.flatMap{ x:((GUID, VolunteerOpportunity, Double),Int) => bind("volop",
                                                chooseTemplate("results",
                                                               "resultdiv",
                                                               in),
                                                bindParams(x._1._2, x._2 + 1, volNumFmtStr): _*) } ++
              bind("volop",
                   chooseTemplate("results",
                                  "resultdivlast",
                                  in),
                   bindParams(xs.last._2, xs.length, volNumFmtStr): _*)
        } : NodeSeq
      }
    }  

  def bindParams(vo: VolunteerOpportunity, volNum: Int, volNumFmtStr: String) = Array[BindParam] (
          "label" -> Text(volNumFmtStr.format(volNum)),
          "urlandtitleanchor" ->
            <a href={vo.detailURL.getOrElse("javascript:void(0)")}>{ vo.title }</a>,
          "location" ->
            (if ( vo.locations.isEmpty ) NodeSeq.Empty else Text(vo.locations.head.city.getOrElse("") + " " + vo.locations.head.postalCode.getOrElse("") + " -" )),
          "startenddates" -> {
            import java.util.Date
            import java.text.SimpleDateFormat
            val df = new SimpleDateFormat("MMM d")
            val startDate = vo.dateTimeDurations.head.startDate.getOrElse(new Date)
            val endDate = vo.dateTimeDurations.head.startDate.getOrElse(new Date)
            Text(" " + df.format(startDate) + " - " + df.format(endDate))
          },
          "what" ->
            Text( { val desc = vo.description.getOrElse(""); if (desc.length > 320) desc.substring(0, 319) + "..." else desc } ),
          "volorgname" -> (if ( vo.organizations.isEmpty ) NodeSeq.Empty else Text(vo.organizations.head.name)),
          "dataprovider" -> NodeSeq.Empty,
          "categoryname" -> Text(vo.categoryTags.mkString(", ")) )


  implicit def bnsToNS(in: Box[NodeSeq]): NodeSeq = in match {
    case Full(i) => i
    case Failure(msg, _, _) => S.error(msg); 
      S.redirectTo(S.referer openOr "/")
    case _ =>
      S.redirectTo(S.referer openOr "/")
  }

}
