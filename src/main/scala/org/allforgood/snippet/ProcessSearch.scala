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

object ProcessSearch {
  val NumResultsPerPage = 10
  
  def intParam(name: String): Box[Int] =
    S.param(name).flatMap(Helpers.asInt).filter(_ > 0)

  def render(in: NodeSeq): NodeSeq =
    for {
      req <- S.request
      q <- S.param("q").map(_.trim).filter(_.length > 0) ?~ "No query"
    } yield {
      val loc: Option[GeoLocation] =
        for {
          l <- S.param("loc").map(_.trim).filter(_.length > 0)
          geo <- Geocoder(l)
        } yield geo

      val start = intParam("start") openOr 0

      val num = (intParam("num") openOr 10)

      val store = PersistenceFactory.store.vend
      val results =
        store.read(store.search(Full(q), loc = loc,
                              start = start, num = num + 1))

      def calcLink(start: Int): String = 
        req.uri +
        "?start="+start+
        req.params.toList.filter(_._1 != "start").flatMap{
          case (key, values) => values.map(v => urlEncode(key)+"="+urlEncode(v))
        }.mkString("&", "&", "")
        

      results match {
        case Nil => Text("No opportunities matched your search")
        case xs => {
          
          def dateFormat(date: Long): String = 
            (new SimpleDateFormat("MMM d").format(new Date(date)))

          def doPrev(in: NodeSeq): NodeSeq =
            if (start == 0) NodeSeq.Empty
          else <a href={calcLink(math.max(0, (start - 10)))}>{in}</a>

          def doNext(in: NodeSeq): NodeSeq = 
            if (num > xs.length) NodeSeq.Empty
          else <a href={calcLink(start + 10)}>{in}</a>

          def doItems(in: NodeSeq): NodeSeq = {
            import scala.collection.mutable.ListBuffer
            val ret: ListBuffer[Node] = new ListBuffer()

            def bindOne(items: List[(GUID, VolunteerOpportunity, Double)], 
                        pos: Int) {
              def doBind(op: VolunteerOpportunity,
                       last: Boolean = false) = {
                val bound = 
                  bind("volop", in,
                       AttrBindParam("class", "searchResults" +
                                     (if (last) " last" else ""),
                                     "class"),
                       "label" -> (('A'.toInt + pos).toChar).toString,
                       ("urlandtitleanchor" ->
                        <a href={op.detailURL.
                                 getOrElse("javascript:void(0)")}
                        >{op.title}</a>),
                       OptionBindParam("location",
                                       for {
                                         loc <- op.locations.headOption
                                         city <- loc.city
                                       } yield 
                                         Text(city + 
                                              " " + 
                                              (loc.postalCode.getOrElse("")))),
                       OptionBindParam("startenddates",
                                       for {
                                         dur <- 
                                         op.dateTimeDurations.headOption
                                         start <- dur.startDate
                                         end <- dur.endDate
                                       } yield
                                         Text(dateFormat(start) +
                                              " - "+
                                              dateFormat(end))),
                       OptionBindParam("what",
                                       op.description.
                                       map(str => Text(
                                         if (str.length > 319)
                                           str.substring(0,319) + "..."
                                         else str))),
                       OptionBindParam("volorgname",
                                       for {
                                         org <-
                                         op.organizations.headOption
                                       } yield Text(org.name)),
                       OptionBindParam("dataprovider",
                                       op.source.map(v => 
                                         Text(v.providerName))),
                       "categoryname" -> op.categoryTags.mkString(", ")
                     )
                ret ++= bound
              }

              
              items match {
                case Nil =>
                case (guid, op, rank) :: Nil =>
                  doBind(op, true)
                case (guid, op, rank) :: xs => 
                  doBind(op)
                  bindOne(xs, pos + 1)
              }
            }

            bindOne(xs, 0)

            ret.toList
          }


          bind("results", in,
               "resultsdiv" -> doItems _,
               "prev" -> doPrev _,
               "next" -> doNext _)
        } : NodeSeq
      }
    }

  private implicit def bnsToNS(in: Box[NodeSeq]): NodeSeq = in match {
    case Full(i) => i
    case Failure(msg, _, _) => S.error(msg); 
      S.redirectTo(S.referer openOr "/")
    case _ =>
      S.redirectTo(S.referer openOr "/")
  }
}
