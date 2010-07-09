package org.allforgood
package api

import scala.xml.Elem
import scala.xml.NodeSeq
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.util._
import org.allforgood.lib.Serializers.anyToRss
import model._
import geocode._

import org.joda.time._
import org.allforgood.lib.AfgDate
import org.allforgood.lib.AfgDate._

sealed trait OutputType
final case object JsonOutputType extends OutputType
final case object RssOutputType extends OutputType
final case object HtmlOutputType extends OutputType

object Api {
  def volopps(r: Req): LiftResponse = {
    try {
      for{
        key <- r.param("key") ?~ missingKey ~> 401
        valKey <- validateKey(key) ?~ ("Invalid key. " + missingKey) ~> 401
      } yield {
        val loc: Option[GeoLocation] = 
          for {
            l <- r.param("vol_loc").map(_.trim).filter(_.length > 0)
            geo <- Geocoder(l)
          } yield geo

        val start = r.param("start").flatMap(Helpers.asInt).filter(_ > 0) openOr 1
        val num = r.param("num").flatMap(Helpers.asInt).filter(_ > 0) openOr 10

        val radius = r.param("vol_dist").flatMap(Helpers.asInt).filter(_ > 0).
        openOr(50)

        val timeperiod = r.param("timeperiod").map(_.trim.toLowerCase)
        val startDate = r.param("vol_startdate").map(_.trim).flatMap(AfgDate.parse)
        val endDate = r.param("vol_enddate").map(_.trim).flatMap(AfgDate.parse)
        val timespan: (DateTime, DateTime) = (timeperiod, startDate, endDate) match {
          case (Full("today"), _, _) => afgnow -> afgnow.plusDays(1)
          case (Full("this_week"), _, _) => afgnow -> afgnow.plusDays(7)
          case (Full("this_weekend"), _, _) => {
            val friday = incTilFriday(afgnow)
            friday -> friday.plusDays(3)
          }
          case (Full("this_month"), _, _) => afgnow -> afgnow.plusDays(30)
          // TODO: detect unknown (but present) timeperiods and error
          // case (Full(suppliedTimePeriod), _, _) => errorOut
          case (_, Full(s), Full(e)) => s -> e
          case _ => afgnow.plusDays(1) -> afgnow.plusDays(1000)
        }

        val store = PersistenceFactory.store.vend
        val res = store.read(store.search(query = r.param("q").map(_.trim).
                                          filter(_.length > 0),
                                          loc = loc,
                                          timeperiod = Some(timespan),
                                          start = start - 1,
                                          num = num,
                                          radius = radius
                                        ))


        r.param("output") match {
          case Full("json") =>
            JsonResponse(Extraction.decompose(RetV1("Sat, 01 May 2010 16:51:10 +0000",
                                                    1.0,
                                                    "English",
                                                    "http://dpp.im",
                                                    "All for Good search results",
                                                    res.flatMap(a => MapToV1(a._2)).toList)))

          case Full("rss") =>
            val v1s: List[VolOppV1] = res.flatMap(a => MapToV1(a._2))
            val itemsXML: List[Elem] = v1s.map(a => a.toXML)
            XmlResponse(<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:fp="http://www.allforgood.org/" xmlns:georss="http://www.georss.org/georss" xmlns:gml="http://www.opengis.net/gml"><channel><title>All for Good search results</title><link>http://www.allforgood.org/</link><atom:link href="http://www.allforgood.org/api/volopps?vol_loc=San+Francisco,CA&amp;output=rss&amp;key=LiftWeb" rel="self" type="application/rss+xml"/><description>All for Good search results</description><language>en-us</language><pubDate/><lastBuildDate>Tue, 15 Jun 2010 18:46:55 +0000</lastBuildDate>
              {itemsXML}</channel></rss> , "application/rss+xml")
          
          case _ => XmlResponse(sampleHtml)
        }
      }
    } catch {
      case e => e.printStackTrace ; throw e
    }
  }


  private implicit def respToBox(in: Box[LiftResponse]): LiftResponse = {
    def build(msg: String, code: Int) = {
      InMemoryResponse(msg.getBytes("UTF-8"), List("Content-Type" -> "text/plain"), Nil, code)
    }

    in match {
      case Full(r) => r
      case ParamFailure(msg, _, _, code: Int) => build(msg, code)
      case Failure(msg, _, _) => build(msg, 404)
      case _ => build("Not Found", 404)
    }
  }

  def validateKey(key: String): Box[String] = 
    Full(key).filter(PartnerKey.valid_?)

  val missingKey =
    """You seem to be missing the API key parameter ('key') in your query.
  Please see the for how to get an API key at
  http://www.allforgood.org/docs/api.html for directions."""

  case class Sample(categories: List[String], quality_score: Double, pageviews: Int)
  val sample = Sample(List("category1", "category2"), .5, 2312)

  implicit val formats = Serialization.formats(NoTypeHints)
  val sampleJson = Extraction.decompose(sample) // Serialization.write(sample) 

  val sampleHtml =
    <p>here's some info on volunteer opportunities</p>

  val sampleRss = <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:fp="http://www.allforgood.org/" xmlns:georss="http://www.georss.org/georss" xmlns:gml="http://www.opengis.net/gml">
  {anyToRss(sample)}
  </rss>
}

/*
object KeyManager {
  @volatile private var validKeys: Set[String] = Set()

  private var lastTestTime = 0L
  private var lastFileTime = 0L

  private def insureKeysUpToDate() {
    synchronized {
      import java.io.File
      import Helpers._
      import scala.io.Source

      // we only care about checking every 5 seconds
      if (millis - lastTestTime > 5000L) {
        val f = new File("key_whitelist.txt")
        tryo {
          val changeTime = f.lastModified
          if (lastFileTime != changeTime) {
            validKeys = Set(Source.fromFile(f).getLines().map(_.trim).toList :_*)
            lastFileTime = changeTime
          }
        } 
        
        lastTestTime = millis
      }
    }
  }


  def validateKey(key: String): Box[String] = {
    insureKeysUpToDate()
    Full(key).filter(validKeys.contains) ?~ "Key not found"
  }
}
*/
