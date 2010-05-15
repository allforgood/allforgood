package org.allforgood
package geocode

import net.liftweb.util._
import net.liftweb.common._
import Helpers._
import net.liftweb.json._
import JsonParser._
import JsonAST._
import JsonDSL._
import net.liftweb.http.testing._

import model.GeoLocation

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: Apr 20, 2010
 * Time: 10:10:55 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * @param in String Takes the String and returns an Option[GeoLocation]
 */
object Geocoder {
  private var cache: Map[String, Box[GeoLocation]] = Map()

  import scala.io.Source
  import java.io._

  private implicit def formats = DefaultFormats
  
  for {
    source <- tryo(Source.fromFile(new File("geocache.txt")))
    line <- source.getLines()
    (key :: jval :: _) =  line.roboSplit("=").map(_.trim)
    decoded = urlDecode(jval)
  } {
    if (decoded == "Empty") cache += urlDecode(key) -> Empty
    else {
      for {
        json <- tryo(parse(decoded))
        value <- tryo(json.extract[GeoLocation])
      } {
        cache += (urlDecode(key) -> Full(value))
      }
    }
  }
  


  def apply(in: String): Box[GeoLocation] = {
    val all = 
    synchronized{
      val key = md5(in)
      cache.get(key) openOr {
        val encoder = new Geocoder
        val ret = encoder.getGeoLocation(in)
        cache += key -> ret

        import net.liftweb.json.Serialization.{read, write}
        implicit def formats = Serialization.formats(NoTypeHints)

        val fr = new PrintWriter(new FileOutputStream(new File("geocache.txt"), true))
        fr.println(urlEncode(key)+"="+(ret match {
          case Full(geo) => urlEncode(write(geo))

          case _ => "Empty"
        }))
        fr.close()

        ret
      }
    }

    if (in.toLowerCase == "ca") println("Hey... loc for CA is "+all)

    all
  }
}

class Geocoder extends RequestKit {
  def baseUrl = "http://maps.google.com"

  protected def getString(url: String, params: (String, Any)*): Box[String] =
    for {
      resp <- get(url, params :_*).filter(_.code == 200) ?~ "Didn't get a 200"
      answer <- tryo(new String(resp.body, "UTF-8"))
    } yield answer

  private def getGeoLocation(in: String): Box[GeoLocation] = {
    val encodedString = urlEncode(in.trim)

    // val url = GOOGLE_GEO_URL + "address=" + encodedString + "&sensor=false"

    for {
      response <- getString("/maps/api/geocode/json", "address" -> in, "sensor" -> false)
      loc <- parseResponse(response)
    } yield new GeoLocation(loc.lng, loc.lat, true)
  }

  private def parseResponse(in: String): Box[GoogGeoLoc] = {
    implicit val formats = DefaultFormats

    for {
      json <- tryo(parse(in))
      geoRet <- tryo(json.extract[GoogGeoRet]).filter(_.status == "OK")
      first <- geoRet.results.headOption
    } yield first.geometry.location
  }
}

case class GoogGeoRet(status: String, results: List[GoogGeoResults])
case class GoogGeoResults(
        types: List[String],
        formatted_address: String,
        address_components: List[GoogGeoAddrComp],
        geometry: GoogGeoGeom
        )
case class GoogGeoAddrComp(
        long_name: String,
        short_name: String,
        types: List[String]
        )
case class GoogGeoGeom(
        location: GoogGeoLoc,
        location_type: String,
        viewport: GoogGeoViewport,
        bounds: Option[GoogGeoViewport]
        )
case class GoogGeoLoc(
        lat: Double,
        lng: Double
        )
case class GoogGeoViewport(southwest: GoogGeoLoc, northeast: GoogGeoLoc)

