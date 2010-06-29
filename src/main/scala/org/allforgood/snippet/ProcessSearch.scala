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
  val NumResultsPerPage = 10
  
  def intParam(name: String): Box[Int] =
    S.param(name).flatMap(Helpers.asInt).filter(_ > 0)

  def render(in: NodeSeq): NodeSeq =
    for {
      q <- S.param("q").map(_.trim).filter(_.length > 0) ?~ "No query"
    } yield {
      val loc: Option[GeoLocation] =
        for {
          l <- S.param("loc").map(_.trim).filter(_.length > 0)
          geo <- Geocoder(l)
        } yield geo

      val start = intParam("start") openOr 0

      val num = (intParam("num") openOr 10) + 1

      val store = PersistenceFactory.store.vend
      val results =
        store.read(store.search(Full(q), loc = loc,
                              start = start, num = num))

      results match {
        case Nil => Text("No opportunities matched your search")
        case xs => {
          val resNumFmtStr = "%1$" + xs.length.toString().length + "d"
          (xs take (num - 1)).zipWithIndex.
                  flatMap{ 
                    case ((guid, volopp, rank), pos) =>
                      bind("volop",
                           chooseTemplate("results",
                                          "resultdiv",
                                          in),
                           bindParams(volopp, pos + start, resNumFmtStr): _*)
                  } ++
          bind("volop",
               chooseTemplate("results",
                              "resultdivlast",
                              in),
               bindParams(xs.last._2, lastIndex + 1, resNumFmtStr): _*) ++                   
          bind("resultnav",
               chooseTemplate("results",
                              "resultnav",
                              in),
               "prevresults" -> (if ( firstIndex != 0 ) <a href={"/search?q=" + q + "&loc=" + loc.getOrElse("") + "&index=" + (firstIndex - NumResultsPerPage)}>&lt;&lt; Prev</a> else NodeSeq.Empty),
               "nextresults" -> (if ( firstIndex + NumResultsPerPage < results.length ) <a href={"/search?q=" + q + "&loc=" + loc.getOrElse("") + "&index=" + (firstIndex + NumResultsPerPage)}>Next &gt;&gt;</a> else NodeSeq.Empty))
          
        } : NodeSeq
      }
    }

  def bindParams(vo: VolunteerOpportunity, resNum: Int, resNumFmtStr: String, q: String, loc: String, firstIndex: Int, resultsLength: Int): Array[BindParam] =  
    bindParams(vo, resNum, resNumFmtStr) ++
      Array[BindParam]("prevresults" -> (if ( firstIndex != 0 ) <a href={"/search?q=" + q + "&loc=" + loc + "&index=" + (firstIndex - NumResultsPerPage)}>&lt;&lt;Prev</a> else NodeSeq.Empty),
                       "nextresults" -> (if ( firstIndex + NumResultsPerPage < resultsLength ) <a href={"/search?q=" + q + "&loc=" + loc + "&index=" + (firstIndex + NumResultsPerPage)}>Next&gt;&gt;</a> else NodeSeq.Empty))
  
  def bindParams(vo: VolunteerOpportunity, resNum: Int, resNumFmtStr: String) = Array[BindParam](
          "label" -> Text(format(resNumFmtStr, resNum)),
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
          "dataprovider" -> (if ( vo.source eq None ) NodeSeq.Empty else Text(vo.source.get.providerName)),
          "categoryname" -> Text(vo.categoryTags.mkString(", ")) )

  implicit def bnsToNS(in: Box[NodeSeq]): NodeSeq = in match {
    case Full(i) => i
    case Failure(msg, _, _) => S.error(msg); 
      S.redirectTo(S.referer openOr "/")
    case _ =>
      S.redirectTo(S.referer openOr "/")
  }

  def setSearchLocFromCookie =
    Script(JsRaw("""
      (function() {
        searchLocCookieName = 'searchlocation';
        allCookies = document.cookie;
        var pos = allCookies.indexOf(searchLocCookieName + '=');
        if ( pos != -1 ) {
          var start = pos + searchLocCookieName.length + 1;
          var end = allCookies.indexOf( ';', start);
          if ( end == -1 ) end = allCookies.length;
          searchLoc = allCookies.substring(start,end);
        }
        if ( ! searchLoc )
          searchLoc = BrowserLocation;
        if ( document.getElementById('loc') )
          document.getElementById('loc').value = searchLoc;
        var elem = document.getElementById('loc');
        elem.onchange =
          'document.cookie.replace(searchLocCookieName + '=' + '.*',
                                   searchLocCookieName + '=' + elem.value)';
      }})()
    """))
}
