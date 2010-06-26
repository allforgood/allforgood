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

  val NumResultsPerPage = 10
  def render(in: NodeSeq): NodeSeq =
    for {
      q <- S.param("q").map(_.trim).filter(_.length > 0) ?~ "No query"
    } yield {
      val loc: Option[GeoLocation] =
        for {
          l <- S.param("loc").map(_.trim).filter(_.length > 0)
          geo <- Geocoder(l)
        } yield geo

      val index = (S.param("index") openOr "-1").toInt
      val store = PersistenceFactory.store.vend
      val results =
        store.read(store.search(Full(q), loc = loc))
      val firstIndex = if (index < 0) 0 else index / NumResultsPerPage * NumResultsPerPage      
      val lastIndex =
        if (firstIndex + NumResultsPerPage > results.length ) results.length - 1 else firstIndex + NumResultsPerPage - 1
      results.slice(firstIndex, lastIndex + 1) match {
        case Nil => Text("No opportunities matched your search")
        case xs => {
          val resNumFmtStr = "%1$" + xs.length.toString().length + "d"
          xs.init.zip(List.range(firstIndex + 1, lastIndex + 1))
                  .flatMap{ x:((GUID, VolunteerOpportunity, Double),Int) => bind("volop",
                                                chooseTemplate("results",
                                                               "resultdiv",
                                                               in),
                                                bindParams(x._1._2, x._2, resNumFmtStr): _*) } ++
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
    <script type="text/javascript">
      (function() {{
        searchLocCookieName = 'searchlocation'
        allCookies = document.cookie
        var pos = allCookies.indexOf(searchLocCookieName + '=');
        if ( pos != -1 ) {{
          var start = pos + searchLocCookieName.length + 1
          var end = allCookies.indexOf( ';', start)
          if ( end == -1 ) end = allCookies.length
          searchLoc = allCookies.substring(start,end)
        }}
        if ( ! searchLoc )
          searchLoc = BrowserLocation
        if ( document.getElementById('loc') )
          document.getElementById('loc').value = searchLoc
        var elem = document.getElementById('loc');
        elem.onchange =
          'document.cookie.replace(searchLocCookieName + '=' + '.*',
                                   searchLocCookieName + '=' + elem.value)'
      }})()

    </script>


}
