package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, 
			   ConnectionManager, 
			   Schemifier, 
			   DefaultConnectionIdentifier, 
			   StandardDBVendor}
import java.sql.{Connection, DriverManager}
import scala.xml.NodeSeq
import org.joda.time._
import org.allforgood._
import model._
import snippet._
import lib.{MenuData, AfgDate}
import api._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  implicit def toFunc(in: {def render(in: NodeSeq): NodeSeq}):
  NodeSeq => NodeSeq = param => in.render(param)
    
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    if (Props.testMode) {
      AfgDate.calcDateTimeFunc = Some(() => new DateTime("2008-12-28"))
    }
    
    LiftRules.passNotFoundToChain = false

    /*
    LiftRules.liftRequest.append {
      case Req("static" :: _, _, _) => false
    }
    */

    LiftRules.snippetDispatch.append{case "Loc" => MenuData}

    // where to search snippet
    LiftRules.addToPackages("org.allforgood")


    LiftRules.setSiteMapFunc(MenuData.siteMap) // () => SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.statelessDispatchTable.append {
      case r @ Req("api" :: "upload" :: Nil, _, _) =>
        () => Full(FeedUpload.upload(r))
      case r @ Req("api" :: "volopps" :: Nil, _, _) =>
        () => Full(Api.volopps(r))
    }

    /*
    LiftRules.snippetDispatch.append {
      case "DoYouLikeCats" => DoYouLikeCats
    }*/


    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

  /**
   * Set up resources
   */
  ResourceServer.allow {
    case "css" :: _ => true
  }
}

