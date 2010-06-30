package org.allforgood.snippet

/**
 * Created by IntelliJ IDEA.
 * User: imtiaz
 * Date: Jun 24, 2010
 * Time: 4:32:20 PM
 * To change this template use File | Settings | File Templates.
 *
 * If the property "googleapi.key" is present in net.liftweb.util.Props,
 *  returns JavaScript that does the following  -
 *      Loads the Google AJAX Maps API.
 *      The AJAX API loader, attempts to geo locate the client based on its IP address
 *      If it is successful, sets the JavaScript global var BrowserLoactionCity to the location(city) of the browser.
 *
 * To obtain a Google API key, visit http://code.google.com/apis/ajaxsearch/signup.html
 *
 */

import scala.xml.NodeSeq
import net.liftweb._
import util._
import Helpers._
import http.js._
import JsCmds._
import JE._

object GoogleApiLoad {
  def render: NodeSeq = 
    Props.get("googleapi.key") map {
      key =>
        <script type="text/javascript"
      src={"http://www.google.com/jsapi?key=" + urlEncode(key)}/> ++
      Script(JsRaw("""
        google.setOnLoadCallback(initialize);
        google.load('maps', '2');
        function initialize() {
            if (google.loader.ClientLocation)
              window.BrowserLocationCity = 
                google.loader.ClientLocation.address.city;            
        }
     """))
    } openOr Script(JsRaw("""
                          alert('You need to install the API key');
                          """))
}

