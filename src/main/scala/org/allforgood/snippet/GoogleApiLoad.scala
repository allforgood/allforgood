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

class GoogleApiLoad {
  import scala.xml.NodeSeq
  def render = {
    import net.liftweb.util.Props
    import scala.xml.NodeSeq
    val key = Props.get("googleapi.key", "")
    if ( key != "")
      <script type="text/javascript"
          src={"http://www.google.com/jsapi?key=" + key}></script> ++
      <script type="text/javascript">
        google.setOnLoadCallback(initialize);
        google.load('maps', '2');
        function initialize() {{
            if (google.loader.ClientLocation)
              window.BrowserLocationCity = google.loader.ClientLocation.address.city;            
        }}
      </script>
    else
      NodeSeq.Empty
  }  
}

