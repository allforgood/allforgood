
var SEARCH_LOCATION_COOKIE_NAME = 'search_location';
var SEARCH_LOCATION_COOKIE_MAX_AGE = 365;

window.BrowserLocationCity = '';

function setCookie(c_name,value,expiredays)
{
    var exdate=new Date();
    exdate.setDate(exdate.getDate()+expiredays);
    document.cookie=c_name+ "=" +escape(value)+
    ((expiredays==null) ? "" : ";expires="+exdate.toUTCString());
}

function getCookie(c_name)
{
    if (document.cookie.length>0)
      {
      c_start=document.cookie.indexOf(c_name + "=");
      if (c_start!=-1)
        {
        c_start=c_start + c_name.length+1;
        c_end=document.cookie.indexOf(";",c_start);
        if (c_end==-1) c_end=document.cookie.length;
        return unescape(document.cookie.substring(c_start,c_end));
        }
      }
    return null;
}

function getSearchLoc() {
    var cookie = getCookie(SEARCH_LOCATION_COOKIE_NAME);
    if ( cookie == null )
        return window.BrowserLocationCity;
    return cookie;
}

function setSearchLocCookie(location) {
    setCookie(SEARCH_LOCATION_COOKIE_NAME, location, SEARCH_LOCATION_COOKIE_MAX_AGE);
}



