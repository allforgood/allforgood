
var SEARCH_LOCATION_COOKIE_NAME = 'search_location';
var SEARCH_LOCATION_COOKIE_MAX_AGE = 365;

function getSearchLoc() {
    var cookie = $.cookie(SEARCH_LOCATION_COOKIE_NAME);
    if ( cookie == null ) {
	try {
	    return google.loader.ClientLocation.address.city+", "+
		google.loader.ClientLocation.address.region;
	} catch (e) {
	    return "";
	}
    }
    return cookie;
}

function setSearchLocCookie(value) {
    $.cookie(SEARCH_LOCATION_COOKIE_NAME, value, 
	     { path : '/', expires : SEARCH_LOCATION_COOKIE_MAX_AGE });
}

