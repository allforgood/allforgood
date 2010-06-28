
var SEARCH_LOCATION_COOKIE_NAME = 'search_location';
var SEARCH_LOCATION_COOKIE_MAX_AGE = 365;

function getSearchLoc() {
    var cookie = $.cookie(SEARCH_LOCATION_COOKIE_NAME)
    if ( cookie == null )
        if ( window.BrowserLocationCity == null )
            return '';
        else
            return window.BrowserLocationCity ;
    return cookie;
}

function setSearchLocCookie(value) {
    $.cookie(SEARCH_LOCATION_COOKIE_NAME, value, { path : '/', expires : SEARCH_LOCATION_COOKIE_MAX_AGE })
}

