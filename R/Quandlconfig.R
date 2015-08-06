#' Query or set Quandl API key
#' @param api_key Optionally passed parameter to set Quandl \code{api_key}.
#' @return Returns invisibly the currently set \code{api_key}.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' Quandl.api_key('foobar')
#' }
#' @export
Quandl.api_key <- function(api_key) {
  if (!missing(api_key)) {
    options(Quandl.api_key = api_key)
  }
  invisible(getOption('Quandl.api_key'))
}

#' @export
Quandl.auth <- function(api_key) {
  message("Quandl.auth() is deprecated. Use Quandl.api_key()")
  Quandl.api_key(api_key)
}

#' Query or set Quandl API version
#' @param api_version Optionally passed parameter to set Quandl \code{api_version}.
#' @return Returns invisibly the currently set \code{api_version}.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' Quandl.api_version('2015-04-09')
#' }
#' @export
Quandl.api_version <- function(api_version) {
  if (!missing(api_version)) {
    options(Quandl.api_version = api_version)
  }
  invisible(getOption('Quandl.api_version'))
}

Quandl.base_url <- function(base_url) {
  if (!missing(base_url)) {
    options(Quandl.base_url = base_url)
  }
  invisible(getOption('Quandl.base_url', 'https://www.quandl.com/api/v3'))
}
