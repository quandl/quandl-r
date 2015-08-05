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
  invisible(getOption('Quandl.api_version', '2015-04-09'))
}