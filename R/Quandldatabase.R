
#' Returns the bulk download url
#'
#' An api key is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your api key go to https://www.quandl.com/account/api
#' @param database_code Database code on Quandl specified as a string
#' @param ... Additional named values that are interpretted as api parameters e.g., download_type.
#' @return Returns the download url
#' @references This R package uses the Quandl API. For more information go to https://www.quandl.com/docs/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' quandldata = Quandl.database.download_url("NSE", download_type="partial")
#' }
#' @export
Quandl.database.bulk_download_url <- function(database_code, ...) {
  url <- paste(Quandl.base_url(), Quandl.database.download_url_path(database_code), sep="/")

  params <- list()
  if (!is.null(Quandl.api_key())) {
    params$api_key <- Quandl.api_key()
  }
  if (!is.null(Quandl.api_version())) {
    params$api_version <- Quandl.api_version()
  }
  params <- c(params, list(...))

  param_names <- names(params)
  if (length(params) > 0) {
    for(i in 1:length(params)) {
      delimiter <- "&"
      if (i == 1) {
        delimiter <- "?"
      }
      query <- paste(param_names[i], params[[i]], sep="=")
      url <- paste(url, query, sep=delimiter)
    }
  }
  url
}

#' Downloads a zip with all data from a Quandl database
#'
#' Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your api key go to https://www.quandl.com/account
#' @param database_code Database code on Quandl specified as a string
#' @param filename Filename where data is to be downloaded
#' @param ... Additional named values that are interpretted as api parameters e.g., download_type.
#' @return VALUE
#' @references This R package uses the Quandl API. For more information go to https://www.quandl.com/docs/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' quandldata = Quandl.database.download_url("NSE", download_type="partial")
#' }
#' @export
Quandl.database.bulk_download_to_file <- function(database_code, filename, ...) {
  if (dir.exists(filename)) {
    stop("Please add a filename to your directory path, e.g., ", filename, '/', database_code, '.zip', call. = FALSE)
  }
  dirname <- dirname(filename)
  if (!dir.exists(dirname)) {
    stop(dirname, " directory does not exist!", call. = FALSE)
  }
  quandl.api.download_file(Quandl.database.download_url_path(database_code), filename = filename, ...)
}

Quandl.database.download_url_path <- function(database_code) {
  paste("databases", database_code, "data", sep="/")
}
