
#' Returns the bulk download url
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/api}
#'
#' @param database_code Database code on Quandl specified as a string.
#' @param ... Additional named values that are interpreted as Quandl API parameters. Please see \url{https://www.quandl.com/docs/api#entire-database} for a full list of parameters.
#' @return Returns the download url.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' url = Quandl.database.download_url("NSE", download_type="partial")
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
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/api}
#'
#' @param database_code Database code on Quandl specified as a string.
#' @param filename Filename (including path) of file to download.
#' @param ... Additional named values that are interpreted as Quandl API parameters. Please see \url{https://www.quandl.com/docs/api#entire-database} for a full list of parameters.
#' @return The filename of the downloaded file.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' Quandl.database.bulk_download_to_file("NSE", "./NSE.zip")
#' }
#' @export
Quandl.database.bulk_download_to_file <- function(database_code, filename, ...) {
  dirname <- dirname(filename)
  quandl.api.download_file(Quandl.database.download_url_path(database_code), filename = filename, ...)
}

Quandl.database.download_url_path <- function(database_code) {
  paste("databases", database_code, "data", sep="/")
}
