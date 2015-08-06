
#' Returns the bulk download url
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your authentication token go to https://www.quandl.com/account
#' @param database_code Database code on Quandl specified as a string
#' @param ... Additional named values that are interpretted as api parameters e.g., download_type.
#' @return Returns the download url
#' @references This R package uses the Quandl API. For more information go to https://www.quandl.com/help/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
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

#' @export
Quandl.database.bulk_download_to_file <- function(database_code, folder_path, ...) {
  if (!dir.exists(folder_path)) {
    stop(folder_path, " directory does not exist!", call. = FALSE)
  }
  filename = paste0(database_code, '.zip')
  file_path = file.path(folder_path, filename)
  quandl.api.download_file(Quandl.database.download_url_path(database_code), file_path = file_path, ...)
}

Quandl.database.download_url_path <- function(database_code) {
  paste("databases", database_code, "data", sep="/")
}
