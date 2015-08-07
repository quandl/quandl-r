#' Pulls Data from the Quandl API
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your authentication token go to https://www.quandl.com/account
#' @param version Set to the version of the Quandl API you want to access.
#' @param path Path to api resource.
#' @param http Type of http request sent.
#' @param postdata A character or raw vector that is sent in a body.
#' @param ... Named values that are interpretted as api parameters.
#' @return Website response.
#' @references This R package uses the Quandl API. For more information go to https://www.quandl.com/help/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' quandldata = quandl.api(version="v1", path="datasets/NSE/OIL", http="GET")
#' plot(quandldata[,1])
#' }
#' @export
quandl.api <- function(path, http = c('GET', 'PUT', 'POST', 'DELETE'), postdata = NULL, ...) {
  http <- match.arg(http)
  request <- quandl.api.build_request(path, ...)
  response <- httr::VERB(http, request$request_url, config = do.call(httr::add_headers, request$headers),
                                      body = postdata, query = request$params)

  quandl.api.handl_errors(response)
  text_response <- httr::content(response, as="text")

  json_response <- tryCatch(jsonlite::fromJSON(text_response, simplifyVector=TRUE), error = function(e) {
      stop(e, " Failed to parse response: ", text_response)
    })
  json_response
}

quandl.api.download_file <- function(path, filename, ...) {
  request <- quandl.api.build_request(path, ...)
  response <- httr::GET(request$request_url, config = do.call(httr::add_headers, request$headers),
                               query = request$params, httr::write_disk(filename, overwrite = TRUE), progress())
  quandl.api.handl_errors(response)
  cat("Saved to file:", response$content)
}

quandl.api.build_request <- function(path, ...) {
  params <- list(...)
  request_url <- paste(Quandl.base_url(), path, sep="/")
  accept_value <- "application/json"
  if (!is.null(Quandl.api_version())) {
    accept_value <- paste0('application/json, application/vnd.quandl+json;version=', Quandl.api_version())
  }

  headers <- list(Accept = accept_value, `Request-Source` = 'R', `Request-Source-Version` = Quandl.version)

  if (!is.null(Quandl.api_key())) {
    headers <- c(headers, list(`X-Api-Token` = Quandl.api_key()))
  }

  # query param api_key takes precedence
  if (!is.null(params$api_key)) {
    headers <- c(headers, list(`X-Api-Token` = params$api_key))
    params$api_key <- NULL
  }

  list(request_url = request_url, headers = headers, params = params)
}

quandl.api.handl_errors <- function(response) {
  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as="text"), call. = FALSE)
  }
}
