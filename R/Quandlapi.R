#' Executes Quandl API calls
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/api}
#'
#' @param path Path to api resource.
#' @param http Type of http request sent.
#' @param postdata A character or raw vector that is sent in a body.
#' @param ... Named values that are interpretted as Quandl API parameters. Please see \url{https://www.quandl.com/docs/api}.
#' @return Quandl API response.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' quandldata = quandl.api(path="datasets/NSE/OIL", http="GET")
#' plot(quandldata[,1])
#' }
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
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
                               query = request$params, httr::write_disk(filename, overwrite = TRUE), httr::progress())
  quandl.api.handl_errors(response)
  cat("Saved to file:", response$content)
}

quandl.api.build_request <- function(path, ...) {
  params <- list(...)
  # ensure vectors get converted into v3 api supported query params
  # e.g., qopts.columns=c('ticker', 'rev') -> list('qopts.columns[]'=ticker,'qopts.columns[]'=rev)
  params <- quandl.api.build_query_params(params)
  # ensure Dates convert to characters or else curl will convert the Dates to timestamp
  params <- quandl.api.convert_dates_to_character(params)

  request_url <- paste(Quandl.base_url(), path, sep="/")
  accept_value <- "application/json"
  if (!is.null(Quandl.api_version())) {
    accept_value <- paste0('application/json, application/vnd.quandl+json;version=', Quandl.api_version())
  }

  quandl_version <- as.character(utils::packageVersion('Quandl'))
  headers <- list(Accept = accept_value, `Request-Source` = 'R', `Request-Source-Version` = quandl_version)

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

quandl.api.convert_dates_to_character <- function(params) {
  convert_date_to_character <- function(param) {
    if (class(param) == 'Date') {
      param <- as.character(param)
    }
    param
  }
  lapply(params, convert_date_to_character)
}

quandl.api.build_query_params <- function(params) {
  if (length(params) <= 0) {
    return(params)
  }
  mod_params <- list()
  for(i in 1:length(params)) {
    # keep the params the same if not a vector
    converted_params <- params[i]

    # check val to see if vector
    # if so, convert
    if (length(params[[i]]) > 1) {
      converted_params <- quandl.api.convert_vector_params(names(params[i]), params[[i]])
    }
    mod_params <- c(mod_params, converted_params)
  }
  return(mod_params)
}

quandl.api.convert_vector_params <- function(name, vector_values) {
  mod_query_name <- paste0(name, '[]')
  mod_query_list <- list()

  for(val in vector_values) {
    l <- list()
    l[[mod_query_name]] <- val
    mod_query_list <- c(mod_query_list, l)
  }
  return(mod_query_list)
}
