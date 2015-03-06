#' Pulls Data from the Quandl API
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param version Set to the version of the Quandl API you want to access.
#' @param path Path to api resource.
#' @param http Type of http request sent.
#' @param ... Named values that are interpretted as api parameters.
#' @return Website response.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' quandldata = quandl.api(version="v1", path="datasets/NSE/OIL", http="GET")
#' plot(quandldata[,1])
#' }
#' @export
quandl.api <- function(version="v1", path, http = c('GET', 'PUT', 'POST', 'DELETE'), postdata = NULL, ...) {

  params <- list(...)

  params$request_source <- 'R'
  params$request_version <- Quandl.version

  http <- match.arg(http)
  request_url <- paste(paste(Quandl.host, version, path, sep="/"), "?", sep="")
  param_names <- names(params)
  if(length(params) > 0) {
    for(i in 1:length(params)) {
      request_url <- paste(request_url, "&", param_names[i], "=", params[[i]], sep="")
    }
  }


  switch(http,
         GET={
           response <- httr::GET(request_url, nullValue=as.numeric(NA))
         },
         PUT={
           response <- httr::PUT(request_url, body=postdata)
         },
         POST={
           response <- httr::POST(request_url, body=postdata)
         },
         DELETE={
           response <- httr::DELETE(request_url)
         }
  )


  if(httr::status_code(response) == 500) {
    stop("Sorry but Quandl is currently down. Please visit our twitter (@quandl) for more information.", call. = FALSE)
  } else if (httr::status_code(response) != 200) {
    stop(httr::content(response), call. = FALSE)
  } else {
    return(httr::content(response, simplifyVector = TRUE))
  }


}


