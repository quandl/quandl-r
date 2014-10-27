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
#' @importFrom RCurl getURL
#' @importFrom RCurl postForm
#' @importFrom RCurl httpDELETE
#' @importFrom RCurl basicHeaderGatherer
#' @export

quandl.api <- function(version="v1", path, http = c('GET', 'PUT', 'POST', 'DELETE'), ...) {
  headers <- basicHeaderGatherer()
  params <- list(...)
  if(http == 'PUT' || http == 'POST') {
    postdata <- params$postdata
    if (length(which(names(params)=="postdata")) == 0) stop("No post data entered")
    params[[which(names(params)=="postdata")]] <- NULL
  }
  params$request_source <- 'R'
  params$request_version <- Quandl.version

  http <- match.arg(http)
  request_url <- paste(paste("http://www.quandl.com/api", version, path, sep="/"), "?", sep="")
  param_names <- names(params)

  if(length(params) >0) {for(i in 1:length(params)) {request_url <- paste(request_url, "&", param_names[i], "=", params[[i]], sep="")}}
  # print(request_url)
  switch(http,
    GET={
      response <- getURL(request_url, customRequest = "GET", headerfunction=headers$update, curl = Quandl.curlopts())
      },
    PUT={
      response <- getURL(request_url, customRequest = "PUT", headerfunction=headers$update, httpheader=c("Content-Length"=nchar(postdata, type="bytes"), "Content-Type"="application/json"), postfields=postdata, curl = Quandl.curlopts())
      },
    POST={
      response <- postForm(request_url, .params=postdata, curl = Quandl.curlopts())
      },
    DELETE={
      response <- httpDELETE(request_url, headerfunction=headers$update, curl = Quandl.curlopts())
    }
    )
  is.error = FALSE
  if(http %in% c('GET', 'PUT', 'DELETE')) {
    status <- try(headers$value()[["status"]], silent=TRUE)
    if (inherits(status, 'try-error'))
       stop("I am sorry but Quandl is down for maintenance. Please check the main website for status updates.", call. = FALSE) 
    if (length(grep("200", status))) {}
    else {
      is.error = TRUE
    }
  }
  json = try(fromJSON(response, nullValue = as.numeric(NA)), silent = TRUE)
  if (inherits(json, 'try-error')) {
    if(is.error)
      stop(response, call. = FALSE)
    else {
      print(response)
      print(request_url)
      stop("Malformed JSON")
    }
  }
  if (is.error)
    stop(json, call. = FALSE)
  else
    return(json)

}
