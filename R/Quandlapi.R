#' Pulls Data from the Quandl API
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param version Set to the version of the Quandl API you want to access.
#' @param path Path to api resource.
#' @param headers Header function to collect header info.
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
#' @export

quandl.api <- function(version="v1", path, headers=NULL, http = c('GET', 'PUT', 'POST', 'DELETE'), ...) {
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

  switch(http,
    GET={
      response <- ifelse(is.null(headers), getURL(request_url), getURL(request_url, headerfunction=headers))
      },
    PUT={
      response <- ifelse(is.null(headers), getURL(request_url, customRequest = "PUT", httpheader=c("Content-Length"=nchar(postdata, type="bytes"), "Content-Type"="application/json"), postfields=postdata), getURL(request_url, customRequest = "PUT", headerfunction=headers, httpheader=c("Content-Length"=nchar(postdata, type="bytes"), "Content-Type"="application/json"), postfields=postdata))
      },
    POST={
      response <- postForm(request_url, .params=postdata)
      },
    DELETE={
      response <- ifelse(is.null(headers), httpDELETE(request_url), httpDELETE(request_url, headerfunction=headers))
    }
    )
  return(response)
}
