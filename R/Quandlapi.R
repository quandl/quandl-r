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
  # headers <- basicHeaderGatherer()
  params <- list(...)

  # if(http == 'PUT' || http == 'POST') {
  #   postdata <- params$postdata
  #   if (length(which(names(params) == "postdata")) == 0) {
  #     stop("No post data entered")
  #   }
  #   params[[which(names(params) == "postdata")]] <- NULL
  # }

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

  print(request_url)
  switch(http,
         GET={
           response <- httr::GET(request_url, nullValue=as.numeric(NA))
         },
         # PUT={
         #   response <- PUT(request_url, customRequest = "PUT", headerfunction=headers$update,
         #                      httpheader=c("Content-Length"=nchar(postdata, type="bytes"), "Content-Type"="application/json"), postfields=postdata, curl = Quandl.curlopts())
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
  # is.error = FALSE

  if(httr::status_code(response) == 500) {
    stop("Sorry but Quandl is currently down. Please visit our twitter (@quandl) for more information.", call. = FALSE)
  } else if (httr::status_code(response) != 200) {
    stop(httr::content(response), call. = FALSE)
  } else {
    # return(RJSONIO::fromJSON(httr::content(response, 'text'), nullValue = as.numeric(NA)))
    return(httr::content(response, simplifyVector = TRUE))
  }

  # if(http %in% c('GET', 'PUT', 'DELETE')) {
  #   status <- try(headers$value()[["status"]], silent = TRUE)
  #   if (inherits(status, 'try-error')) {
  #     stop("I am sorry but Quandl is down for maintenance. Please check the main website for status updates.", call. = FALSE)
  #   }

  #   if (length(grep("200", status))) {
  #   } else {
  #     is.error = TRUE
  #   }
  # }

  # json = try(fromJSON(response, nullValue = as.numeric(NA)), silent = TRUE)
  # if (inherits(json, 'try-error')) {
  #   if(is.error) {
  #     stop(response, call. = FALSE)
  #   } else {
  #     print(response)
  #     print(request_url)
  #     stop("Malformed JSON")
  #   }
  # }

  # if (is.error) {
  #   stop(json, call. = FALSE)
  # } else {
  #   return(json)
  # }

}



# > httr:::parsers[['application/json'']]
# + 
# > 
# > 
# > httr:::parsers[['application/json']]
# function (x, simplifyVector = FALSE, ...) 
# {
#     jsonlite::fromJSON(parse_text(x, encoding = "UTF-8"), simplifyVector = simplifyVector, 
#         ...)
# }
# <environment: namespace:httr>
# > httr:::parsers[['application/xml']]
# function (x, ...) 
# {
#     need_package("XML")
#     XML::xmlParse(parse_text(x, encoding = "UTF-8"), ...)
# }
# <environment: namespace:httr>
# > need_package
# Error: object 'need_package' not found
# > httr:::need_package
# function (pkg) 
# {
#     if (is_installed(pkg)) 
#         return(invisible())
#     stop("Please install ", pkg, " package", call. = FALSE)
# }
# <environment: namespace:httr>
# > class(httr:::parsers)
# [1] "environment"
# > bah <- function(x, ...)
# + {
# + jsonlite::fromJSON(parse_text(x, encoding = 'UTF-8'), nullValue = as.numeric(NA), ...)
# + }
# > httr:::parsers[['application/quandl']] = bah
# Error in httr:::parsers[["application/quandl"]] = bah : 
#   object 'httr' not found
# > assign('application/quandl', bah, envir = httr:::parsers)
# > httr:::parsers[['application/quandl']]
# function(x, ...)
# {
# jsonlite::fromJSON(parse_text(x, encoding = 'UTF-8'), nullValue = as.numeric(NA), ...)
# }
