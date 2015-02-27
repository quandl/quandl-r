#' Search the Quandl database
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param query Search terms
#' @param page Specifies which page of results to return.
#' @param source Specifies a specific source to search within.
#' @param silent Prints the first few results when FALSE.
#' @param authcode Authentication Token for extended API access by default set by \code{\link{Quandl.auth}}.
#' @return A list of the search results.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' search.results <- Quandl.search("oil")
#' }
#' @export
Quandl.search <- function(query, page=1, source=NULL, silent=FALSE, authcode=Quandl.auth()) {

  params <- list()
  parsedQuery <- gsub(" ", "+", query)
  params$query <- parsedQuery

  # url <- paste("http://www.quandl.com/api/v1/datasets.json?query=", parsedQuery, sep="")
  if (is.na(authcode)) {
    warning("It would appear you aren't using an authentication token.
            Please visit http://www.quandl.com/help/r or your usage may be limited.")
  } else {
    params$auth_token <- authcode
  }

  if (!is.null(source)) {
    params$source_code <- source
  }
  params$page <- as.character(page)
  path = "datasets"
  json <- do.call(quandl.api, c(path=path, params))

  # if there are zero returns than inform the user about it
  params$total_count <- json$total_count
  if(params$total_count == 0){
    warning("It seems as we haven't found anything.")
  }

  # print(params$total_count)
  # json <- try(fromJSON(response),silent=FALSE)
  # print(json)
  if (inherits(json, 'try-error')) {
    stop("No data")
  }

  list <- list()
  length(list) <- length(json$docs)
  if (length(json$docs)>0) {
    for (i in 1:length(json$docs)) {
      name <- json$docs[[i]]$name
      code <- paste(json$docs[[i]]$source_code, "/", json$docs[[i]]$code, sep="")
      desc <- json$docs[[i]]$description
      freq <- json$docs[[i]]$frequency
      colname <- json$docs[[i]]$column_names
      if (i < 4 & !silent) {
        cat(name, "\nCode: ", code, "\nDesc: ", desc, "\nFreq: ", freq, "\nCols: ", paste(colname, collapse="|"), "\n\n", sep="")
      }
      list[[i]]$name <- name
      list[[i]]$code <- code
      list[[i]]$description <- desc
      list[[i]]$frequency <- freq
      list[[i]]$column_names <- colname
      list[[i]]$from_date <- json$docs[[i]]$from_date
      list[[i]]$to_date <- json$docs[[i]]$to_date
    }
  }

  invisible(list)
}
