#' Search the Quandl database
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your authentication token go to https://www.quandl.com/account
#' @param query Search terms
#' @param page Specifies which page of results to return.
#' @param source Specifies a specific source to search within.
#' @param silent Prints the first few results when FALSE.
#' @param authcode Authentication Token for extended API access by default set by \code{\link{Quandl.api_key}}.
#' @return A list of the search results.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/help/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' search.results <- Quandl.search("oil")
#' }
#' @export
Quandl.search <- function(query, silent = FALSE, per_page = 10, ...) {
  params <- list()
  params$query <- query
  params$per_page <- per_page
  params <- c(params, list(...))

  path = "datasets"
  json <- do.call(quandl.api, c(path=path, params))

  # results is a dataframe
  results <- structure(json$datasets, meta = json$meta)

  if (!is.null(nrow(results)) && nrow(results) > 0) {
    for (i in 1:nrow(results)) {
      name <- results[i,]$name
      code <- paste(results[i,]$database_code, "/", results[i,]$dataset_code, sep="")
      desc <- results[i,]$description
      freq <- results[i,]$frequency
      colname <- results[i,]$column_names
      if (!silent) {
        cat(name, "\nCode: ", code, "\nDesc: ", desc, "\nFreq: ", freq, "\nCols: ", paste(colname, collapse="|"), "\n\n", sep="")
      }
    }
  } else {
    warning("No datasets found")
  }
  invisible(results)
}
