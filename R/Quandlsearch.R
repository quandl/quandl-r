#' Search the Quandl database
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.api_key} function.
#'
#' For instructions on finding your authentication token go to https://www.quandl.com/account/api
#' @param query Search terms
#' @param silent Prints the first few results when FALSE.
#' @param per_page Number of results returned per page.
#' @param ... Additional named values that are interpretted as api parameters e.g., page.
#' @return A list of the search results.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/docs/api. For more help on the package itself go to http://www.quandl.com/help/r.
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

  path <- "datasets"
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
        cat(name, "\nCode: ", code, "\nDesc: ", desc, "\nFreq: ", freq, "\nCols: ", paste(unlist(colname), collapse=" | "), "\n\n", sep="")
      }
    }
  } else {
    warning("No datasets found")
  }
  invisible(results)
}
