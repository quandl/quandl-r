#' Search the Quandl database
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/api}
#'
#' For instructions on finding your authentication token go to https://www.quandl.com/account/api
#' @param query Search terms
#' @param silent Prints the results when FALSE.
#' @param per_page Number of results returned per page.
#' @param ... Additional named values that are interpretted as Quandl API parameters.
#' @return Search results returned as a data.frame.
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
