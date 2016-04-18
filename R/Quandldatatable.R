#' Retrieves Data from the Quandl Datatable endpoint
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/api}
#'
#' @param code Datatable code on Quandl specified as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Quandl API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' Quandl.datatable('ZACKS/FC', paginate=TRUE)
#' }
#' @export
Quandl.datatable <- function(code, paginate=FALSE, ...) {
  path <- paste0("datatables/", code)
  params <- list(...)

  # make request for first page of data
  json <- do.call(quandl.api, c(path=path, params))
  datatable <- json$datatable
  data <- datatable$data
  # contains a list of names and corresponding types
  columns <- datatable$columns
  next_cursor_id <- json$meta$next_cursor_id
  df <- as.data.frame(data, stringsAsFactors=FALSE)

  # continue to make requests for data if paginate=TRUE and there is data
  while (isTRUE(paginate) && !is.null(next_cursor_id)) {
    params['qopts.cursor_id'] <- next_cursor_id
    json <- do.call(quandl.api, c(path=path, params))
    df_page <- as.data.frame(json$datatable$data, stringsAsFactors=FALSE)
    df <- rbind(df, df_page)
    next_cursor_id <- json$meta$next_cursor_id

    # only fetch a maximum of 1,000,000 rows
    if (nrow(df) >= quandl.datatable.max_rows() && !is.null(next_cursor_id)) {
      warning(paste("This call returns a larger amount of data than Quandl.datatable() allows.",
                    "Please view our documentation on developer methods to request more data.",
                    "https://github.com/quandl/quandl-r/blob/master/README.md#datatables"), call. = FALSE)
      break
    }
  }

  if (!isTRUE(paginate) && !is.null(next_cursor_id)) {
    warning(paste("This call returns more data. To request more pages, please set paginate=TRUE",
                  "in your Quandl.datatable() call. For more information see our documentation:",
                  "https://github.com/quandl/quandl-r/blob/master/README.md#datatables"), call. = FALSE)
  }

  df <- quandl.datatable.set_df_columns(df, columns)

  return(df)
}

quandl.datatable.set_df_columns <- function(df, columns) {
  ncols <- length(columns[,1])
  # if df is empty create an empty df with ncolumns set
  # or else we won't be able to set the column names
  if (nrow(df) <= 0 && ncols > 0) {
    df <- data.frame(matrix(ncol = ncols, nrow = 0))
  }

  # set column names
  names(df) <- columns[,1]

  # set column types
  df <- quandl.datatable.convert_df_columns(df, columns[,2])

  return(df)
}

quandl.datatable.convert_df_columns <- function(df, column_types) {
  if (length(column_types) <= 0) {
    return(df)
  }
  column_types <- tolower(column_types)
  for(i in 1:length(column_types)) {
    if (grepl("^float|^bigdecimal|^integer", column_types[i])) {
      df[,i] <- as.numeric(df[,i])
    } else if (grepl("^date", column_types[i])) {
      df[,i] <- as.Date(df[,i])
    } else {
      df[,i] <- as.character(df[,i])
    }
  }
  return(df)
}

quandl.datatable.max_rows <- function() {
  return(1000000)
}
