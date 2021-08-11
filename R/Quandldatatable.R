#' Retrieves Data from the Quandl Datatable endpoint
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/profile}
#'
#' @param datatable_code Datatable code on Quandl specified as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Quandl API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' Quandl.datatable('ZACKS/FC', paginate=TRUE)
#' }
#' @export
Quandl.datatable <- function(datatable_code, paginate = FALSE, ...) {
  path <- paste0("datatables/", datatable_code)
  quandl.datatable.perform(path, paginate, list(...))
}

quandl.datatable.perform <- function(path, paginate, params) {
  # make request for first page of data
  json <- do.call(quandl.api, c(path = path, params))
  datatable <- json$datatable
  data <- datatable$data

  # contains a list of names and corresponding types
  columns <- datatable$columns
  next_cursor_id <- json$meta$next_cursor_id
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # continue to make requests for data if paginate=TRUE and there is data
  while (isTRUE(paginate) && !is.null(next_cursor_id)) {
    params["qopts.cursor_id"] <- next_cursor_id
    json <- do.call(quandl.api, c(path = path, params))
    df_page <- as.data.frame(json$datatable$data, stringsAsFactors = FALSE)
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

#' Downloads a zip with all data requested from a Quandl database
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/profile}
#'
#' @param datatable_code Datatable code on Quandl specified as a string.
#' @param filename Filename (including path) of file to download.
#' @param ... Additional named values that are interpreted as Quandl API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' Quandl.datatable.bulk_download_to_file('ZACKS/EE')
#' }
#' @export
Quandl.datatable.bulk_download_to_file <- function(datatable_code, filename, ...) {
  download_link <- Quandl.datatable.bulk_download_url(datatable_code, ...)

  response <- httr::GET(download_link,
                        httr::write_disk(filename, overwrite = TRUE),
                        httr::progress())
}

#' Generates and returns a bulk download url
#'
#' @details Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/profile}
#'
#' @param datatable_code Datatable code on Quandl specified as a string.
#' @param ... Additional named values that are interpreted as Quandl API parameters. Please see \url{https://docs.quandl.com/docs/parameters-1} for a full list of parameters.
#' @return Returns the download url.
#' @seealso \code{\link{Quandl.api_key}}
#' @examples \dontrun{
#' url = Quandl.datatable.bulk_download_url("ZACKS/EE", ticker="AAPL")
#' }
#' @export
Quandl.datatable.bulk_download_url <- function(datatable_code, ...) {
  path <- paste0("datatables/", datatable_code)
  params <- c(list(...), qopts.export='true')

  json <- quandl.datatable.poll_export(path, params)

  download_link <- json$datatable_bulk_download$file$link
  return(download_link)
}

quandl.datatable.poll_export <-function(path, params) {
  json <- do.call(quandl.api, c(path = path, params))
  first_time <- Sys.time()
  while(!quandl.datatable.export_ready(json, first_time)) {
    Sys.sleep(60)
    json <- do.call(quandl.api, c(path = path, params))
  }
  return(json)
}

quandl.datatable.snapshot_time <- function(response) {
  if(is.null(response$datatable_bulk_download$file$data_snapshot_time)) {
    return('1970-01-01')
  } else {
    return(response$datatable_bulk_download$file$data_snapshot_time)
  }
}

quandl.datatable.export_ready <- function(response, first_time='1970-01-01') {
  return(response$datatable_bulk_download$file$status == 'fresh' | quandl.datatable.snapshot_time(response) >= first_time)
}
quandl.datatable.set_df_columns <- function(df, columns) {
  ncols <- length(columns[, 1])
  # if df is empty create an empty df with ncolumns set
  # or else we won't be able to set the column names
  if (nrow(df) <= 0 && ncols > 0) {
    df <- data.frame(matrix(ncol = ncols, nrow = 0))
  }

  # set column names
  names(df) <- columns[, 1]

  # set column types
  df <- quandl.datatable.convert_df_columns(df, columns[, 2])

  return(df)
}

quandl.datatable.convert_df_columns <- function(df, column_types) {
  if (length(column_types) <= 0) {
    return(df)
  }
  column_types <- tolower(column_types)
  for (i in 1:length(column_types)) {
    if (grepl("^float|^bigdecimal|^integer|^double", column_types[i])) {
      df[, i] <- as.numeric(df[, i])
    } else if (grepl("^datetime", column_types[i])) {
      df[, i] <- as.POSIXct(df[, i])
    } else if (grepl("^date", column_types[i])) {
      df[, i] <- as.Date(df[, i])
    } else {
      df[, i] <- as.character(df[, i])
    }
  }
  return(df)
}

quandl.datatable.max_rows <- function() {
  return(1000000)
}
