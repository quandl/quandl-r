Quandl.auth_token <- NA
Quandl.host <- 'https://www.quandl.com/api'
Quandl.version <- '2.5.2'
Quandl.curl <- NA


#' Query or set Quandl API token
#' @param auth_token Optionally passed parameter to set Quandl \code{auth_token}.
#' @return Returns invisibly the currently set \code{auth_token}.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' Quandl.auth('foobar')
#' }
#' @export
Quandl.auth <- function(auth_token) {
  # Checks if a new token is being assigned and assigns it.
  if (!missing(auth_token)) {
    assignInMyNamespace('Quandl.auth_token', auth_token)
  }
  invisible(Quandl.auth_token)
}

#' Retrieve metadata from a Quandl series
#' @param x A Quandl time series object with attached meta data.
#' @return Returns a list of meta data about the series.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' metaData(ts)
#' }
#' @export
metaData <- function(x){
  attr(x, "meta")
}

#' Pulls Data from the Quandl Dataset endpoint and formats
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param code Dataset code on Quandl specified as a string or an array of strings.
#' @param type Type of data returned specified as string. Can be 'raw', 'ts', 'zoo', 'xts' or 'timeSeries'.
#' @param start_date Use to truncate data by start date in 'yyyy-mm-dd' format.
#' @param end_date Use to truncate data by end date in 'yyyy-mm-dd' format.
#' @param transformation Apply Quandl API data transformations.
#' @param collapse Collapse frequency of Data.
#' @param sort Select if data is given to R in ascending or descending formats. Helpful for the rows parameter.
#' @param meta Returns meta data in list format as well as data.
#' @param authcode Authentication Token for extended API access by default set by \code{\link{Quandl.auth}}.
#' @param ... Additional named values that are interpretted as api parameters.
#' @return Depending on the type the class is either data.frame, time series, xts, zoo or timeSeries.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' quandldata = Quandl("NSE/OIL", collapse="monthly", start_date="2013-01-01", type="ts")
#' plot(quandldata[,1])
#' }
#' @importFrom zoo zoo
#' @importFrom zoo as.zooreg
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.yearqtr
#' @importFrom xts xts
#' @importFrom xts as.xts
#' @export
Quandl <- function(code, type = c('raw', 'ts', 'zoo', 'xts', 'timeSeries'), start_date, end_date, transformation = c('', 'diff', 'rdiff', 'normalize', 'cumul', 'rdiff_from'), collapse = c('', 'daily', 'weekly', 'monthly', 'quarterly', 'annual'), sort = c('desc', 'asc'), meta = FALSE, authcode = Quandl.auth(), ...) {
  params = list()
  ## Flag to indicate frequency change due to collapse
  freqflag = FALSE
  ## Default to single dataset
  multiset = FALSE
  ## Default to entire dataset
  col = NULL
  ## Check params
  type                    <- match.arg(type)
  params$transformation   <- match.arg(transformation)
  params$collapse         <- match.arg(collapse)
  params$sort_order       <- match.arg(sort)

  if (type == 'timeSeries' && system.file(package = type) == "") {
    stop("Package ", type, " needed to use this type", call. = FALSE)
  }

  ## Helper functions
  frequency2integer <- function(freq) {

    if (is.null(freq) || is.na(freq)) {
      return(365)
    } else {
      switch(freq,
           'daily'    = 365,
           'weekly'   = 52,
           'monthly'  = 12,
           'quarterly' = 4,
           'yearly'   = 1,
           1)
    }
  }

  as.year <- function(x) {
    floor(as.numeric(as.yearmon(x)))
  }

  format.code <- function(code) {
    col <- NULL
    if (!all(gsub("[^A-Z0-9_./]", "", code) == code)) {
      stop("Codes are comprised of capital letters, numbers and underscores only.")
    }

    codearray <- strsplit(code, "/")
    if (length(codearray[[1]]) == 3) {
      col <- codearray[[1]][3]
      code <- paste(codearray[[1]][1:2], collapse='/')
    } else if (length(strsplit(code, "\\.")[[1]]) == 2) {
      col <- strsplit(code, "\\.")[[1]][2]
      code <- strsplit(code, "\\.")[[1]][1]
    } else if (length(strsplit(code, "\\.")[[1]]) == 3) {
      col <- strsplit(code, "\\.")[[1]][2]
      code <- paste(strsplit(code, "\\.")[[1]][1:2], collapse='/')
    }

    return(c(code, col))
  }

  if (!is.na(authcode)) {
    params$auth_token <- authcode
  }

  ## Add API options
  if (!missing(start_date)) {
    params$trim_start <- as.Date(start_date)
  }

  if (!missing(end_date)) {
    params$trim_end <- as.Date(end_date)
  }

  # if (type != "raw")
  #     params$sort_order <- "asc"
  if (params$collapse %in% c("weekly", "monthly", "quarterly", "annual")) {
    freq   <- frequency2integer(collapse)
    freqflag = TRUE
  }

  params <- c(params, list(...))
  if(!is.null(params$force_irregular)) {
    force_irregular <- TRUE
    params[[which(names(params) == "force_irregular")]] <- NULL
  } else {
    force_irregular <- FALSE
  }

  ## Download and parse data
  errors <- list()
  if(length(code) == 1) {
    code_col <- format.code(code)
    code <- code_col[1]
    col <- code_col[2]

    if(!is.null(col) && !is.na(col)) {
      params$column <- col
    }

    if(meta) {
      params$meta <- meta
    }

    # download data
    data <- Quandl.dataset.get(code, params)
    if(params$collapse != '') {
      freq <- frequency2integer(params$collapse)
    } else {
      freq <- frequency2integer(attr(data, "freq"))
    }
  } else {
    data <- NULL

    for(c in code) {
      tmp.params <- params
      code_col <- format.code(c)
      c <- code_col[1]
      col <- code_col[2]

      if(!is.null(col) && !is.na(col)) {
        tmp.params$column <- col
      }

      merge_data <- tryCatch(Quandl.dataset.get(c, tmp.params), error=function(e) {
        d <- data.frame(Date=character(0), ERROR=numeric(0))
        attr(d, "errors") <- e
        return(d)
      })

      if(is.null(col)) {
        suppressWarnings(errors[c] <- attr(merge_data, "errors"))
      } else {
        suppressWarnings(errors[paste(c,col,sep=".")] <- attr(merge_data, "errors"))
      }

      for(i in 2:length(names(merge_data))) {
        names(merge_data)[i] <- paste(sub('/','.',c), names(merge_data)[i], sep=' - ')
      }

      if(is.null(data)) {
        data <- merge_data
      } else {
        data <- merge(data, merge_data, by=1, all=TRUE)
      }
    }

    multiset = TRUE
    freqflag = TRUE ## Frequency not automatically supported with multisets
    if(params$collapse != '') {
      freq <- frequency2integer(params$collapse)
    } else {
      freq <- 365
    }

    if(type == "raw" && params$sort_order == "desc") {
      data <- data[order(data[,1], decreasing=TRUE),]
    }
  }

  meta <- attr(data, "meta")

  ## Returning raw data
  if (type == "raw") {
    data_out <- data
  } else {
    # Deal with regularly spaced time series first
    if (freq %in% c(1, 4, 12) && !force_irregular) {
      # Build regular zoo with correct frequency
      if(freq == 1) {
        data_out <- zoo(data[,-1], frequency = freq, as.year(data[,1]))
      } else if (freq == 4) {
        data_out <- zoo(data[,-1], frequency = freq, as.yearqtr(data[,1]))
      } else if (freq == 12) {
        data_out <- zoo(data[,-1], frequency = freq, as.yearmon(data[,1]))
      }

      # Convert to type
      if (type == "ts") {
        data_out <- as.ts(data_out)
      } else if (type == "zoo") {
        data_out <- as.zooreg(data_out)
      } else if (type == "xts") {
        data_out <- if(freq == 1) {
          xts(data[, -1], frequency = 1, order.by=data[, 1])
        } else  {
          as.xts(data_out)
        }
        if (freq != frequency(data_out)) {
          warning("xts has a non-standard meaning for 'frequency'.")
        }
      } else if (type == "timeSeries") {
        data_out <- timeSeries::timeSeries(data=data[, -1], charvec=data[, 1])
      }

    } else if (type=="zoo" || type=="ts") {
      # Time series is not regularly spaced
      if (type == "ts") {
        warning("Type 'ts' does not support frequency ", freq, ". Returning zoo.")
      }
      data_out <- zoo(data[, -1], order.by=data[, 1])
    } else if (type == "xts") {
      data_out <- xts(data[, -1], order.by=data[, 1])
    } else if (type == "timeSeries") {
      data_out <- timeSeries::timeSeries(data = data[, -1], charvec=data[, 1])
    }
  }

  if(length(errors) > 0) {
    attr(data_out, "errors") <- errors
  }

  if(!is.null(meta)) {
    attr(data_out, "meta") <- meta
  }

  return(data_out)
}

#' Pulls Data from the Quandl Dataset endpoint
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param code Dataset code on Quandl specified as a string or an array of strings.
#' @param params A list of parameters to be passed to the Quandl api.
#' @return Returns a data.frame of the requested data
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' quandldata = Quandl.dataset.get("NSE/OIL", list(rows=5))
#' plot(quandldata[,1])
#' }
#' @export
Quandl.dataset.get <- function(code, params) {
  if(!is.null(params$meta) && params$meta) {
    meta <- params$meta
    params[[which(names(params) == "meta")]] <- NULL
  } else {
    meta <- FALSE
  }

  if(!is.null(params$auth_token)) {
    authcode <- params$auth_token
  } else {
    authcode <- ""
  }

  path <- path <- paste("datasets/", code, sep="")
  json <- do.call(quandl.api, c(path=path, params))
  #return(json)
  #print(json)
  if (length(json$data) == 0) {
    stop("Requested Entity does not exist.")
  }

  ## Detect frequency
  # freq <- frequency2integer(json$frequency)
  if (!is.null(params$column) && length(json$column_names) > 2) {
    json$column_names = json$column_names[c(1, as.numeric(params$column)+1)]
  }

  ## Shell data from JSON's list
  # data <- tryCatch(as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE), stringsAsFactors=FALSE),
  #                  warning=function(w) {
  #                    warning(w)
  #                    warning(paste("This warning is most likely the result of a data structure error. If the output of this function does not make sense please email connect@quandl.com with the Quandl code: ", code), call. = FALSE)
  #                    return(suppressWarnings(as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE),stringsAsFactors=FALSE)))
  #                  })
  data <- as.data.frame(json$data)
  # data <- do.call(rbind, lapply(json$data, rbind))
  # data[apply(data, 1:2,is.null)] <- NA
  names(data) <- json$column_names
  data[,1]    <- as.Date(data[, 1])

  ## Transform values to numeric
  if (ncol(data) > 2) {
    data[, 2:ncol(data)]  <- apply(data[, 2:ncol(data)], 2, as.numeric)
  } else {
    data[, 2]  <- as.numeric(data[, 2])
  }

  if (meta) {
    source_json <- quandl.api(path=paste("sources", json$source_code, sep="/"), auth_token=authcode)
    meta <- list(
      frequency   = json$frequency,
      name        = json$name,
      description = json$description,
      updated     = json$updated_at,
      source_code = json$source_code,
      code        = paste(json$source_code, json$code, sep = "/"),
      source_name = source_json$name,
      source_link = source_json$host,
      source_description = source_json$description
    )
    attr(data, "meta") <- meta
  }

  attr(data, "freq") <- json$frequency

  return(data)
}
