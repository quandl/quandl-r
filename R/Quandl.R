auth_token <- NA

#' Query or set Quandl API token
#' @param auth_token Optionally passed parameter to set Quandl \code{auth_token}.
#' @return Returns invisibly the currently set \code{auth_token}.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' Quandl.auth('foobar')
#' }
#' @export
Quandl.auth <- function(auth_token) {

    if (!missing(auth_token))
        assignInNamespace('auth_token', auth_token, 'Quandl')

    invisible(Quandl:::auth_token)

}

#' Pulls Data from the Quandl API
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param code Dataset code on Quandl specified as a string.
#' @param type Type of data returned specified as string. Can be 'raw', 'ts', 'zoo' or 'xts'.
#' @param start_date Use to truncate data by start date in 'yyyy-mm-dd' format.
#' @param end_date Use to truncate data by end date in 'yyyy-mm-dd' format.
#' @param transformation Apply Quandl API data transformations.
#' @param collapse Collapse frequency of Data.
#' @param rows Select number of dates returned.
#' @param meta Returns meta data in list format as well as data.
#' @param authcode Authentication Token for extended API access by default set by \code{\link{Quandl.auth}}.
#' @return Depending on the outpug flag the class is either data.frame, time series, xts, zoo or a list containing one.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' quandldata = Quandl("NSE/OIL", collapse="monthly", start_date="2013-01-01", type="ts")
#' plot(quandldata[,1])
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom zoo zoo
#' @importFrom xts xts
#' @export
Quandl <- function(code, type = c('raw', 'ts', 'zoo', 'xts'), start_date, end_date, transformation = c('', 'diff', 'rdiff', 'normalize', 'cumul'), collapse = c('', 'weekly', 'monthly', 'quarterly', 'annual'), rows, meta = FALSE, authcode = Quandl.auth()) {

    ## Flag to indicate frequency change due to collapse
    freqflag = FALSE
    ## Check params
    type           <- match.arg(type)
    transformation <- match.arg(transformation)
    collapse       <- match.arg(collapse)

    ## Helper function
    frequency2integer <- function(freq) {
        switch(freq,
               'daily'    = 365,
               'monthly'  = 12,
               'quarterly' = 4,
               'yearly'   = 1,
               1)
    }

    ## Build API URL and add auth_token if available
    string <- paste("http://www.quandl.com/api/v1/datasets/", code, ".json?sort_order=asc&", sep="")
    if (is.na(authcode))
        warning("It would appear you aren't using an authentication token. Please visit http://www.quandl.com/help/r or your usage may be limited.")
    else
        string <- paste(string, "&auth_token=", authcode, sep = "")

    ## Add API options
    if (!missing(start_date))
        string <- paste(string, "&trim_start=", as.Date(start_date), sep = "")
    if (!missing(end_date))
        string <- paste(string,"&trim_end=", as.Date(end_date) ,sep = "")
    if (transformation %in% c("diff", "rdiff", "normalize", "cumul"))
        string <- paste(string,"&transformation=", transformation, sep = "")
    if (collapse %in% c("weekly", "monthly", "quarterly", "annual")) {
        string <- paste(string, "&collapse=", collapse, sep = "")
        freq   <- frequency2integer(collapse)
        freqflag = TRUE
    }
    if (!missing(rows))
        string <- paste(string,"&rows=", rows ,sep = "")

    ## Download and parse data
    json <- try(fromJSON(string, nullValue = as.numeric(NA)), silent = TRUE)

    ## Check if code exists
    if (inherits(json, 'try-error'))
        stop("Code does not exist")
    if (length(json) == 0)
        stop("Code does not exist")

    ## Detect frequency
    if (!freqflag)
        freq <- frequency2integer(json$frequency)

    ## Shell data from JSON's list
    data        <- as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE),stringsAsFactors=FALSE)
    names(data) <- json$column_names
    data[,1]    <- as.Date(data[, 1])

    ## Transform values to numeric
    if (ncol(data) > 2)
        data[, 2:ncol(data)]  <- apply(data[, 2:ncol(data)], 2, as.numeric)
    else
        data[, 2]  <- as.numeric(data[, 2])

    ## Returning raw data
    if (type == "raw")
        data_out <- data

    ## Returning ts object
    if (type == "ts") {
        date <- data[1,1]
        year <- 1900+as.POSIXlt(date)$year
        startdate <- 1
        if(freq == 1) {
            start <- year
        }
        else if (freq == 4) {
            quarter <- pmatch(quarters(date), c("Q1","Q2","Q3","Q4"))
            startdate <- c(year, quarter)
        }
        else if (freq == 12) {
            month <- 1+as.POSIXlt(date)$mon
            startdate <- c(year, month)
        }
        else
            freq <- 1
        data_out <- ts(data[, -1], frequency = freq, start = startdate)
    }
    ## Returning zoo object
    if (type == "zoo")
        data_out <- zoo(data[c(-1)],data[,1])

    ## Returning xts object
    if (type == "xts")
        data_out <- xts(data[c(-1)],data[,1])

    ## Append Metadata
    if (meta) {
        output <- list()
        output$data <- data_out
        output$frequency <-json$frequency
        output$name <- json$name
        output$description <- json$description
        output$updated <- json$updated_at
        source_code <- strsplit(code, "/")
        source_code <- source_code[[1]][1]
        source_string <- paste("http://www.quandl.com/api/v1/sources/", source_code, ".json", sep="")
        if (!is.na(authcode))
            source_string <- paste(source_string, "&auth_token=", authcode, sep = "")
        source_json <- fromJSON(source_string, nullValue = as.numeric(NA))
        output$source_name <- source_json$name
        output$source_link <- source_json$host
        output$source_description <- source_json$description
        return(output)
    }
    else {
        return(data_out)
    }
    ## Just in case
    stop("Invalid Type")

}
