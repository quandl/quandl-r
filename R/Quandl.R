Quandl.auth_token <- NA
Quandl.remaining_limit <- NA
Quandl.version <- '2.3.1'

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
    if (!missing(auth_token))
        assignInMyNamespace('Quandl.auth_token', auth_token)

    invisible(Quandl.auth_token)

}

#' Query remaining API limit
#' @param remaining_limit Optionally passed parameter to update Quandl \code{remaining_limit}
#' @param force_check Forces the function to requery Quandl for the api limit remaining, could be used after an authentication token change.
#' @return Returns the number of remaining API calls.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' Quandl.limit(700)
#' }
#' @importFrom RCurl getURL
#' @importFrom RCurl basicHeaderGatherer
#' @export
Quandl.limit <- function(remaining_limit, force_check=FALSE) {
    if (!missing(remaining_limit)) {
        assignInMyNamespace('Quandl.remaining_limit', remaining_limit)
    }
    else if (is.na(Quandl.remaining_limit) || force_check) {
        headers <- basicHeaderGatherer()
        if (is.na(Quandl.auth())) {
            response <- quandl.api("v2", "datasets/TAMMER/RANDOM", headers = headers$update)
            status <- try(headers$value()[["status"]], silent=TRUE)
            if (length(grep("403", status)) || length(grep("429", status))) {
                stop(response)
            }
            assignInMyNamespace('Quandl.remaining_limit', headers$value()[["X-RateLimit-Remaining"]])
        }
        else {
            return(NA)
        }
    }

    return(Quandl.remaining_limit)

}

#' Retrieve metadata from a Quandl series
#' @param x A Quandl time series object with attached meta data.
#' @return Returns a list of meta data about the series.
#' @seealso \code{\link{Quandl}}
#' @examples \dontrun{
#' metaData(ts)
#' }
#' @export

metaData <- function(x)attr(x, "meta")

#' Pulls Data from the Quandl API
#'
#' An authentication token is needed for access to the Quandl API multiple times. Set your \code{access_token} with \code{Quandl.auth} function.
#'
#' For instructions on finding your authentication token go to www.quandl.com/API
#' @param code Dataset code on Quandl specified as a string or an array of strings.
#' @param type Type of data returned specified as string. Can be 'raw', 'ts', 'zoo' or 'xts'.
#' @param start_date Use to truncate data by start date in 'yyyy-mm-dd' format.
#' @param end_date Use to truncate data by end date in 'yyyy-mm-dd' format.
#' @param transformation Apply Quandl API data transformations.
#' @param collapse Collapse frequency of Data.
#' @param sort Select if data is given to R in ascending or descending formats. Helpful for the rows parameter.
#' @param meta Returns meta data in list format as well as data.
#' @param authcode Authentication Token for extended API access by default set by \code{\link{Quandl.auth}}.
#' @param ... Additional named values that are interpretted as api parameters.
#' @return Depending on the outpug flag the class is either data.frame, time series, xts, zoo or a list containing one.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' quandldata = Quandl("NSE/OIL", collapse="monthly", start_date="2013-01-01", type="ts")
#' plot(quandldata[,1])
#' }
#' @importFrom RCurl getURL
#' @importFrom RCurl basicHeaderGatherer
#' @importFrom RJSONIO fromJSON
#' @importFrom zoo zoo
#' @importFrom zoo as.zooreg
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.yearqtr
#' @importFrom xts xts
#' @importFrom xts as.xts
#' @export
Quandl <- function(code, type = c('raw', 'ts', 'zoo', 'xts'), start_date, end_date, transformation = c('', 'diff', 'rdiff', 'normalize', 'cumul', 'rdiff_from'), collapse = c('', 'weekly', 'monthly', 'quarterly', 'annual'), sort = c('desc', 'asc'), meta = FALSE, authcode = Quandl.auth(), ...) {
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


    ## Helper functions
    frequency2integer <- function(freq) {
        switch(freq,
               'daily'    = 365,
               'weekly'   = 52,
               'monthly'  = 12,
               'quarterly' = 4,
               'yearly'   = 1,
               1)
    }
    as.year <- function(x) floor(as.numeric(as.yearmon(x)))

    if (!all(gsub("[^A-Z0-9_./]", "", code) == code))
        stop("Codes are comprised of capital letters, numbers and underscores only.")
    ## Build API URL and add auth_token if available
    if (length(code) == 1) {
        codearray <- strsplit(code, "/")
        if (length(codearray[[1]]) == 3) {
            col <- codearray[[1]][3]
            code <- paste(codearray[[1]][1:2], collapse='/')
            params$column <- col
        }
        path <- paste("datasets/", code, sep="")
    }
    else {
        multiset = TRUE
        freqflag = TRUE ## Frequency not automatically supported with multisets
        freq <- 365
        path <- "multisets"
        params$columns <- sub("/",".",code[1])
        for (i in 2:length(code)) {
            params$columns <- paste(params$columns, sub("/",".",code[i]), sep=",")
        }
    }
    if (is.na(authcode)) {
        if (length(grep('TESTS/', code)) != length(code)) warning("It would appear you aren't using an authentication token. Please visit http://www.quandl.com/help/r or your usage may be limited.")
    }
    else
        params$auth_token <- authcode


    ## Add API options
    if (!missing(start_date))
        params$trim_start <- as.Date(start_date)
    if (!missing(end_date))
        params$trim_end <- as.Date(end_date)
    if (type != "raw")
        params$sort_order <- "asc"  
    if (params$collapse %in% c("weekly", "monthly", "quarterly", "annual")) {
        freq   <- frequency2integer(collapse)
        freqflag = TRUE
    }
    params <- c(params, list(...))

    ## Download and parse data
    headers <- basicHeaderGatherer()
    response <- do.call(quandl.api, c(path=path, headers = headers$update, params))
    status <- try(headers$value()[["status"]], silent=TRUE)
    if (inherits(status, 'try-error'))
        stop("I am sorry but Quandl is down for maintenance. Please check the main website for status updates.")
    if (length(grep("403", status)) || length(grep("429", status))) {
        stop(response)
    }
    if(is.na(authcode))
        Quandl.limit(headers$value()[["X-RateLimit-Remaining"]])


    json <- try(fromJSON(response, nullValue = as.numeric(NA)), silent = TRUE)


    ## Error Catching
    if (inherits(json, 'try-error')) {
        print(params)
        print(Quandl.version)
        stop("Something is wrong, and you got past my error catching. Please copy the above output and email to connect@quandl.com")
    }
    if (json["error"] == "Requested entity does not exist." || json["error"] == "Unknown api route.")
        stop("Requested entity does not exist. This could mean the code does not exist or the parameters you have passed have returned an empty dataset.")
    if (length(json$errors) != 0)
      stop(json$errors)
    if (length(json$data) == 0)
        stop("Requested Entity does not exist.")
    if (length(json$column_names) > 100 && multiset)
        stop("Currently we only support multisets with up to 100 columns. Please contact connect@quandl.com if this is a problem.")
    ## Detect frequency
    if (!freqflag)
        freq <- frequency2integer(json$frequency)
    if (!is.null(col) && length(json$column_names) > 2)
        json$column_names = json$column_names[c(1, as.numeric(col)+1)]
    ## Shell data from JSON's list
    data <- tryCatch(as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE),stringsAsFactors=FALSE), 
        warning=function(w) {
            warning(w)
            warning(paste("This warning is most likely the result of a data structure error. If the output of this function does not make sense please email connect@quandl.com with the Quandl code: ", code), call. = FALSE)
            return(suppressWarnings(as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE),stringsAsFactors=FALSE)))
        }
    )
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
    else {
        # Deal with regularly spaced time series first
        if (freq %in% c(1, 4, 12)) {
            # Build regular zoo with correct frequency
            if(freq == 1)
                data_out <- zoo(data[,-1], frequency = freq, as.year(data[,1]))
            else if (freq == 4)
                data_out <- zoo(data[,-1], frequency = freq, as.yearqtr(data[,1]))
            else if (freq == 12)
                data_out <- zoo(data[,-1], frequency = freq, as.yearmon(data[,1]))

            # Convert to type
            if (type == "ts")
                data_out <- as.ts(data_out)
            else if (type == "zoo")
                data_out <- as.zooreg(data_out)
            else if (type == "xts") {
                data_out <- if(freq==1) xts(data[, -1], frequency = 1, order.by=data[, 1]) else as.xts(data_out)
                if (freq != frequency(data_out)) 
                    warning("xts has a non-standard meaning for 'frequency'.")
            }

        }
        # Time series is not regularly spaced
        else if (type=="zoo" || type=="ts") {
            if (type=="ts") warning("Type 'ts' does not support frequency ", freq, ". Returning zoo.")
            data_out <- zoo(data[, -1], order.by=data[, 1])
        }
        else if (type=="xts")
            data_out <- xts(data[, -1], order.by=data[, 1])
    }
    if (meta && !multiset) {
        source_json <- fromJSON(quandl.api(path=paste("sources",json$source_code,sep="/"), auth_token=authcode), nullValue = as.numeric(NA))
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
        attr(data_out, "meta") <- meta
    }
    return(data_out)
}
