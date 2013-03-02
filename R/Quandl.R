auth_token <- NULL

Quandl <- function(code, type="raw",start_date=NULL,end_date=NULL,transformation="",collapse="",authcode=auth_token)
{

    frequency2integer <- function(freq) {
        switch(freq,
               'daily'    = 365,
               'monthly'  = 12,
               'quaterly' = 4,
               'yearly'   = 1,
               1)
    }

    ## Check if data is available & grab metadata (although it's one extra API request)
    string = paste("http://www.quandl.com/api/v1/datasets/", code, ".xml?", "&rows=0", sep="")
    if(is.null(authcode))
        warning("It would appear you aren't using an authentication token. Please visit http://www.quandl.com/help/r or your usage may be limited.")
    else
        string = paste(string, "&auth_token=", authcode, sep = "")
    xml = try(xmlRoot(xmlTreeParse(string)),silent=TRUE)

    ## Check if code exists
    if (inherits(xml, 'error'))
        stop("Code does not exist")

    ## Detect frequency
    frequency = xmlSApply(xml[[9]],xmlValue)
    freq      = frequency2integer(frequency)

    ## Build API URL and add auth_token if available
    string = paste("http://www.quandl.com/api/v1/datasets/", code, ".csv?&sort_order=asc", sep="")
    if (!is.null(authcode))
        paste(string, "&auth_token=", authcode, sep = "")

    ## Add API options
    if (!is.null(start_date))
        string = paste(string, "&trim_start=", as.Date(start_date), sep="")
    if (!is.null(end_date))
        string = paste(string,"&trim_end=",as.Date(end_date),sep="")
    if (!transformation %in% c("diff", "rdiff", "normalize", "cumul"))
        string = paste(string,"&transformation=",transformation,sep="")
    if (!collapse %in% c("weekly", "monthly", "quarterly", "annual")) {
        string = paste(string, "&collapse=", collapse, sep="")
        freq   = frequency2integer(collapse)
    }

    ## Fetch data
    data = read.csv(string)
    data[,1] = as.Date(data[,1])

    ## Returning raw data
    if (type == "raw")
        return(data)

    ## Returning ts object
    if (type == "ts")
        return(ts(data[, -1], frequency = freq, start = c(as.POSIXlt(data[1,1])$year+1900, as.POSIXlt(data[1,1])$mon + 1, as.POSIXlt(data[1,1])$mday)))

    ## Returning zoo object
    if (type == "zoo")
        return(zoo(data[c(-1)],data[,1]))

    ## Returning xts object
    if (type == "xts")
        return(xts(data[c(-1)],data[,1]))

    ## Just in case
    stop("Invalid Type")

}
