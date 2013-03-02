auth_token <- NULL

Quandl <- function(code, type="raw",start_date=NULL,end_date=NULL,transformation="",collapse="",authcode=auth_token)
{

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
    freq      = pmatch(frequency,c("annual","quarterly","monthly"),nomatch=10)
    freq      = as.integer(2.5*freq^2-4.5*freq+3)

    ## Build API URL and add auth_token if available
    string = paste("http://www.quandl.com/api/v1/datasets/", code, ".csv?&sort_order=asc", sep="")
    if (!is.null(authcode))
        paste(string, "&auth_token=", authcode, sep = "")

    ## Add API options
    if (!is.null(start_date))
        string = paste(string, "&trim_start=", as.Date(start_date), sep="")
    if (!is.null(end_date))
        string = paste(string,"&trim_end=",as.Date(end_date),sep="")
    if (!is.na(pmatch(transformation, c("diff", "rdiff", "normalize", "cumul"))))
        string = paste(string,"&transformation=",transformation,sep="")
    if (!is.na(pmatch(collapse, c("weekly", "monthly", "quarterly", "annual")))) {
        string = paste(string,"&collapse=",collapse,sep="")
        freq = pmatch(collapse,c("annual","quarterly","monthly"),nomatch=10)
        freq = as.integer(2.5*freq^2-4.5*freq+3)
    }

    ## Fetch data
    data = read.csv(string)
    data[,1] = as.Date(data[,1])

    ## Build data types
    if(type == "raw")
    {
        return(data)
    }
    else if(type == "ts")
    {
        date = data[1,1]
        year = 1900+as.POSIXlt(date)$year
        startdate = 1
        if(freq == 1)
        {
            start = year
        }
        else if (freq == 4)
        {
            quarter = pmatch(quarters(date),c("Q1","Q2","Q3","Q4"))
            startdate = c(year,quarter)
        }
        else if (freq == 12)
        {
            month = 1+as.POSIXlt(date)$mon
            startdate = c(year, month)
        }
        else
        {
            freq = 1
        }
        data = ts(data[c(-1)],start=startdate,frequency=freq)
        return(data)
    }
    else if(type == "zoo")
    {
        data = zoo(data[c(-1)],data[,1])
        return(data)
    }
    else if(type == "xts")
    {
        data = xts(data[c(-1)],data[,1])
        return(data)
    }
    else
    {
        stop("Invalid Type")
    }
}
