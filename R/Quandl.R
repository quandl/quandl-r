library(zoo)
library(XML)
library(xts)

Quandlr <- new.env()


Quandl <- function(code, type="raw",start_date=NULL,end_date=NULL,transformation="",collapse="",authcode=Quandlr$auth_token)
{
    if(is.null(authcode))
    {
        string = paste("http://www.quandl.com/api/v1/datasets/",code,".xml?","&trim_start=1000-01-01&trim_end=1000-01-01",sep="");
        print("It would appear you aren't using an authentication token. Please visit http://www.quandl.com/help/r or your usage may be limited.")
    }
    else
    {
        string = paste("http://www.quandl.com/api/v1/datasets/",code,".xml?&auth_token=",authcode,"&trim_start=1000-01-01&trim_end=1000-01-01",sep="");
    }
    xml = try(xmlRoot(xmlTreeParse(string)),silent=TRUE);
    #Check if code exists
    if(class(xml)[1] == "try-error")
    {
        stop("Code does not exist")
    }
    #Detect frequency
    else
    {
        frequency = xmlSApply(xml[[9]],xmlValue);
    }
    freq = pmatch(frequency,c("annual","quarterly","monthly"),nomatch=10)
    freq = as.integer(2.5*freq^2-4.5*freq+3)
    #Add auth_token if available
    if(is.null(authcode))
    {
        string = paste("http://www.quandl.com/api/v1/datasets/",code,".csv?&sort_order=asc",sep="")
    }
    else
    {
        string = paste("http://www.quandl.com/api/v1/datasets/",code,".csv?&sort_order=asc&auth_token=",authcode,sep="")   
    }
    #API options
    if(!is.null(start_date))
    {
        string = paste(string,"&trim_start=",as.Date(start_date),sep="")
    }
    if(!is.null(end_date))
    {
        string = paste(string,"&trim_end=",as.Date(end_date),sep="")
    }
    if(!is.na(pmatch(transformation,c("diff","rdiff","normalize","cumul"))))
    {
        string = paste(string,"&transformation=",transformation,sep="")
    }
    if(!is.na(pmatch(collapse,c("weekly","monthly","quarterly","annual"))))
    {
        string = paste(string,"&collapse=",collapse,sep="")
        freq = pmatch(collapse,c("annual","quarterly","monthly"),nomatch=10)
        freq = as.integer(2.5*freq^2-4.5*freq+3)
    }
    data = read.csv(string)
    data[,1] = as.Date(data[,1])
    #Build data types
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
        #data = ts(rev(t(data[c(-1)])),start=startdate,frequency=freq)
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
