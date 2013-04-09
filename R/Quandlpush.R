#' An authentication token is needed to upload data. Set your \code{access_token} with \code{Quandl.auth} function.
#' 
#' For instructions on finding your authentication token go to www.quandl.com/API.
#' @param code Dataset code you would like to upload or override, specified as string.
#' @param name Name of dataset you would like to upload or override, specified as string.
#' @param desc Description of the dataset you would like to upload or override, specified as string.
#' @param data Data you are uploading, specified as a data frame.
#' @param override Defaulted to false, if true will rewrite the dataset on Quandl with the code given.
#' @param authcode Authentiation Token to identify user by dfault set by \code{\link{Quandl.auth}}.
#' @return Returns a string url to the newly loaded dataset.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' data <- t(c("2013-01-01",200.5,123.4))
#' data <- data.frame(data)
#' names(data) <- c("Date","Col1","Col2")
#' Quandlpush(code="TEST",name="MY test data", desc="This data is test data", data=data)
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl postForm
#' @export

Quandl.push <- function(code, name, desc, data, override=FALSE, authcode = Quandl.auth()) {

    # Check inputs are proper
    if (missing(code))
        stop("No code indicated")
    if (!nchar(code) == nchar(gsub("[^A-Z0-9_]","",code)))
        stop("Only uppercase letters, numbers, and underscores are permitted in the code")
    if (missing(name))
        stop("No name indicated")
    if (missing(data))
        stop("No data passed as argument")
    if (missing(desc))
        desc = ""
    if (is.na(authcode))
        stop("You are not using an authentication token. Please visit http://www.quandl.com/help/r or this function will not work")
    truefalse = c("true","false")
    override = match(tolower(as.character(override)),truefalse)
    if (is.na(override))
        stop("override is either TRUE or FALSE")
    else
        override = truefalse[override]
    if (!inherits(data,"data.frame"))
        stop("Please pass data as a data frame.")

    # Create url to access API
    url <- paste("http://www.quandl.com/api/v1/datasets.json?auth_token=",authcode,sep="")
    # Make sure dates are formatted correctly.
    data[,1] <- as.Date(data[,1])
    data[,1] <- as.character(data[,1])

    # Build datastring to pass to Quandl
    datastring = paste(paste(names(data),collapse=","),"\n",sep="")

    for (i in 1:(nrow(data)-1)) {
        tempstring <- paste(data[i,],collapse=",")
        datastring <- paste(datastring,tempstring,"\n",sep="")
    }
    tempstring <- paste(data[nrow(data),],collapse=",")
    datastring <- paste(datastring,tempstring,sep="")
    # API Call
    output <- postForm(url, name=name, code=code, description=desc, update_or_create=override, data=datastring)
    # Check if uploaded properly
    json <- fromJSON(output,asText=TRUE)
    if (override == "false") {
        if (try(json$errors$code) == "has already been taken")
            stop("You are trying to overwrite a dataset that already exists on Quandl. If this is what you want to do please call the function again with override=TRUE")
    }
    returnurl <- paste("http://www.quandl.com/",json$source_code,"/",json$code,sep="")
    returnurl
}
