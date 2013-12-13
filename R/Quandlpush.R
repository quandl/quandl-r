#' An authentication token is needed to upload data. Set your \code{access_token} with \code{Quandl.auth} function.
#' 
#' For instructions on finding your authentication token go to www.quandl.com/API.
#' @param code Dataset code you would like to create or update, specified as string.
#' @param username Used to identify user source.
#' @param source_code Source code to upload dataset under.
#' @param update Defaults to false, when true overwrites existing datasets on Quandl.
#' @param authcode Authentiation Token to identify user by dfault set by \code{\link{Quandl.auth}}.
#' @return Returns a string url to the newly loaded dataset.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' data <- t(c("2013-01-01",200.5,123.4))
#' data <- data.frame(data)
#' names(data) <- c("Date","Col1","Col2")
#' Quandlpush(code="TEST", username="someone", name="MY test data", description="This data is test data", data=data)
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom RJSONIO toJSON
#' @importFrom RCurl postForm
#' @importFrom RCurl curlOptions
#' @export

Quandl.push <- function(code, username=NULL, source_code=NULL, update=FALSE, authcode = Quandl.auth(), ...) {
    params <- list(...)
    # Check inputs are proper
    if (is.na(authcode))
      stop("You are not using an authentication token. Please visit http://www.quandl.com/help/r or this function will not work")
    if (missing(code))
        stop("No code indicated")
    if (!nchar(code) == nchar(gsub("[^A-Z0-9_]","",code)))
        stop("Only uppercase letters, numbers, and underscores are permitted in the code")
    splitcode = strsplit(code, "/")
    if (length(splitcode[[1]]) == 2) {
      code = splitcode[[1]][2]
      source_code = splitcode[[1]][1]
    }
    if (is.null(username) && is.null(source_code))
      stop("Please enter your username or your user source code.")
    if(is.null(source_code)) {
      response <- fromJSON(getURL(paste("http://www.quandl.com/api/v1/sources.json?auth_token=",authcode, "&query=", username, "&code=USER_", sep="")), asText=TRUE)
      source_code <- response$docs[[1]]$code
      if(is.null(source_code))
        stop("You need your user source code. Please email connect@quandl.com for assistance.")
    }
    response <- try(Quandl(paste(source_code, "/", code, sep=""), rows=1), silent=TRUE)
    create = inherits(response, 'try-error')
    if (!create && !update) {
      print("This dataset already exists on Quandl. Do you want to overwrite? (y/n)")
      if (readline() != "y")
        return(NULL)
      print("Pass update=TRUE as a parameter to bypass this error message")
    }  
    if (is.null(params$name) && create)
        stop("Missing parameter 'name'")
    if (is.null(params$data) && create)
        stop("No data passed as argument")
    if (!inherits(params$data,"data.frame") && create)
        stop("Please pass data as a data frame.")
    
    # Create url to access API
    url <- paste("http://www.quandl.com/api/v1/datasets.json?auth_token=",authcode,sep="")
    if(!is.null(params$data)) {
        data = params$data
        # Make sure dates are formatted correctly.
        data[,1] <- as.Date(data[,1])
        data[,1] <- as.character(data[,1])

        # Build datastring to pass to Quandl
        column_names = paste(names(data),collapse=",")
        datastring = ""
        if (nrow(data) > 1) {
          for (i in 1:(nrow(data)-1)) {
              tempstring <- paste(data[i,],collapse=",")
              datastring <- paste(datastring,tempstring,"\n",sep="")
          }
        }
        tempstring <- paste(data[nrow(data),],collapse=",")
        datastring <- paste(datastring,tempstring,sep="")
        params$data = datastring
        if (is.null(params$column_names))
          params$column_names = column_names
    }
    
    if (create) {
        # Must Create Dataset
        output <- postForm(url, .params=c(code=code, params), .opts=curlOptions(verbose=TRUE))
    }
    else {
         
        # update Data set
        postdata = toJSON(params)
        post_length = nchar(postdata, type="bytes")
        url <- paste("http://www.quandl.com/api/v1/datasets/", source_code, "/", code, ".json?auth_token=",authcode,sep="")
        output <- getURL(url, customrequest="PUT", httpheader=c("Content-Length"=post_length, "Content-Type"="application/json"), postfields=postdata, verbose=TRUE)
    }

    # Check if uploaded properly
    json <- fromJSON(output,asText=TRUE)
    
    returnurl <- paste("http://www.quandl.com/",json$source_code,"/",json$code,sep="")
    returnurl
}
