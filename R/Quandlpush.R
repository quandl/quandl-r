#' THIS FUNCTION IS CURRENTLY DISABLED. SEE http://www.quandl.com/help/toolbelt TO UPLOAD DATA
#'
#' An authentication token is needed to upload data. Set your \code{access_token} with \code{Quandl.auth} function.
#' 
#' For instructions on finding your authentication token go to www.quandl.com/API.
#' @param code Dataset code you would like to create or update, specified as string.
#' @param update Defaults to false, when true overwrites existing datasets on Quandl.
#' @param authcode Authentiation Token to identify user by dfault set by \code{\link{Quandl.auth}}.
#' @param ... Additional named values that are interpretted as api parameters.
#' @return Returns a string url to the newly loaded dataset.
#' @references This R package uses the Quandl API. For more information go to http://www.quandl.com/api.
#' @author Raymond McTaggart
#' @seealso \code{\link{Quandl.auth}}
#' @examples \dontrun{
#' data <- t(c("2013-01-01",200.5,123.4))
#' data <- data.frame(data)
#' names(data) <- c("Date","Col1","Col2")
#' Quandlpush(code="TEST", name="MY test data", description="This data is test data", data=data)
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom RJSONIO toJSON
#' @importFrom RCurl postForm
#' @importFrom RCurl curlOptions
#' @export

Quandl.push <- function(code, update=FALSE, authcode = Quandl.auth(), ...) {
    stop("This function is currently disabled. If you would like to upload data to Quandl please use Quandl Toolbelt at http://www.quandl.com/help/toolbelt")
    postparams <- list(...)
    params <- list()
    source_code <- NULL
    datastring <- NULL
    incode <- code
    # Check inputs are proper
    if (is.na(authcode))
      stop("You are not using an authentication token. Please visit http://www.quandl.com/help/r or this function will not work")
    params$auth_token <- authcode
    if (!nchar(code) == nchar(gsub("[^A-Z0-9_/]","",code)))
        stop("Only uppercase letters, numbers, and underscores are permitted in the code")
    
    path <- paste("datasets", code, sep="/")
    # response <- try(fromJSON(do.call(quandl.api, c(version="v2", path=path, params))), silent = TRUE)
    response <- try(do.call(quandl.api, c(version="v2", path=path, params)), silent = TRUE)
    if (inherits(response, 'try-error'))
      create <- TRUE
    else {
      create <- FALSE
      path <- paste("datasets", response$id, sep="/")
    }
    splitcode = strsplit(code, "/")
    if (length(splitcode[[1]]) == 2) {
      code = splitcode[[1]][2]
      source_code = splitcode[[1]][1]
    }

    if (!create && !update) {
      print("This dataset already exists on Quandl. Do you want to overwrite? (y/n)")
      if (readline() != "y")
        return(NULL)
      print("Pass update=TRUE as a parameter to bypass this error message")
    }  
    if (is.null(postparams$name) && create)
        stop("Missing parameter 'name'")
    if (is.null(postparams$data) && create)
        stop("No data passed as argument")
    # if (!inherits(postparams$data,"data.frame") && create)
    #     stop("Please pass data as a data frame.")
    
    # Format data to string
    if(!is.null(postparams$data)) {
      data = as.data.frame(postparams$data)
      column_names <- paste(names(data),collapse=",")
      # Make sure dates are formatted correctly and check if they are in row names or column 1.
      if (inherits(try(as.Date(rownames(data)), silent=TRUE), 'try-error')) {
        data[,1] <- as.Date(data[,1])
        data[,1] <- as.character(data[,1])
        print_rows <- FALSE
      }
      else {
        column_names = c("Date", column_names)
        print_rows <- TRUE
      }
      # Build datastring to pass to Quandl
      
      datastring <- paste(capture.output(write.table(data, row.names=print_rows, col.names=FALSE, sep=",")), collapse="\n")
      postparams[[which(names(postparams)=="data")]] <- NULL
      if (is.null(postparams$column_names))
        postparams$column_names = column_names
    }
    if (create) {
      # Must Create Dataset
      path <- "datasets"
      postparams$code <- code
      if (!is.null(source_code)) {postparams$source_code <- source_code}
      params$postdata <- postparams
      output <- do.call(quandl.api, c(version="v2", http="POST", path=path, params))
    }
    else {
      # update Data set
      postdata = toJSON(postparams)
      params$postdata <- postdata
      output <- do.call(quandl.api, c(version="v2", http="PUT", path=path, params))
    }
    # Adding Data is an extra call
    if (!is.null(datastring)) {
      json <- output
      postdata <- toJSON(list(data=datastring))
      params$postdata <- postdata
      path <- paste("datasets", json$id, "data", sep="/")
      
      output <- do.call(quandl.api, c(version="v2", http="PUT", path=path, params))
    }
    # Check if uploaded properly
    # json <- fromJSON(output,asText=TRUE)
    
    returnurl <- paste("http://www.quandl.com/",output$source_code,"/",output$code,sep="")
    returnurl
}
