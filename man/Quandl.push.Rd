\name{Quandl.push}
\alias{Quandl.push}
\title{An authentication token is needed to upload data. Set your \code{access_token} with \code{Quandl.auth} function.}
\usage{
  Quandl.push(code, update = FALSE,
    authcode = Quandl.auth(), ...)
}
\arguments{
  \item{code}{Dataset code you would like to create or
  update, specified as string.}

  \item{update}{Defaults to false, when true overwrites
  existing datasets on Quandl.}

  \item{authcode}{Authentiation Token to identify user by
  dfault set by \code{\link{Quandl.auth}}.}

  \item{...}{Additional named values that are interpretted
  as api parameters.}
}
\value{
  Returns a string url to the newly loaded dataset.
}
\description{
  For instructions on finding your authentication token go
  to www.quandl.com/API.
}
\examples{
\dontrun{
data <- t(c("2013-01-01",200.5,123.4))
data <- data.frame(data)
names(data) <- c("Date","Col1","Col2")
Quandlpush(code="TEST", name="MY test data", description="This data is test data", data=data)
}
}
\author{
  Raymond McTaggart
}
\references{
  This R package uses the Quandl API. For more information
  go to http://www.quandl.com/api.
}
\seealso{
  \code{\link{Quandl.auth}}
}

