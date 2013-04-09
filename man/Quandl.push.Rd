\name{Quandl.push}
\alias{Quandl.push}
\title{An authentication token is needed to upload data. Set your \code{access_token} with \code{Quandl.auth} function.}
\usage{
  Quandl.push(code, name, desc, data, override = FALSE,
    authcode = Quandl.auth())
}
\arguments{
  \item{code}{Dataset code you would like to upload or
  override, specified as string.}

  \item{name}{Name of dataset you would like to upload or
  override, specified as string.}

  \item{desc}{Description of the dataset you would like to
  upload or override, specified as string.}

  \item{data}{Data you are uploading, specified as a data
  frame.}

  \item{override}{Defaulted to false, if true will rewrite
  the dataset on Quandl with the code given.}

  \item{authcode}{Authentiation Token to identify user by
  dfault set by \code{\link{Quandl.auth}}.}
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
Quandlpush(code="TEST",name="MY test data", desc="This data is test data", data=data)
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

