\name{quandl.api}
\alias{quandl.api}
\title{Pulls Data from the Quandl API}
\usage{
  quandl.api(version = "v1", path, headers = NULL,
    http = c("GET", "PUT", "POST", "DELETE"), ...)
}
\arguments{
  \item{version}{Set to the version of the Quandl API you
  want to access.}

  \item{path}{Path to api resource.}

  \item{headers}{Header function to collect header info.}

  \item{http}{Type of http request sent.}

  \item{...}{Named values that are interpretted as api
  parameters.}
}
\value{
  Website response.
}
\description{
  An authentication token is needed for access to the
  Quandl API multiple times. Set your \code{access_token}
  with \code{Quandl.auth} function.
}
\details{
  For instructions on finding your authentication token go
  to www.quandl.com/API
}
\examples{
\dontrun{
quandldata = quandl.api(version="v1", path="datasets/NSE/OIL", http="GET")
plot(quandldata[,1])
}
}
\author{
  Raymond McTaggart
}
\references{
  This R package uses the Quandl API. For more information
  go to http://www.quandl.com/api. For more help on the
  package itself go to http://www.quandl.com/help/r.
}
\seealso{
  \code{\link{Quandl.auth}}
}

