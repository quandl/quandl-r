\name{Quandl}
\alias{Quandl}
\title{Quandl API}
\description{Pulls Data from the Quandl API.}
\usage{Quandl(code, type = "raw", start_date = NULL, end_date = NULL, transformation = "", collapse = "",authcode=Quandlr$auth_token)}
\arguments{
  \item{code}{Dataset code on Quandl.}
  \item{type}{Type of data returned. Can be [raw],[ts],[zoo],or[xts].}
  \item{start_date}{Use to truncate data by start date. [yyyy-mm-dd]}
  \item{end_date}{Use to truncate data by end date.[yyyy-mm-dd]}
  \item{transformation}{Apply Quandl API data transformations.}
  \item{collapse}{Collapse frequency of Data.}
  \item{authcode}{Authentication Token for extended API access.}
}
\value{
Depending on the outpug flag the class is either data.frame, time series, xts, or zoo
\item{data}{Named data matrix with dimensions depending on the dataset.}
}
\references{This R package uses the Quandl API. For more information go to http://www.quandl.com/api. For more help on the package itself go to http://www.quandl.com/help/r.}
\author{Raymond McTaggart}
\examples{
\dontrun{quandldata = Quandl("NSE/OIL",collapse="monthly",start_date="2013-01-01",type="ts")
plot(quandldata[,1])}
}

\keyword{quandl}
\keyword{api}
\keyword{data}
