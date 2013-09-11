\name{Quandl.limit}
\alias{Quandl.limit}
\title{Query remaining API limit}
\usage{
  Quandl.limit(remaining_limit, force_check = FALSE)
}
\arguments{
  \item{remaining_limit}{Optionally passed parameter to
  update Quandl \code{remaining_limit}}

  \item{force_check}{Forces the function to requery Quandl
  for the api limit remaining, could be used after an
  authentication token change.}
}
\value{
  Returns the number of remaining API calls.
}
\description{
  Query remaining API limit
}
\examples{
\dontrun{
Quandl.limit(700)
}
}
\seealso{
  \code{\link{Quandl}}
}

