% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Quandldatatable.R
\name{Quandl.datatable.bulk_download_to_file}
\alias{Quandl.datatable.bulk_download_to_file}
\title{Downloads a zip with all data requested from a Quandl database}
\usage{
Quandl.datatable.bulk_download_to_file(datatable_code, filename, ...)
}
\arguments{
\item{datatable_code}{Datatable code on Quandl specified as a string.}

\item{filename}{Filename (including path) of file to download.}

\item{...}{Additional named values that are interpreted as Quandl API parameters.}
}
\value{
Returns a data.frame.
}
\description{
Downloads a zip with all data requested from a Quandl database
}
\details{
Set your \code{api_key} with \code{Quandl.api_key} function. For instructions on finding your api key go to \url{https://www.quandl.com/account/profile}
}
\examples{
\dontrun{
Quandl.datatable.bulk_download_to_file('ZACKS/EE')
}
}
\seealso{
\code{\link{Quandl.api_key}}
}
