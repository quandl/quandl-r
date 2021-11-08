Quandl R Package [![Build Status](https://travis-ci.org/quandl/quandl-r.svg?branch=master)](https://travis-ci.org/quandl/quandl-r)
=========

This is Quandl's R package. The Quandl R package uses the [Quandl API](https://docs.quandl.com). The official Quandl R package manual can be found [here](https://cran.r-project.org/package=Quandl).

License provided by MIT.

For more information please contact raymond@quandl.com

# Installation

To install the [devtools](https://cran.r-project.org/package=devtools) package:

    install.packages("devtools")
    library(devtools)
    install_github("quandl/quandl-r")

## CRAN

To install the most recent package from CRAN type:

    install.packages("Quandl")
    library(Quandl)

Note that the version on CRAN might not reflect the most recent changes made to this package.

# Authentication

To make full use of the package we recommend you set your [api key](https://docs.quandl.com/docs/getting-started#getting-an-api-key). To do this create or sign into your account and go to your [account api key page](https://www.quandl.com/account/profile). Then input your API key (with quotes):

```r
Quandl.api_key("tEsTkEy123456789")
```

# Usage

The Quandl package functions use the Quandl API. Optional Quandl API query parameters can be passed into each function. For more information on supported query parameters, please see the [Quandl API documentation page](https://docs.quandl.com). Once you find the data you would like to load into R on Quandl, copy the Quandl code from the description box and paste it into the function.

```r
data <- Quandl("NSE/OIL")
```

## Graphing Data Example
To create a graph of Google's performance month-over-month:

```r
plot(stl(Quandl("WIKI/GOOG",type="ts",collapse="monthly")[,11],s.window="per"))
```

Note: `collapse` is a Quandl API query parameter. Click [here](https://docs.quandl.com/docs/in-depth-usage#get-time-series-metadata) for a full list of query parameter options.

## Return Types

The supported return types for the `Quandl(code)` function are:
* raw (which returns a data.frame)
* [ts](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ts.html)
* [zoo](https://cran.r-project.org/package=zoo)
* [xts](https://cran.r-project.org/package=xts)
* [timeSeries](https://cran.r-project.org/package=timeSeries)

To request a specific type, assign the `type` argument the return type:

```r
data <- Quandl('NSE/OIL', type = "xts")
```

### Date Formats

zoo, xts, and ts have their own time series date formats. For example:

```r
data <- Quandl('NSE/OIL', collapse = "quarterly", type = "zoo", limit = 3)
```

`data` will have indexes `2015 Q1`, `2015 Q2`, and `2015 Q3`:

```r
         Open  High    Low   Last  Close Total Trade Quantity Turnover (Lacs)
2015 Q1 459.8 462.8 452.45 454.45 454.95               277225         1265.84
2015 Q2 448.0 451.7 445.10 447.80 446.80               352514         1576.93
2015 Q3 456.0 465.0 454.15 456.80 456.75               174154          797.79
```

If you want the time series index to be displayed as dates, you will need to set `force_irregular = TRUE`:

```r
data <- Quandl('NSE/OIL', collapse = "quarterly", type = "zoo", limit = 3, force_irregular = TRUE)
```

`data` will now have indexes `2015-03-31`, `2015-06-30`, and `2015-09-30`:

```r
            Open  High    Low   Last  Close Total Trade Quantity Turnover (Lacs)
2015-03-31 459.8 462.8 452.45 454.45 454.95               277225         1265.84
2015-06-30 448.0 451.7 445.10 447.80 446.80               352514         1576.93
2015-09-30 456.0 465.0 454.15 456.80 456.75               174154          797.79
```

## Merged Dataset Data
If you want to get multiple codes at once, delimit the codes with ',', and put them into an array. This will return a multiset.

```r
merged_data <- Quandl(c('GOOG/NASDAQ_AAPL', 'GOOG/NASDAQ_MSFT'))
```

You can also specify specific columns to retrieve. For example, if you only want column 1 from `GOOG/NASDAQ_AAPL` and column 2 from `GOOG/NASDAQ_MSFT`:

```r
merged_data <- Quandl(c('GOOG/NASDAQ_AAPL.1', 'GOOG/NASDAQ_MSFT.2'))
```

## Downloading Entire Database

An entire database's data can be downloaded. For example, to download the database `ZEA`:

```r
Quandl.database.bulk_download_to_file("ZEA", "./ZEA.zip")
```

Note you must set your [api key](#authentication) to download [premium databases](https://www.quandl.com/search?type=premium) to which you are subscribed.

For a full list of optional query parameters for downloading an entire database, click [here](https://docs.quandl.com/docs/parameters-2).

## Datatables

To retrieve Datatable data, provide a Datatable code to the Quandl datatables function:

```r
data = Quandl.datatable('ZACKS/FC')
```

The output format is `data.frame`. Given the volume of data stored in datatables, this call will retrieve the first page of the ZACKS/FC datatable. You may turn on pagination to return more data by using:

```r
data = Quandl.datatable('ZACKS/FC', paginate=TRUE)
```

This will retrieve multiple pages of data and merge them together as if they were one large page. In some cases, however, you will still exceed the request limit. In this case we recommend you filter your data using the available query parameters, as in the following example:

```r
Quandl.datatable('ZACKS/FC', ticker=c('AAPL', 'MSFT'), per_end_date.gt='2015-01-01', qopts.columns=c('ticker', 'per_end_date', 'tot_revnu'))
```

In this query we are asking for more pages of data, ticker values of either AAPL or MSFT and a per_end_date that is greater than or equal to 2015-01-01. We are also filtering the returned columns on ticker, per_end_date and tot_revnu rather than all available columns.

## Search
Searching Quandl from within the R console is now supported. The search function is:

```r
Quandl.search(query = "Search Term", page = n, database_code = "Specific database to search", silent = TRUE|FALSE)
```

* **query**: Required; Your search term, as a string
* **page**: Optional; page number of search you wish returned, defaults to 1.
* **per_page**: Optional; number of results per page, defaults to 10 in the Quandl R package.
* **database_code**: Optional; Name of a specific source you wish to search, as a string
* **silent**: Optional; specifies whether you wish the first three results printed to the console, defaults to True (see example below).

Which outputs to console a list containing the following information for every item returned by the search:

* Name
* Quandl code
* Description
* Frequency
* Column names


### Example
A search for Oil,  searching only the National Stock Exchange of India (NSE).

```r
Quandl.search("Oil", database_code = "NSE", per_page = 3)
```

prints:

```r
Oil India Limited
Code: NSE/OIL
Desc: Historical prices for Oil India Limited<br><br>National Stock Exchange of India<br><br>Ticker: OIL<br><br>ISIN: INE274J01014
Freq: daily
Cols: Date | Open | High | Low | Last | Close | Total Trade Quantity | Turnover (Lacs)

Oil Country Tubular Limited
Code: NSE/OILCOUNTUB
Desc: Historical prices for Oil Country Tubular Limited<br><br>National Stock Exchange of India<br><br>Ticker: OILCOUNTUB<br><br>ISIN: INE591A01010
Freq: daily
Cols: Date | Open | High | Low | Last | Close | Total Trade Quantity | Turnover (Lacs)

Gulf Oil Corporation Limited
Code: NSE/GULFOILCOR
Desc: Historical prices for Gulf Oil Corporation Limited (GULFOILCOR), (ISIN: INE077F01027),  National Stock Exchange of India.
Freq: daily
Cols: Date | Open | High | Low | Last | Close | Total Trade Quantity | Turnover (Lacs)
```

## Point in Time

PointInTime works similarly to datatables but filtering the data based on dates. For example, a simple way to retrieve datatable information for a specific date would be:

```r
Quandl.pit.asofdate('DATABASE/CODE', '2020-01-01')
```

## Date Format

Dates passed to `Quandl.pit` calls must be a valid `ISO 8601` datetime. For example, the follow values are valid dates:

- `2021-03-02`
- `2021-03-02T13:45:00`
- `2021-03-02T12:55:00-05:00`

While the following are invalid:

- `2021-03-02 13:45:00` (missing `T` between date and time)
- `March 2nd, 2021` (not `ISO 8601` compliant)

### Available functions

| Interval | Explanation | Required params | Example |
|----------|-------------|-----------------|---------|
| asofdate | Returns data as of a specific date | date | `Quandl.pit.asofdate('DATABASE/CODE', 'yyyy-mm-dd', ...)` |
| fromto | Returns data from `start` up to but excluding `end`; [start, end) | start_date, end_date  | `Quandl.pit.fromto('DATABASE/CODE', '2020-01-01', '2020-02-01', ...)` |
| between | Returns data inclusively between dates; [start, end] | start_end, end_date  | `Quandl.pit.between('DATABASE/CODE', '2019-01-01', '2020-01-31', ...)` |

### Additional Examples

Filter Point in Time records for specific columns:

```r
Quandl.pit.asofdate('DATABASE/CODE', '2020-01-01', qopts.columns=c('x', 'y', 'z'))
Quandl.pit.fromto('DATABASE/CODE', '2020-01-01', '2020-02-01', qopts.columns=c('x', 'y', 'z'))
Quandl.pit.between('DATABASE/CODE', '2020-01-01', '2020-01-31', qopts.columns=c('x', 'y', 'z'))
```

## Additional Resources

More help can be found at [Quandl](https://www.quandl.com) in our [R](https://www.quandl.com/tools/r) and [API](https://docs.quandl.com) pages.
