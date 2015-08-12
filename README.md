Quandl R Package
=========

This is Quandl's R package. The Quandl R package uses the [Quandl API](https://www.quandl.com/docs/api). The official Quandl R package manual can be found [here](https://cran.r-project.org/web/packages/Quandl/index.html).

License: MIT

For more information please contact raymond@quandl.com

# Installation

Using the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package:

    install.packages("devtools")
    library(devtools)
    install_github("quandl/quandl-r")

## CRAN

To install the most recent package from CRAN type:

    install.packages("Quandl")
    library(Quandl)
    
Note that the version on CRAN might not reflect the most recent changes made to this package.

# Authentication

To extend your access to the Quandl API, use your [api key](https://www.quandl.com/docs/api#api-keys). To do this sign into your account (or create one) and go to your [account api key page](https://www.quandl.com/account/api). Then input your api key (with quotes):

```r
Quandl.api_key("tEsTkEy123456789")
```

This will then extend your usage.

# Usage

The Quandl package functions use the Quandl API. Optional Quandl API query parameters can be passed into each function. For more information on supported query parameters, please see the [Quandl API documentation page](https://www.quandl.com/docs/api). Once you find the data you would like to load into R on Quandl, copy the Quandl code from the description box and paste it into the function.

```r
data <- Quandl("NSE/OIL")
```

To reset the package to not use an api key:

```r
Quandl.api_key(NULL)
```

## Graphing Data Example
To create a graph of the Nasdaq, with a monthly frequency

```r
plot(stl(Quandl("GOOG/NASDAQ_GOOG",type="ts",collapse="monthly")[,1],s.window="per"))
```

Note: `collapse` is a Quandl API query parameter. Click [here](https://www.quandl.com/docs/api#retrieve-data-and-metadata) for a full list of query parameter options.  

## Return Types

The supported return types for the `Quandl(code)` function are:
* raw (which returns a data.frame)
* [ts](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ts.html)
* [zoo](https://cran.r-project.org/web/packages/zoo/index.html)
* [xts](https://cran.r-project.org/web/packages/xts/index.html)
* [timeSeries](https://cran.r-project.org/web/packages/timeSeries/index.html)

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
To get a merged representation of multiple Quandl code data, specify a vector of Quandl codes:

```r
merged_data <- Quandl(c('GOOG/NASDAQ_AAPL', 'GOOG/NASDAQ_MSFT'))
```

You can also specify specific columns to retrieve. For example, if you only want column 1 from `GOOG/NASDAQ_AAPL` and column 2 from `GOOG/NASDAQ_MSFT`:

```r
merged_data <- Quandl(c('GOOG/NASDAQ_AAPL.1', 'GOOG/NASDAQ_MSFT.2'))
```

## Downloading Entire Database

An entire Database's data can be downloaded. For example, to download database `ZEA`:

```r
Quandl.database.bulk_download_to_file("ZEA", "./ZEA.zip")
```

Please set your [api key](#authentication) to download [premium databases](https://www.quandl.com/search?type=premium) you are subscribed to.

For a full list of optional query parameters for downloading an entire database, click [here](https://www.quandl.com/docs/api#entire-database).
    
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


# Additional Resources
    
More help can be found at [Quandl](https://www.quandl.com) in our [R](https://www.quandl.com/help/r) and [API](https://www.quandl.com/docs/api) pages.
