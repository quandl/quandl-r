R-package
=========

This is Quandl's R Package

License: MIT

For more information please contact raymond@quandl.com

# Installation #

Using the 'devtools' package:

    install.packages("devtools")
    library(devtools)
    install_github('quandl/R-package')

## CRAN ##

To install the most recent package from CRAN type:

    install.packages("Quandl")
    library(Quandl)
    
Note that the version on CRAN might not reflect the most recent changes made to this package.

# Usage #

Once you find the data you'd like to load into R on Quandl, copy the Quandl code from the description box and past it into the function.

    data <- Quandl("NSE/OIL")

To extend your access to the Quandl API, use your authentication token. To do this sign into your account (or create one) and go to your [account info page](https://www.quandl.com/account). Then copy your authentication token and type (with quotes):

    Quandl.auth("authenticationtoken")

This will then extend your usage.


### Example ###
Create a graph of the Nasdaq, with a monthly frequency
	 
    plot(stl(Quandl("GOOG/NASDAQ_GOOG",type="ts",collapse="monthly")[,1],s.window="per"))

    
## Search ##
Searching Quandl from within the R console is now supported. An authorization token is not required, but for extended use specify your token using `Quandl.auth()`.  The search function is:

    Quandl.search(query = "Search Term", page = n, source = "Specific source to search", silent = TRUE|FALSE)

* **Query**: Required; Your search term, as a string
* **Page**: Optional; page number of search you wish returned, defaults to 1.
* **Source**: Optional; Name of a specific source you wish to search, as a string
* **Silent**: Optional; specifies whether you wish the first three results printed to the console, defaults to True (see example below).

Which returns a list containing the following information for every item returned by the search:

* Name
* Quandl code
* Description
* Frequency
* Column names  


###Example###
A search for Oil,  searching only the National Stock Exchange of India (NSE).

	Quandl.search("Oil", source = "NSE")
	
prints:

	Oil India Limited
	Code: NSE/OIL
	Desc: Historical prices for Oil India Limited (OIL), (ISIN: INE274J01014),  National Stock Exchange of India.
	Freq: daily
	Cols: Date|Open|High|Low|Last|Close|Total Trade Quantity|Turnover (Lacs)

	Crude Oil (petroleum) Price
	Code: IMF/POILAPSP_INDEX
	Desc: Crude Oil (petroleum), Price index, 2005 = 100, simple average of three spot prices; Dated Brent, West Texas Intermediate, and the Dubai Fateh
	Freq: monthly
	Cols: Date|Price

	China Crude Oil Consumption
	Code: INDEXMUNDI/ENERGY_CHINA_CRUDEOIL
	Desc: Energy production of Crude Oil in China. Units=Thousand Barrels per Day
	Freq: annual
	Cols: Year|Thousand Barrels per Day


# Additional Resources #
    
More help can be found at [Quandl](http://www.quandl.com) in our [R](http://www.quandl.com/help/r) and [API](http://www.quandl.com/help/api) help pages.
