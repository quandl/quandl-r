R-package
=========

This is Quandl's R Package

License: GPL-2

For more information please contact raymond@quandl.com

# Installation

    git clone git://github.com/quandl/R-package.git
    R CMD build R-package/

This will create a file named "Quandl_1.5.tar.gz". The current version of Quandl on github is "1.5"

Then move the file into your working direcotry in R and type:

    > install.packages("Quandl_1.5.tar.gz",repos=NULL,type="source")
    > library(Quandl)

A simpler solution is to use the 'devtools' package.

    > install.packages("devtools")
    > library(devtools)
    > install_github('R-package','quandl')

## CRAN

To install the most recent package from CRAN type:

    > install.packages("Quandl")
    > library(Quandl)
    
Note that the version on CKAN might not reflect the most recent changes made to this package.

# Usage

Once you find the data you'd like to load into R on Quandl, copy the Quandl code from the description box and past it into the function.

    > data <- Quandl("NSE/OIL")

To extend your access to the Quandl API, use your authentication token. To do this sign into your account (or create one) and go to the API tab under in your account page. Then copy your authentication token and type(with quotes):

    > Quandl.auth("authenticationtoken")

This will then extend your usage.

# Examples

    > plot(stl(Quandl("GOOG/NASDAQ_GOOG",type="ts",collapse="monthly")[,1],s.window="per"))

# Additional Resources
    
More help can be found at [Quandl](http://www.quandl.com) in our [R](http://www.quandl.com/help/r) and [API](http://www.quandl.com/api) help pages.
