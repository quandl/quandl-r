require("zoo")
require("xts")


daily <- Quandl("TESTS/1", type="zoo", meta=TRUE)
dailytoquart <- Quandl("TESTS/1", type="ts", collapse="quarterly")
monthly <- Quandl("TESTS/2")
monthlyts <- Quandl("TESTS/2", type="ts")
monthlyzoo <- Quandl("TESTS/2", type="zoo")
monthlyxts <- Quandl("TESTS/2", type="xts")
quarterly <- Quandl("TESTS/3", type="zoo")
annual <- Quandl("TESTS/4", type="zoo")

if (metaData(daily)$source_code != "TESTS") stop("Meta Data Incorrect")
if (frequency(dailytoquart) != 4) stop("Collapsed Dataset Frequency Incorrect")
if (frequency(monthlyts) != 12) stop("Monthly ts Frequency Incorrect")
if (frequency(monthlyzoo) != 12) stop("Monthly zoo Frequency Incorrect")
if (frequency(monthlyxts) != 12) stop("Monthly xts Frequency Incorrect")

if (max(abs(monthlyts - coredata(monthlyzoo))) > 1e-6 ) stop("Monthly ts and zoo do not compare.")
if (max(abs(coredata(monthlyzoo) - coredata(monthlyxts))) > 1e-6 ) stop("Monthly xts and zoo do not compare.")
