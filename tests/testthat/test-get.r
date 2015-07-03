library(testthat)
library(Quandl)
library(zoo)
library(xts)
library(timeSeries)

Quandl.auth("GgnxpyUBXHsyQxqp67bY")

context("Checking return formats")

test_that("Data is parsed correctly", {
  daily <- Quandl("TESTS/1")
  expect_named(daily, c("Date","Open","High" ,"Low" , "Last", "Close",
                        "Total Trade Quantity", "Turnover (Lacs)"))
  expect_equal(dim(daily), c(258,8))
})

test_that("Metadata is correct", {
  daily <- Quandl("TESTS/1", type="zoo", meta=TRUE)
  expect_false(is.null(attr(daily,"meta")))
  expect_equal(metaData(daily)$source_code, "TESTS")
  expect_equal(metaData(daily)$name, "Daily Dataset Test")
})

test_that("Stop and start dates are correct (zoo)", {
  annual <- Quandl("TESTS/4", type="zoo", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
  expect_equal(start(annual), 1995)
  expect_equal(end(annual), 2005)
})

test_that("Stop and start dates are correct (xts)", {
  annual <- Quandl("TESTS/4", type="xts", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
  expect_equivalent(start(annual), as.Date("1995-12-31"))
  expect_equivalent(end(annual), as.Date("2005-12-31"))
})

test_that("Stop and start dates are correct (timeSeries)", {
  annual <- Quandl("TESTS/4", type="timeSeries", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
  expect_equivalent(start(annual), as.timeDate("1995-12-31"))
  expect_equivalent(end(annual), as.timeDate("2005-12-31"))
})

test_that("Collapsed data frequency", {
  dailytoquart <- Quandl("TESTS/1", type="ts", collapse="quarterly")
  expect_equal(frequency(dailytoquart), 4)
})

test_that("Frequencies are correct across output formats", {
  monthlyts <- Quandl("TESTS/2", type="ts")
  monthlyzoo <- Quandl("TESTS/2", type="zoo")
  monthlyxts <- Quandl("TESTS/2", type="xts")
  monthlytimeSeries <- Quandl("TESTS/2", type="timeSeries")

  expect_equal(frequency(monthlyts), 12)
  expect_equal(frequency(monthlyzoo), 12)
  expect_equal(frequency(monthlyxts), 12)
  # timeSeries allows time index in reverse order but regularity checks won't work then
  # So we check reversed series also
  expect_true((frequency(monthlytimeSeries)==12)||(frequency(rev(monthlytimeSeries))==12))
})

test_that("Data is the same across formats", {
  monthlyraw <- Quandl("TESTS/2", type="raw")
  monthlyts <- Quandl("TESTS/2", type="ts")
  monthlyzoo <- Quandl("TESTS/2", type="zoo")
  monthlyxts <- Quandl("TESTS/2", type="xts")
  monthlytimeSeries <- Quandl("TESTS/2", type="timeSeries")

  expect_equal(max(abs(monthlyts - coredata(monthlyzoo))), 0)
  expect_equal(max(abs(coredata(monthlyzoo) - coredata(monthlyxts))), 0)
  # timeSeries keeps data in same order as passed in, not chronological
  # Have to compare against raw as zoo and xts are sorted chronologically
  expect_equal(max(abs(monthlyraw[,-1] - getDataPart(monthlytimeSeries))), 0)
})

test_that("Output message lists 3 codes", {
  expect_output(str(Quandl.search("gas")), "GasCoin/BITCOIN Price")
  expect_output(str(Quandl.search("oil")), "JODI/OIL_CRPRKB_USA")
})

test_that("Doesn't find anything", {
  expect_warning(Quandl.search("asfdsgfrg"), "we haven't found anything")
})
