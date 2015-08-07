library("zoo")
library("xts")
library("timeSeries")
library("httr")

reset_config <- function() {
  Quandl.api_key(NULL)
  Quandl.api_version(NULL)
}

mock_content <- function() {
  "{
    \"dataset\":{
       \"id\":6668,
       \"dataset_code\":\"OIL\",
       \"database_code\":\"NSE\",
       \"name\":\"Oil India Limited\",
       \"description\":\"Historical\",
       \"refreshed_at\":\"2015-08-07T02:37:20.453Z\",
       \"newest_available_date\":\"2015-08-06\",
       \"oldest_available_date\":\"2009-09-30\",
       \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
       \"frequency\":\"daily\",
       \"type\":\"Time Series\",
       \"premium\":false,\"limit\":2,
       \"transform\":null,
       \"column_index\":null,
       \"start_date\":\"2009-09-30\",
       \"end_date\":\"2015-08-06\",
       \"data\":[[\"2015-08-06\",450.9,460.7,447.3,454.8,456.4,339324.0,1542.22],[\"2015-08-05\",440.5,454.0,439.05,450.2,449.4,287698.0,1286.17]],
       \"collapse\":null,
       \"order\":\"desc\",
       \"database_id\":33
    }
  }"
}

mock_annual_content <- function() {
  "{
    \"dataset\":{
      \"id\":6668,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"NSE\",
      \"name\":\"Oil India Limited\",
      \"description\":\"Historical prices for Oil India Limited\",
      \"refreshed_at\":\"2015-08-07T02:37:20.453Z\",
      \"newest_available_date\":\"2015-08-06\",
      \"oldest_available_date\":\"2009-09-30\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"limit\":3,
      \"transform\":null,
      \"column_index\":null,
      \"start_date\":\"2009-09-30\",
      \"end_date\":\"2015-08-06\",
      \"data\":[[\"2015-12-31\",450.9,460.7,447.3,454.8,456.4,339324.0,1542.22],[\"2014-12-31\",565.0,579.0,565.0,578.9,576.4,212525.0,1220.73],[\"2013-12-31\",484.05,492.0,475.1,488.2,488.35,359499.0,1741.57]],
      \"collapse\":\"annual\",
      \"order\":\"desc\",
      \"database_id\":33
    }
  }"
}

mock_response <- function(status_code = 200, content = mock_content()) {
  httr:::response(
    status_code = status_code,
    content = content
  )
}

context("Getting Dataset data")

context("Quandl() bad argument errors")
test_that("Invalid transform throws error", {
  expect_error(Quandl("NSE/OIL", transform = "blah"))
})

test_that("Invalid collapse throws error", {
  expect_error(Quandl("NSE/OIL", collapse = "blah"))
})

test_that("Invalid type throws error", {
  expect_error(Quandl("NSE/OIL", type = "blah"))
})

context("Quandl() call")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/datasets/NSE/OIL")
      expect_is(config, "request")
      expect_null(body)
      expect_equal(query, list(transform = "rdiff", collapse = "annual", 
                               order = "desc", start_date = "2015-01-01"))
    })
    mock_response()
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl("NSE/OIL", transform = "rdiff", collapse = "annual", start_date = "2015-01-01")
)

context("Quandl() response")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response()
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  test_that("list names are set to column names", {
    dataset <- Quandl("NSE/OIL")
    expect_named(dataset, c("Date","Open","High" ,"Low" , "Last", "Close",
                            "Total Trade Quantity", "Turnover (Lacs)"))
  }),
  test_that("returned data is a dataframe", {
    dataset <- Quandl("NSE/OIL")
    expect_is(dataset, 'data.frame')
  }),
  test_that("does not contain meta attribute by default", {
    dataset <- Quandl("NSE/OIL")
    expect_null(attr(dataset, 'meta'))
  }),
  test_that("does contain meta attribute when requested", {
    dataset <- Quandl("NSE/OIL", meta = TRUE)
    expect_true(!is.null(attr(dataset, "meta")))
    expect_equal(attr(dataset, "meta")$id, 6668)
    expect_equal(attr(dataset, "meta")$database_code, "NSE")
    expect_equal(attr(dataset, "meta")$dataset_code, "OIL")
  }),
  test_that("zoo is returned when requested", {
    dataset <- Quandl("NSE/OIL", type = "zoo")
    expect_is(dataset, "zoo")
  }),
  test_that("xts is returned when requested", {
    dataset <- Quandl("NSE/OIL", type = "xts")
    expect_is(dataset, "xts")
  }),
  test_that("timeSeries is returned when requested", {
    dataset <- Quandl("NSE/OIL", type = "timeSeries")
    expect_is(dataset, "timeSeries")
  }),
  test_that("zoo is returned instead of ts if ts is not supported for frequency", {
    dataset <- Quandl("NSE/OIL", type = "ts")
    expect_is(dataset, "zoo")
  }),
  test_that("display warning message if type ts is not supported by frequency", {
    expect_warning(Quandl("NSE/OIL", type = "ts"),
      "Type 'ts' does not support frequency 365. Returning zoo.", fixed = TRUE)
  })
)

context("Request ts with supported frequency")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_annual_content())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("return ts when requested", {
    dataset <- Quandl("NSE/OIL", type = "ts", collapse = "annual")
    expect_is(dataset, "ts")
  })
)

# test_that("Stop and start dates are correct (zoo)", {
#   annual <- Quandl("TESTS/4", type="zoo", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
#   expect_that(start(annual), equals(1995))
#   expect_that(end(annual), equals(2005))
# })

# test_that("Stop and start dates are correct (xts)", {
#   annual <- Quandl("TESTS/4", type="xts", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
#   expect_that(start(annual), is_equivalent_to(as.Date("1995-12-31")))
#   expect_that(end(annual), is_equivalent_to(as.Date("2005-12-31")))
# })

# test_that("Stop and start dates are correct (timeSeries)", {
#   annual <- Quandl("TESTS/4", type="timeSeries", start_date="1995-01-01", end_date=as.Date("2006-01-01"))
#   expect_that(start(annual), is_equivalent_to(as.timeDate("1995-12-31")))
#   expect_that(end(annual), is_equivalent_to(as.timeDate("2005-12-31")))
# })

# test_that("Collapsed data frequency", {
#   dailytoquart <- Quandl("TESTS/1", type="ts", collapse="quarterly")
#   expect_that(frequency(dailytoquart), equals(4))
# })

# test_that("Frequencies are correct across output formats", {
#   monthlyts <- Quandl("TESTS/2", type="ts")
#   monthlyzoo <- Quandl("TESTS/2", type="zoo")
#   monthlyxts <- Quandl("TESTS/2", type="xts")
#   monthlytimeSeries <- Quandl("TESTS/2", type="timeSeries")
#   expect_that(frequency(monthlyts), equals(12))
#   expect_that(frequency(monthlyzoo), equals(12))
#   expect_that(frequency(monthlyxts), equals(12))
#   # timeSeries allows time index in reverse order but regularity checks won't work then
#   # So we check reversed series also
#   expect_true((frequency(monthlytimeSeries)==12)||(frequency(rev(monthlytimeSeries))==12))
# })

# test_that("Data is the same across formats", {
#   monthlyraw <- Quandl("TESTS/2", type="raw")
#   monthlyts <- Quandl("TESTS/2", type="ts")
#   monthlyzoo <- Quandl("TESTS/2", type="zoo")
#   monthlyxts <- Quandl("TESTS/2", type="xts")
#   monthlytimeSeries <- Quandl("TESTS/2", type="timeSeries")
#   expect_that(max(abs(monthlyts - coredata(monthlyzoo))), equals(0))
#   expect_that(max(abs(coredata(monthlyzoo) - coredata(monthlyxts))) , equals(0))
#   # timeSeries keeps data in same order as passed in, not chronological
#   # Have to compare against raw as zoo and xts are sorted chronologically
#   expect_that(max(abs(monthlyraw[,-1] - getDataPart(monthlytimeSeries))), equals(0))
# })

# test_that("Output message lists 3 codes", {
#   expect_output(Quandl.search("gas"), "(Code: [A-Z0-9_]+/[A-Z0-9_]+.+){3}")
# })

# test_that("Doesn't find anything", {
#   expect_warning(Quandl.search("asfdsgfrg"), "No datasets found")
# })

reset_config()
