library("zoo")
library("xts")
library("timeSeries")
library("httr")
source("test-helpers.r")

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

context("Quandl() daily data response")
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

context("Quandl() annual collapse response data")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_annual_data())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("return ts when requested", {
    dataset <- Quandl("NSE/OIL", type = "ts", collapse = "annual")
    expect_is(dataset, "ts")
  }),
  test_that("Data is the same across formats", {
    annaulraw <- Quandl("NSE/OIL", type="raw", collapse = "annual")
    annaults <- Quandl("NSE/OIL", type="ts", collapse = "annual")
    annaulzoo <- Quandl("NSE/OIL", type="zoo", collapse = "annual")
    annaulxts <- Quandl("NSE/OIL", type="xts", collapse = "annual")
    annaultimeSeries <- Quandl("NSE/OIL", type="timeSeries")
    expect_equal(max(abs(annaults - coredata(annaulzoo))), 0)
    expect_equal(max(abs(coredata(annaulzoo) - coredata(annaulxts))) , 0)
    # timeSeries keeps data in same order as passed in, not chronological
    # Have to compare against raw as zoo and xts are sorted chronologically
    expect_equal(max(abs(annaulraw[,-1] - getDataPart(annaultimeSeries))), 0)
  }),
  test_that("When requesting xts annual warn user xts has non-standard meaning for frequency", {
    expect_warning(Quandl("NSE/OIL", type="xts", collapse = "annual"), 
      "xts has a non-standard meaning for 'frequency'.", fixed = TRUE)
  })
)

context("Quandl() monthly collapse response data")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_monthly_data())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("Frequencies are correct across output formats", {
    monthlyts <- Quandl("NSE/OIL", type="ts", collapse = "monthly")
    monthlyzoo <- Quandl("NSE/OIL", type="zoo", collapse = "monthly")
    monthlyxts <- Quandl("NSE/OIL", type="xts", collapse = "monthly")
    monthlytimeSeries <- Quandl("NSE/OIL", type="timeSeries", collapse = "monthly")

    expect_equal(frequency(monthlyts), 12)
    expect_equal(frequency(monthlyzoo), 12)
    expect_equal(frequency(monthlyxts), 12)
    # timeSeries allows time index in reverse order but regularity checks won't work then
    # So we check reversed series also
    expect_true((frequency(monthlytimeSeries)==12)||(frequency(rev(monthlytimeSeries))==12))
  })
)

context("Quandl() annual frequency response data")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_annual_frequency_data())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("Start and end dates are correct (zoo)", {
    annual <- Quandl("NSE/OIL", type="zoo")
    expect_equal(start(annual), 1995)
    expect_equal(end(annual), 2005)
  }),
    test_that("Start and end dates are correct (zoo) with force_irregular param set", {
    annual <- Quandl("NSE/OIL", type="zoo", force_irregular = TRUE)
    expect_equal(start(annual), as.Date("1995-12-31"))
    expect_equal(end(annual), as.Date("2005-12-31"))
  }),
  test_that("Start and end dates are correct (xts)", {
    annual <- Quandl("TESTS/4", type="xts")
    expect_that(start(annual), is_equivalent_to(as.Date("1995-12-31")))
    expect_that(end(annual), is_equivalent_to(as.Date("2005-12-31")))
  }),
  test_that("Start and end dates are correct (timeSeries)", {
   annual <- Quandl("TESTS/4", type="timeSeries")
   expect_that(start(annual), is_equivalent_to(as.timeDate("1995-12-31")))
   expect_that(end(annual), is_equivalent_to(as.timeDate("2005-12-31")))
  })
)

context("Quandl() quarterly frequency response data")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_quarterly_collapse_data())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("Collapsed data frequency", {
    dailytoquart <- Quandl("TESTS/1", type="ts", collapse="quarterly")
    expect_equal(frequency(dailytoquart), 4)
  })
)

context('Quandl() filtering by column index')
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in", {
      expect_equal(query$column_index, "1")
    })
    mock_response()
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl("NSE/OIL.1")
)

context('Quandl() multiple datasets')
test_that("Multiple dataset codes with dataset code column indexes are requested", {
  database_codes <- c("NSE", "AAPL")
  dataset_codes <- c("OIL", "WIKI")
  requested_column_indexes <- c("1", "2")
  i <- 0
  with_mock(
    `httr::VERB` = function(http, url, config, body, query) {
      i <<- i + 1
      test_that("request is made with correct params", {
        expected_code <- paste(database_codes[i], dataset_codes[i], sep = "/")
        expect_true(grepl(expected_code, url))
        expect_equal(query$column_index, requested_column_indexes[i])
        expect_equal(query$transform, "rdiff")
      })
      mock_response(content = mock_data(database_code = database_codes[i], dataset_code = dataset_codes[i]))
    },
    `httr::content` = function(response, as = "text") {
      response$content
    },
    Quandl(c("NSE/OIL.1", "AAPL/WIKI.2"), transform = "rdiff", column_index = 3)
  )
  expect_equal(i, 2)
})

test_that("Multiple dataset codes calls merge", {
  database_codes <- c("NSE", "AAPL", "TESTS")
  dataset_codes <- c("OIL", "WIKI", "4")
  i <- 0
  with_mock(
    `httr::VERB` = function(http, url, config, body, query) {
      mock_response(content = mock_data(database_code = database_codes[i], dataset_code = dataset_codes[i]))
    },
    `httr::content` = function(response, as = "text") {
      response$content
    },
    merge = function(data, merge_data, ...) {
      i <<- i + 1
    },
    Quandl(c("NSE/OIL.1", "WIKI/AAPL.2", "TESTS/4.1"))
  )
  expect_equal(i, 2, info = "3 datasets are merged together, merged is called twice")
})

test_that("Multiple dataset codes returns desired requested type", {
  with_mock(
    `httr::VERB` = function(http, url, config, body, query) {
      mock_response(content = mock_annual_data())
    },
    `httr::content` = function(response, as = "text") {
      response$content
    },
    test_that("return type is correct", {
      types <- c('raw', 'ts', 'zoo', 'xts', 'timeSeries')
      expected <- c('data.frame', 'ts', 'zoo', 'xts', 'timeSeries')
      i <- 0
      for(type in types) {
        i <- i + 1
        dataset = Quandl(c("NSE/OIL.1", "WIKI/AAPL.2", "TESTS/4.1"), type = type, collapse = 'annual')
        expect_is(dataset, expected[i])
      }
    })
  )
})

reset_config()
