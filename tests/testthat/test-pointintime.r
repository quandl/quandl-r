library("httr")
source("test-datatable-helper.r")
source("test-helpers.r")

context("Getting PIT Datatable data")

context("Quandl.pit.fromto() call")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when dates used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/from/2020-01-01/to/2020-01-03")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.fromto("RSM/MSB", "2020-01-01", "2020-01-03")
)
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when datetimes used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/from/2020-01-01T12:00/to/2020-01-03T14:00")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.fromto("RSM/MSB", "2020-01-01T12:00", "2020-01-03T14:00")
)


context("Quandl.pit.between() call")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when dates used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/between/2020-01-01/2020-01-03")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.between("RSM/MSB", "2020-01-01", "2020-01-03")
)
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed when datetimes used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/between/2020-01-01T12:00/2020-01-03T14:00")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.between("RSM/MSB", "2020-01-01T12:00", "2020-01-03T14:00")
)

context("Quandl.pit.asofdate() call")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when dates used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/asofdate/2020-01-01")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.asofdate("RSM/MSB", "2020-01-01")
)
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when datetimes used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/asofdate/2020-01-01T12:00")
      expect_null(body)
      expect_equal(query, list())
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.asofdate("RSM/MSB", "2020-01-01T12:00")
)

context("Quandl.pit.fromto() call with options")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when dates used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/from/2020-01-01/to/2020-01-03")
      expect_null(body)
      expect_equal(query, list('ticker[]'='AAPL', 'ticker[]'='MSFT',
                               'qopts.columns[]'='ticker', 'qopts.columns[]'='per_end_date',
                               'qopts.columns[]'='tot_revnu'))
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.fromto("RSM/MSB", "2020-01-01", "2020-01-03", ticker=c('AAPL', 'MSFT'),
                               qopts.columns=c('ticker','per_end_date','tot_revnu'),
                    paginate=FALSE)
)
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    test_that("correct arguments are passed in when datetimes used", {
      expect_equal(http, "GET")
      expect_equal(url, "https://www.quandl.com/api/v3/pit/RSM/MSB/from/2020-01-01T12:00/to/2020-01-03T14:00")
      expect_null(body)
      expect_equal(query, list('ticker[]'='AAPL', 'ticker[]'='MSFT',
                               'qopts.columns[]'='ticker', 'qopts.columns[]'='per_end_date',
                               'qopts.columns[]'='tot_revnu'))
    })
    mock_response(content = mock_datatable_data())
  },
  `httr::content` = function(response, as="text") {
    response$content
  },
  Quandl.pit.fromto("RSM/MSB", "2020-01-01T12:00", "2020-01-03T14:00", ticker=c('AAPL', 'MSFT'),
                    qopts.columns=c('ticker','per_end_date','tot_revnu'),
                    paginate=FALSE)
)

reset_config()
