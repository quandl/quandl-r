source("test-helpers.r")

context('Test api error handling')

context('quandl.api')
with_mock(
  `httr::content` = function(response, as="text") {
    "{}"
  },
  `httr::VERB` = function(http, url, config, body, query) {
    httr:::response(status_code = 500)
  },
  test_that('When status code is not 200 error is thrown', {
    expect_error(quandl.api('datasets'))
  })
)

context('quandl.api.download_file')
with_mock(
  `httr::content` = function(response, as="text") {
    "{}"
  },
  `httr::GET` = function(url, config, query, ...) {
    httr:::response(status_code = 403)
  },
  test_that('When status code is not 200 error is thrown', {
    expect_error(quandl.api.download_file('datasets', 'foobar'))
  })
)

context('quandl.api.build_request')
test_that('request headers and query params are constructed', {
  Quandl.api_key('test_key')
  Quandl.api_version('2015-04-09')
  path <- 'datasets'
  results <- quandl.api.build_request(path, start_date = '2015-01-01', end_date = as.Date('2015-02-01'))
  expected_params <- list(start_date = '2015-01-01', end_date = '2015-02-01')
  expected_headers <- list(
      Accept = 'application/json, application/vnd.quandl+json;version=2015-04-09',
      `Request-Source` = 'R',
      `Request-Source-Version` = '2.7.0',
      `X-Api-Token` = 'test_key'
    )
  expected_url <- "https://www.quandl.com/api/v3/datasets"
  expect_equal(results, list(request_url = expected_url, headers = expected_headers, params = expected_params))
})

reset_config()
