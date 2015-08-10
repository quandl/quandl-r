source("test-helpers.r")

context("Quandl.search with results")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_search_response())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("Output message lists 3 codes", {
    expect_output(Quandl.search("oil"), "(Code: [A-Z0-9_]+/[A-Z0-9_]+.+){3}")
  })
)


context("Quandl.search with no results")
with_mock(
  `httr::VERB` = function(http, url, config, body, query) {
    mock_response(content = mock_search_empty_response())
  },
  `httr::content` = function(response, as = "text") {
    response$content
  },
  test_that("Doesn't find anything", {
    expect_warning(Quandl.search("asfdsgfrg"), "No datasets found")
  })
)

reset_config()
