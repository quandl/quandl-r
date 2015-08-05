source("test-helpers.r")

Quandl.api_key('test_key')
Quandl.api_version('2015-04-09')

test_that("download database url is constructed correctly", {
  expected <- "https://www.quandl.com/api/v3/databases/NSE/data?api_key=test_key&api_version=2015-04-09&download_type=partial"
  expect_equal(Quandl.database.bulk_download_url("NSE", download_type = "partial"), expected)
})

with_mock(
  # this is not a folder
  dir.exists = function(paths) {
    FALSE
  },
  test_that("bulk download errors when invalid filepath given", {
    expect_error(Quandl.database.bulk_download_to_file("NSE", "path/does/not/exist"), "path/does/not directory does not exist!", fixed = TRUE)
  })
)

with_mock(
  dir.exists = function(paths) {
    TRUE
  },
  test_that("bulk download errors when invalid filepath given", {
    expect_error(Quandl.database.bulk_download_to_file("NSE", "path/is/folder"), "Please add a filename to your directory path, e.g., path/is/folder/NSE.zip", fixed = TRUE)
  })
)

with_mock(
  dir.exists = function(paths) {
    if (paths == "folder/exists/NSE.zip") {
      return(FALSE)
    }
    return(TRUE)
  },
  `Quandl:::quandl.api.download_file` = function(path, filename, ...) {
    test_that("correct arguments are passed to api layer", {
      params <- list(...)
      expect_equal(params$download_type, "partial")
      expect_equal(path, "databases/NSE/data")
      expect_equal(filename, "folder/exists/NSE.zip")
    })
  },
  Quandl.database.bulk_download_to_file("NSE", "folder/exists/NSE.zip", download_type = "partial")
)

reset_config()