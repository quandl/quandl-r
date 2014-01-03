require("zoo")
require("xts")


context("Checking return formats")



# monthly <- Quandl("TESTS/2")

# quarterly <- Quandl("TESTS/3", type="zoo")
# annual <- Quandl("TESTS/4", type="zoo")


test_that("metadata is correct", {
  daily <- Quandl("TESTS/1", type="zoo", meta=TRUE)
  expect_that(metaData(daily)$source_code,
    equals("TESTS"))


})

test_that("Collapsed data frequency", {
  dailytoquart <- Quandl("TESTS/1", type="ts", collapse="quarterly")
  expect_that(frequency(dailytoquart), equals(4))

})

test_that("Frequencies are correct across output formats", {
  monthlyts <- Quandl("TESTS/2", type="ts")
  monthlyzoo <- Quandl("TESTS/2", type="zoo")
  monthlyxts <- Quandl("TESTS/2", type="xts")
  expect_that(frequency(monthlyts), equals(12))
  expect_that(frequency(monthlyzoo), equals(12))
  expect_that(frequency(monthlyxts), equals(12))
})

test_that("Data is the same across formats", {
  monthlyts <- Quandl("TESTS/2", type="ts")
  monthlyzoo <- Quandl("TESTS/2", type="zoo")
  monthlyxts <- Quandl("TESTS/2", type="xts")
  expect_that(max(abs(monthlyts - coredata(monthlyzoo))), equals(0))
  expect_that(max(abs(coredata(monthlyzoo) - coredata(monthlyxts))) , equals(0))
})