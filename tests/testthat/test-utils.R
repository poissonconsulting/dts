context("utils")

test_that("tz", {
  expect_identical(dts_tz(dts_data$DateTime[1]), "Etc/GMT+8")
})

test_that("complete", {
  expect_true(dts_complete(dts_data$DateTime[c(1,2,3)]))
  expect_false(dts_complete(dts_data$DateTime[c(1,3)]))
  expect_true(dts_complete(dts_data$DateTime[c(1,3,2)]))
  expect_true(dts_complete(as.POSIXct(character(0))))
  expect_true(dts_complete(dts_data$DateTime[2]))
})

test_that("interval", {
  expect_identical(dts_interval(as.POSIXct("2000-01-01 00:00:01")), "seconds")
  expect_identical(dts_interval(as.POSIXct("2000-01-01 00:01:01")), "seconds")
  expect_identical(dts_interval(as.POSIXct("2000-01-01 00:01:00")), "minutes")
  expect_identical(dts_interval(as.POSIXct("2000-01-01 01:00:00")), "hours")
  expect_identical(dts_interval(as.POSIXct("2000-01-01 00:00:00")), "years")
  expect_identical(dts_interval(as.POSIXct("2000-01-02 00:00:00")), "days")
  expect_identical(dts_interval(as.POSIXct("2000-02-01 00:00:00")), "months")
  expect_identical(dts_interval(as.POSIXct(character(0))), character(0))
})

