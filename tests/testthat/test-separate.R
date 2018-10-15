context("separate")

test_that("separate", {
  x <- dts_separate_date_time(dts_data[1:3,], suffix = "o", prefix = "b", minute = FALSE)
  expect_identical(colnames(x), c("DateTime", "Value", "Value2", "Value3", 
                                  "oYearb", "oMonthb", "oDayb",
                                  "oHourb", "oSecondb"))
  expect_identical(x$oDayb, c(29L, 29L, 30L))
})

test_that("separate", {
  x <- dts_separate_date(dts_data[1:3,])
  expect_identical(colnames(x), c(
    "DateTime", "Value", "Value2", "Value3",
    "Year", "Month", "Day"))
  expect_identical(x$Year, rep(1999L, 3))
})
