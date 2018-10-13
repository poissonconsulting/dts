context("separate")

test_that("separate", {
  x <- dts_separate(dts_data[1:3,], suffix = "o", prefix = "b")
  expect_identical(colnames(x), c("DateTime", "Value", "Value2", "Value3", 
                                  "oYearb", "oMonthb", "oDayb", "oDayteb"))
  expect_identical(x$oDayb, c(29L, 29L, 30L))
})

test_that("separate", {
  x <- dts_separate(dts_data[1:3,], hour = TRUE, second = TRUE, dayte = TRUE)
  expect_identical(colnames(x), c(
    "DateTime", "Value", "Value2", "Value3",
    "Year", "Month", "Day",  "Hour", 
    "Second", "Dayte"))
  expect_identical(x$Hour, c(22L, 23L, 0L))
})
