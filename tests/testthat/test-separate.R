context("separate")

test_that("separate_date", {
  x <- dts_separate_date(dts_data[1:3,], dayte = TRUE, doy = TRUE, 
                         suffix = "o", prefix = "b")
  expect_identical(colnames(x), c("DateTime", "Value", "Value2", "Value3", 
                                  "oYearb", "oMonthb", "oDayb", "oDayteb", "oDoyb"))
  expect_identical(x$oDoyb, c(333L, 333L, 334L))
})

test_that("separate_date_time", {
  x <- dts_separate_date_time(dts_data[1:3,], dayte = TRUE, doy = TRUE, 
                              dayte_time = TRUE)
  expect_identical(colnames(x), c(
    "DateTime", "Value", "Value2", "Value3",
    "Year", "Month", "Day", "Hour", "Minute",    
    "Second", "Dayte", "Doy", "DayteTime"))
  expect_identical(x$Hour, c(22L, 23L, 0L))
})
