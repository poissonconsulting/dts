context("check")

test_that("check", {
  expect_identical(check_dts(dts_data), dts_data)
  data <- data.frame(DateTime = as.Date(c("2001-01-01", "2001-01-02")), Value = c(1,2))
  expect_identical(check_dts(data), data)
  expect_error(check_dts(data, values = 1L), "column 'Value' of data must be class integer")
  data$Value[2] <- NA
  expect_error(check_dts(data, values = 1), "column 'Value' of data must not include missing values")
  is.na(data$DateTime[1]) <- TRUE
  expect_error(check_dts(data), "column 'DateTime' of data must not include missing values OR column 'DateTime' of data must be class POSIXct")
})
