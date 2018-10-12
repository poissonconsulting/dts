context("check")

test_that("check", {
  expect_identical(check_dts(dts_data), dts_data)
  data <- data.frame(DateTime = as.Date(c("2001-01-01", "2001-01-02")), Value = c(1,2))
  expect_identical(check_dts(data), data)
  is.na(data$DateTime[1]) <- TRUE
  expect_identical(check_dts(data, value = "Value"), data)
  expect_error(check_dts(data, value = c("Value", "Value2")), 
                         "data column names must include 'DateTime', 'Value' and 'Value2'")
})
