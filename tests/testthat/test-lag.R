test_that("lag", {
  data <- dts_data[1:3,]
  expect_identical(dts_lag(data, n = 1, units = "hours")$Value2[2:3], data$Value2[1:2])
  expect_identical(dts_lag(data, n = 1, units = "hours")$DateTime, data$DateTime[1:3])
  expect_identical(dts_lag(data, colname = "Value2", n = -1)$Value, 
                   data$Value)
  expect_identical(dts_lag(data, colname = "Value2", n = -1),
                   dts_lead(data, colname = "Value2", n = 1))
  expect_identical(dts_lag(data, colname = "Value2", units = "hours", 
                             n = -1)$Value2, 
                   c(data$Value2[2:3], NA))
  
  data <- dts_lag(dts_data, n = 61, units = "minutes")
  expect_identical(data$Value2[1:3], c(NA, dts_data$Value2[1:2]))
})