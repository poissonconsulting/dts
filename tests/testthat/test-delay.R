context("delay")

test_that("delay", {
  data <- dts_data[1:3,]
  expect_identical(dts_delay(data, delay = 1, units = "hours")$Value2[2:3], data$Value2[1:2])
  expect_identical(dts_delay(data, delay = 1, units = "hours")$DateTime, data$DateTime[1:3])
  expect_identical(dts_delay(data, colname = "Value2", delay = -1)$Value, 
                   data$Value)
  expect_identical(dts_delay(data, colname = "Value2", units = "hours", 
                             delay = -1)$Value2, 
                   c(data$Value2[2:3], NA))
  
  data <- dts_delay(dts_data, delay = 61, units = "minutes")
  expect_identical(data$Value2[1:3], c(NA, dts_data$Value2[1:2]))
})