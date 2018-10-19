context("delay")

test_that("delay", {
  data <- dts_data[1:3,]
  expect_identical(dts_delay(data, delay = 1)$Value2[2:3], data$Value2[1:2])
  expect_identical(dts_delay(data, delay = 1)$DateTime, data$DateTime[1:3])
  expect_identical(dts_delay(data, colname = "Value2", delay = -1)$Value, 
                   data$Value)
  expect_identical(dts_delay(data, colname = "Value2", delay = -1)$Value2, 
                   c(data$Value2[2:3], NA))
})