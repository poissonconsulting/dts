test_that("aggregate", {
  expect_equal(dts_aggregate(dts_data, units = "years")$Value, c(NA_real_, NA_real_, NA_real_))
  expect_equal(colnames(dts_aggregate(dts_data, colname = "Value2", units = "years")), c("DateTime", "Value2"))
  expect_equal(dts_aggregate(dts_data, units = "years", na.rm =TRUE)$Value, 
               c(10.31948, 10.44202, 11.24257), tolerance = 1e-06)
})