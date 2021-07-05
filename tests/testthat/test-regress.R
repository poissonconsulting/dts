test_that("regress", {
  expect_error(dts_regress(dts_data[1:5,], min_gap = 0L), 
                   "There are less than 4 values to fit the regression.")
  expect_error(dts_regress(dts_data[1:5,], min_gap = 0L, min_n = 5L), 
                   "There are less than 5 values to fit the regression.")
  expect_identical(dts_regress(dts_data[1:5,], min_gap = 10L), dts_data[1:5,])
  expect_identical(dts_regress(dts_data, min_gap = 10L)[1:5,], dts_data[1:5,])
  expect_equal(dts_regress(dts_data, min_gap = 0L)$Value[1:5], 
                   c(-9.00000, 10.24685, 10.02504, 10.35431, 10.26348),
                tolerance = 1e-06)
  expect_equal(dts_regress(dts_data, min_gap = 1L)$Value[1:5], 
                   c(-9.00000, NA, 10.02504, NA, 10.26348),
                tolerance = 1e-06)
  expect_equal(dts_regress(dts_data, min_gap = 0L, intercept = FALSE)$Value[1:5], 
                   c(-9.00000, 10.26092, 10.05124, 10.36250, 10.26348),
                tolerance = 1e-06)
})
