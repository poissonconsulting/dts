context("regress")

test_that("regress", {
  expect_error(dts_regress(dts_data[1:5,], min_gap = 0L), 
                   "there are less than 4 values to fit the regression")
  expect_identical(dts_regress(dts_data[1:5,]), dts_data[1:5,])
  expect_identical(dts_regress(dts_data)[1:5,], dts_data[1:5,])
  expect_equal(dts_regress(dts_data, min_gap = 0L)$Value[1:5], 
                   c(-9.00000, 10.24542, 10.02246, 10.35344, 10.26348),
                tolerance = 1e-06)
})