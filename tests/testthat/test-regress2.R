test_that("regress2", {
  expect_error(dts_regress2(dts_data[1:5,], min_gap = 0L), 
                   "there are less than 5 values to fit the regression")
  expect_error(dts_regress2(dts_data[1:5,], min_gap = 0L, min_n = 7L), 
                   "there are less than 7 values to fit the regression")
  expect_identical(dts_regress2(dts_data[1:5,], min_gap = 10L), dts_data[1:5,])
  expect_identical(dts_regress2(dts_data, min_gap = 10L)[1:5,], dts_data[1:5,])
  expect_equal(dts_regress2(dts_data, min_gap = 0L)$Value[1:5], 
                   c(-9.00000, 10.16121, 10.21792, 10.25005, 10.26348),
                tolerance = 1e-06)
  expect_equal(dts_regress2(dts_data, min_gap = 1L)$Value[1:5], 
                   c(-9.00000, NA, 10.21792, NA, 10.26348),
                tolerance = 1e-06)
  expect_equal(dts_regress2(dts_data, min_gap = 0L, intercept = FALSE)$Value[1:5], 
                   c(-9.00000, 10.16138, 10.21828, 10.25014, 10.26348),
                tolerance = 1e-06)
  
})
