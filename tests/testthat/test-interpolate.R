context("interpolate")

test_that("interpolate", {
  expect_error(dts_interpolate(dts_data[c(1,3),]), "column 'DateTime' of x must be complete")
  expect_identical(dts_interpolate(dts_data[1:4,]), dts_data[1:4,]) 
  expect_equal(dts_interpolate(dts_data[1:5,])$Value, c(-9.0000000, -4.1841297,  0.6317406,  5.4476109, 10.2634812))
  expect_equal(dts_interpolate(dts_data[1:5,], max_gap = 1L)$Value, c(-9.00000, NA, NA, NA, 10.26348),
               tolerance = 1e-06)
})