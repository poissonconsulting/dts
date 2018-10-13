context("extrapolate")

test_that("extrapolate", {
  expect_equal(dts_extrapolate(dts_data[2:5,])$Value, rep(10.26348, 4),
               tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[4:6,])$Value2, 
               c(20.51740, 20.42421, 20.42421), tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[1:5,])$Value, 
               c(-9, NA, NA, NA, 10.26348), tolerance = 1e-06)
})