test_that("extrapolate", {
  x <- dts_data[4:6,]
  x$Value2[2] <- NA
  expect_equal(dts_extrapolate(x)$Value2, rep(20.517399406987, 3))
  expect_equal(dts_extrapolate(x, max_span = 1L)$Value2, c(rep(20.517399406987, 2), NA))
  expect_equal(dts_extrapolate(x, max_span = 2L)$Value2, rep(20.517399406987, 3))
  
  expect_equal(dts_extrapolate(dts_data[2:5,])$Value, rep(10.26348, 4),
               tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[2:5,], max_span = 1L)$Value, c(rep(NA, 2), rep(10.26348, 2)),
               tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[2:5,], max_span = 2L)$Value, c(rep(NA, 1), rep(10.26348, 3)),
               tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[4:6,])$Value2, 
               c(20.51740, 20.42421, 20.42421), tolerance = 1e-06)
  expect_equal(dts_extrapolate(dts_data[1:5,])$Value, 
               c(-9, NA, NA, NA, 10.26348), tolerance = 1e-06)
})