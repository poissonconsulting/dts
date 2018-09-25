context("aggregate")

test_that("aggregate", {
  expect_equal(dts_aggregate(dts_data, units = "years")$Value, c(NA_real_, NA_real_, NA_real_))
  expect_equal(dts_aggregate(dts_data, units = "years", na.rm =TRUE)$Value, 
               c( 7.723556, 9.645600, 8.654284), tolerance = 1e-07)
})