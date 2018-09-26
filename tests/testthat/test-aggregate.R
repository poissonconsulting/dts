context("aggregate")

test_that("aggregate", {
  expect_equal(dts_aggregate(dts_data, units = "years")$Value, c(NA_real_, NA_real_, NA_real_))
  expect_equal(dts_aggregate(dts_data, units = "years", na.rm =TRUE)$Value, 
               c(10.34461, 10.43526, 11.29553), tolerance = 1e-06)
})