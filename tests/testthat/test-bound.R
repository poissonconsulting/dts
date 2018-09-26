context("bound")

test_that("bound", {
  expect_equal(dts_bound(dts_data[1:5,], bound = c(0, Inf))$Value, 
                   c(NA, NA, NA, NA, 10.26348), tolerance = 1e-06)
})