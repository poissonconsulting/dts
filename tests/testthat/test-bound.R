context("bound")

test_that("bound", {
  expect_equal(dts_bound(dts_data[1:5,], bound = c(10.24, 10.26))$Value, 
                   c(NA, 10.25331, NA, 10.24685, 10.24762), tolerance = 1e-06)
})