context("utils")

test_that("mean_na_rm", {
  expect_identical(mean_na_rm(c(1:2, NA)), 1.5)
  expect_identical(mean_na_rm(NA), NaN)
})