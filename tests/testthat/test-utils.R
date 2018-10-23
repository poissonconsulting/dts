context("utils")

test_that("mean_na_rm", {
  expect_identical(mean_na_rm(c(1:2, NA)), 1.5)
  expect_identical(mean_na_rm(NA), NaN)
})

test_that("pnorm_arrival_departure", {
  expect_equal(pnorm_arrival_departure(1:2, residence = 1/2), 
                   c(0.14988228, 0.04405707))
})
