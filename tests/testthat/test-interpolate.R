context("interpolate")

test_that("interpolate", {
  expect_error(dts_interpolate(dts_data[c(1,3),]), "column 'DateTime' of x must be complete")
  expect_identical(dts_interpolate(dts_data[1:3,]), dts_data[1:3,]) 
  data <- dts_data[1:4,]
  data$Value[2:3] <- NA
  expect_equal(dts_interpolate(data)$Value, c(-999.00000, -662.58438, -326.16877, 10.24685))
  expect_equal(dts_interpolate(data, max_gap = 1L)$Value, c(-999.00000, NA, NA, 10.24685),
               tolerance = 1e-06)
})