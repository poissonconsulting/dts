context("fill-dayte-time")

test_that("fill-dayte-time", {
  filled <- dts_fill_dayte_time(dts_data, min_gap = 0L)
  expect_identical(colnames(filled), colnames(dts_data))
  expect_identical(filled$DateTime, dts_data$DateTime)
  expect_equal(filled$Value[1:5], 
               c(-9.000000,  1.055405, 10.686807, 10.686807, 10.263481),
               tolerance = 1e-07)
  expect_equal(
    dts_fill_dayte_time(dts_data, min_gap = 0L, units = "days")$Value[1:5], 
    c(-9.00000, 10.29834, 10.29834, 10.29834, 10.26348),
    tolerance = 1e-06)
  expect_equal(
    dts_fill_dayte_time(dts_data, min_gap = 1L, units = "days")$Value[1:5], 
    c(-9.00000, NA, 10.29834, NA, 10.26348),
    tolerance = 1e-06)
  expect_equal(
    dts_fill_dayte_time(dts_data, min_n = 3L, min_gap = 0L, units = "days")$Value[1:5], 
    c(-9.00000, 10.29834, 10.29834, 10.29834, 10.26348),
    tolerance = 1e-06)
  expect_equal(
    dts_fill_dayte_time(dts_data, min_n = 3L, min_gap = 0L)$Value[1:5], 
    c(-9.00000, NA, NA, NA, 10.26348),
    tolerance = 1e-06)
})
