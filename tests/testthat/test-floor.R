context("floor")

test_that("floor", {
  floored <- dts_floor(dts_data[c(1,3),], units = "years")
  expect_identical(floored[c("Value", "Value2", "Value3")],
                   dts_data[c(1,3),c("Value", "Value2", "Value3")])
  expect_identical(dttr::dtt_month(floored$DateTime), c(1L, 1L))
})