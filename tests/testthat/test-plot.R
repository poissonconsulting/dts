context("plot")

test_that("plot", {
  expect_is(dts_plot(dts_data), "ggplot")
})