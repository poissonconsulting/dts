test_that("colnames", {
  expect_identical(dts_colnames(dts_data), c("Value", "Value2", "Value3"))
})