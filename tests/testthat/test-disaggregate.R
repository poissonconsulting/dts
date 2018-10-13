context("disaggregate")

test_that("disaggregate", {
  dis <- dts_disaggregate(dts_aggregate(dts_data[1,], units = "years"))
  expect_identical(colnames(dis), c("DateTime", "Value", "Value2", "Value3"))
  expect_identical(dis$Value, rep(-9, 12))
})
