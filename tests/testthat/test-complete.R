context("complete")

test_that("complete", {
  expect_identical(dts_complete(dts_data[1:3,]), dts_complete(dts_data[1:3,c("DateTime", "Value")]))
  complete <- dts_data[c(1,3,2),c("DateTime", "Value")]
  complete$Value[3] <- NA
  rownames(complete) <- NULL
  expect_equal(dts_complete(dts_data[c(1,3),]), complete[order(complete$DateTime),])
})