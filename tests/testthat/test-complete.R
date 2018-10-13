context("complete")

test_that("completed", {
  expect_true(dts_completed(dts_data[1:3,]))
  expect_true(dts_completed(dts_data[c(1,3,2),], sorted = FALSE))
  expect_false(dts_completed(dts_data[c(1,3,2),]))
  expect_false(dts_completed(dts_data[c(1,3),]))
})

test_that("complete", {
  expect_identical(dts_complete(dts_data[1:3,]), dts_complete(dts_data[1:3,]))
  complete <- dts_data[c(1,3,2),c("DateTime", "Value")]
  complete$Value[3] <- NA
  rownames(complete) <- NULL
  expect_equal(dts_complete(dts_data[c(1,3),c("DateTime", "Value")])$DateTime, 
               complete[order(complete$DateTime),]$DateTime)
})