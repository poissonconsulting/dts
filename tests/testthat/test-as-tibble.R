context("as-tibble")

test_that("as_tibble", {
  expect_is(dts_as_tibble(data.frame()), "tbl_df")
})