context("internal")

test_that("size_gaps", {
  expect_identical(size_gaps(logical(0)), integer(0))
  expect_identical(size_gaps(FALSE), 0L)
  expect_identical(size_gaps(TRUE), 1L)
  expect_identical(size_gaps(c(FALSE, FALSE)), c(0L, 0L))
  expect_identical(size_gaps(c(TRUE, TRUE)), c(2L, 2L))
  expect_identical(size_gaps(c(FALSE, FALSE, FALSE)), c(0L, 0L, 0L))
  expect_identical(size_gaps(c(TRUE, TRUE, TRUE)), c(3L, 3L, 3L))
  expect_identical(size_gaps(c(TRUE, FALSE)), c(1L, 0L))
  expect_identical(size_gaps(c(FALSE, TRUE)), c(0L, 1L))
  expect_identical(size_gaps(c(FALSE, TRUE, FALSE)), c(0L, 1L, 0L))
  expect_identical(size_gaps(c(FALSE, TRUE, TRUE, FALSE)), c(0L, 2L, 2L, 0L))
  expect_identical(size_gaps(c(TRUE, TRUE, FALSE, FALSE, TRUE)), c(2L, 2L, 0L, 0L, 1L))
})

