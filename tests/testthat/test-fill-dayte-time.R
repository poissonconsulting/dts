context("fill-dayte-time")

test_that("fill-dayte-time", {
  data <- data.frame(DateTime = as.Date(c("2000-01-01", "2001-01-01", "2002-01-01", "2003-01-01", "2004-01-01")),
                     Value = c(NA, NA, 10, 16, NA))
  expect_equal(dts_fill_dayte_time(data)$Value, c(13, 13, 10, 16, 13),
               tolerance = 1e-06)
  expect_equal(dts_fill_dayte_time(data, min_gap = 1L)$Value, c(13, NA, 10, 16, NA),
               tolerance = 1e-06)
})

test_that("fill-dayte-time", {
  data <- data.frame(DateTime = seq(as.Date("2000-01-01"), as.Date("2003-01-01"), by = "days"))
  data$Value <- dttr::dtt_days(data$DateTime)
  data$Value[1:5] <- NA
  expect_identical(dts_fill_dayte_time(data)$Value[1:5], as.double(1:5))
})
