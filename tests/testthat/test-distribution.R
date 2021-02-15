test_that("distribution", {
  data <- data.frame(DateTime = seq(as.Date("1972-01-01"), as.Date("1973-01-01"),
                                    by = 1))
  
  sd <- 7
  data <- dts_distribution(data, .timing = as.Date(c("1972-03-01", "1972-09-01", "1973-01-02")),
                      sd = sd, normalize = FALSE)
  expect_equal(data$Distribution[1], 6.3407e-18, tolerance = 1e-24) 
  expect_equal(data$Distribution[61], 0.05699175, tolerance = 1e-08)
  expect_equal(data$Distribution[153], 3.531668e-39, tolerance = 1e-45) 
  expect_equal(data$Distribution[245], 0.05699175, tolerance = 1e-08)
  expect_equal(data$Distribution[246], 0.05641316, tolerance = 1e-08)
  expect_equal(data$Distribution[367], 0.05641316, tolerance = 1e-08)

  data <- dts_distribution(data, .timing = as.Date(c("1972-03-01", "1972-09-01", "1973-01-02")),
                      sd = 7, normalize = TRUE)
  expect_equal(sum(data$Distribution), 1)
  expect_equal(data$Distribution[61], 0.02305954, tolerance = 1e-08)
})
