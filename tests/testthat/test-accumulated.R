test_that("accumulated", {
  data <- dts_accumulated(dts_data[1:5,], colname = "Value2",
                          accum = c(Hatch = 40), units = "hours")
  
  expect_identical(data$Hatch, 
                   structure(c(943948800, 943952400, 943956000, NA, NA), 
                             class = c("POSIXct", "POSIXt"), tzone = "Etc/GMT+8"))
  
  data <- dts_accumulated(dts_data[1:5,] , colname = "Value2",
                          accum = c(Hatch = 40))
  
  expect_identical(data$Hatch,   
                   structure(rep(NA_real_, 5), 
                             class = c("POSIXct", "POSIXt"), tzone = "Etc/GMT+8"))
  data$Value2 <- data$Value2 * -1
})