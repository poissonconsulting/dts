context("accumulated")

test_that("accumulated", {
  data <- dts_accumulated(dts_data[1:5,], colname = "Value2",
                          accum = list(Hatch = dttr::dtt_duration(40, "hours")))
  
  expect_identical(data$Hatch, 
  structure(c(943948800, 943952400, 943956000, NA, NA), class = c("POSIXct", 
"POSIXt"), tzone = "Etc/GMT+8"))
})