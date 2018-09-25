
dts_data <- data.frame(DateTime = seq(as.POSIXct("1999-11-29 22:00:00", tz = "Etc/GMT+8"),
                                 as.POSIXct("2001-02-02 02:00:00", tz = "Etc/GMT+8"),
                                 by = "hours"))

dts_data$Value <- 10
dts_data$Value <- dts$Value - 0.5 + 1:nrow(dts_data)/nrow(dts_data) * 1

dts_data$Value <- dts_data$Value + rnorm(length(dts_data$Value), sd = 0.01)

dts::dts_plot(dts_data)

devtools::use_data(dts_data, overwrite = TRUE)
