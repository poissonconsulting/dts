
dts_data <- data.frame(DateTime = seq(as.POSIXct("1999-11-29 22:00:00", tz = "Etc/GMT+8"),
                                 as.POSIXct("2001-02-02 02:00:00", tz = "Etc/GMT+8"),
                                 by = "hours"))

dts_data$Value <- 10
dts_data$Value <- dts_data$Value - 0.5 + 1:nrow(dts_data)/nrow(dts_data) * 1
dts_data$Value <- dts_data$Value + abs(dtt_doy(dts_data$DateTime) - 366/2) * 0.005 

set.seed(-99)
dts_data$Value <- dts_data$Value + rnorm(length(dts_data$Value), sd = 0.01)

dts_data$Value2 <- 5 + dts_data$Value * 1.5 + rnorm(nrow(dts_data), 0, sd = 0.3)

dts_data$Value3 <- dts_data$Value / 2.5 + rnorm(nrow(dts_data), 0, sd = 0.01)

dts_data$Value[unique(runif(100, 1, nrow(dts_data)))] <- NA_real_
dts_data$Value[unique(runif(10, 1, nrow(dts_data)))] <- -9
dts_data$Value[1] <- -9

dts_data$Value2[unique(runif(100, 1, nrow(dts_data)))] <- NA_real_
dts_data$Value3[unique(runif(100, 1, nrow(dts_data)))] <- NA_real_

dts_data$Value[2:4] <- NA
dts_data$Value2[6] <- NA
dts_data$Value3[6] <- NA

dts::dts_plot(dts_data)

dts_data <- tibble::as_tibble(dts_data)

devtools::use_data(dts_data, overwrite = TRUE)
