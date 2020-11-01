library(readr)
library(dplyr)
library(lubridate)

#data load
data_path <- list.files(pattern = "household_power_consumption.txt",
                        recursive = TRUE,
                        ignore.case = TRUE)

data <- read_delim(data_path, delim = ";")

#date subset
data_feb <- data %>% 
  filter(dmy(Date) %within% interval(ymd("2007-02-01"), ymd("2007-02-02"))) %>%
  #concatenating date and time converting to POSIXct date format
  mutate(f_date = dmy_hms(paste(Date, Time, sep = " ")))

#ploting Sub metering
plot(data_feb$f_date, data_feb$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
points(data_feb$f_date, data_feb$Sub_metering_2, col = "red", type = "l")
points(data_feb$f_date, data_feb$Sub_metering_3, col = "blue", type = "l")

#legend in top right corner
legend("topright", lty = c(1, 1, 1),  col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#saving png file
dev.copy(png, file = "plot3.png")
dev.off()