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

#4 plots on one plot 
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

#Global active power linear ploting 
plot(data_feb$f_date, data_feb$Global_active_power, 
     type = "l", xlab = "", ylab = "Global Active Power")

#Voltage linear ploting 
plot(data_feb$f_date, data_feb$Voltage, 
     type = "l", xlab = "datetime", ylab = "Voltage")

#ploting Sub metering
plot(data_feb$f_date, data_feb$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
points(data_feb$f_date, data_feb$Sub_metering_2, col = "red", type = "l")
points(data_feb$f_date, data_feb$Sub_metering_3, col = "blue", type = "l")

#legend in top right corner
legend("topright", lty = c(1, 1, 1),  col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Global reactive power linear ploting 
plot(data_feb$f_date, data_feb$Global_reactive_power, 
     type = "l", xlab = "datetime", ylab = "Global_reactive_power")

#saving png file
dev.copy(png, file = "plot4.png")
dev.off()