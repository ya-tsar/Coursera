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

#linear ploting 
plot(data_feb$f_date, data_feb$Global_active_power, 
     type = "l", xlab = "", ylab = "Global Active Power")

#saving png file
dev.copy(png, file = "plot2.png")
dev.off()