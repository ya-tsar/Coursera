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
  filter(dmy(Date) %within% interval(ymd("2007-02-01"), ymd("2007-02-02")))

#histogram ploting
hist(data_feb$Global_active_power, col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)")

#saving png file
dev.copy(png, file = "plot1.png")
dev.off()

