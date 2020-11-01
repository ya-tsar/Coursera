library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
#reading data

#loading the data
NEI_path <- list.files(pattern = "summarySCC_PM25.rds", 
                       ignore.case = TRUE, 
                       recursive = TRUE)

NEI <- readRDS(NEI_path) %>% data.table()

SCC_path <- list.files(pattern = "Source_Classification_Code.rds", 
                       ignore.case = TRUE, 
                       recursive = TRUE)
SCC <- readRDS(SCC_path) %>% data.table()

NEI_1 <- NEI %>% na.omit()

par(mfrow = c(2,2), mar = c(4,4,2,1))

NEI_1999 <- NEI %>% filter(year == 1999)
boxplot(log10(NEI_1999$Emissions))

NEI_2002 <- NEI %>% filter(year == 2002)
boxplot(log10(NEI_2002$Emissions))

NEI_2005 <- NEI %>% filter(year == 2005)
boxplot(log10(NEI_2005$Emissions))

NEI_2008 <- NEI %>% filter(year == 2008)
boxplot(log10(NEI_2008$Emissions))

NEI_Baltimor <- NEI %>% filter(fips == "24510")

hist(NEI_Baltimor$Emissions)

#ploting boxplot (log10(Emissions))
boxplot(log10(Emissions) ~ year, data = NEI, xlab = "year", main = "Total emissions from PM2.5")

#saving png file
dev.copy(png, file = "plot1.png")
dev.off()

#Subset Baltimor data
NEI_Baltimor <- NEI %>% 
  filter(fips == "24510")

#ploting boxplot for Baltimor data (log10(Emissions))
boxplot(log10(Emissions) ~ year, data = NEI_Baltimor, xlab = "year", main = "Total emissions from PM2.5 the Baltimore City, Maryland")

#saving png file
dev.copy(png, file = "plot2.png")
dev.off()

#countig of type for all years
NEI_Baltimor_type_count <- NEI_Baltimor %>%
  group_by(type, year) %>%
  summarise(Count = n())

g <- ggplot(NEI_Baltimor_type_count, aes(year, Count)) + geom_point() 
g + geom_line(aes(year, Count, color = factor(type))) + ggtitle("Types of sources in the Baltimore City")

#saving png file
dev.copy(png, file = "plot3.png")
dev.off()

#Join Source Classification Code to PM2.5 Emissions Data
NEI_SCC <- NEI %>% 
  merge(SCC, by = "SCC", all.x = TRUE)

#Coal sources
NEI_SCC_coal <- NEI_SCC %>%
  filter(str_detect(Short.Name, "[Cc]oal") == TRUE)

g <- ggplot(NEI_SCC_coal, aes(factor(year), log10(Emissions)))
g + geom_boxplot() + xlab("Year") + ggtitle("Emissions from coal combustion-related sources")

#saving png file
dev.copy(png, file = "plot4.png")
dev.off()

#Motor Vehicles sources
NEI_SCC_motor_vehicles_Baltimor <- NEI_SCC %>%
  filter(fips==24510) %>%
  filter(str_detect(Short.Name, "[Mm]otor [Vv]ehicle") == TRUE)

g <- ggplot(NEI_SCC_motor_vehicles_Baltimor, aes(factor(year), Emissions))
g + geom_point() + xlab("Year") + ggtitle("Emissions from Motor Vehicles sources in the Baltimore City")

#saving png file
dev.copy(png, file = "plot5.png")
dev.off()

#Motor Vehicles sources in the Baltimor and Los Angeles County
NEI_SCC_motor_vehicles_Baltimor_LA <- NEI_SCC %>%
  filter(fips == 24510 | fips == "06037") %>%
  filter(str_detect(Short.Name, "[Mm]otor [Vv]ehicle") == TRUE) %>%
  mutate(City = if_else(fips == 24510, "Baltimor", "Los Angeles"))

g <- ggplot(NEI_SCC_motor_vehicles_Baltimor_LA, aes(factor(year), Emissions, color = City))
g + geom_point() + xlab("Year") + ggtitle("Emissions from Motor Vehicles sources")

#saving png file
dev.copy(png, file = "plot6.png")
dev.off()
