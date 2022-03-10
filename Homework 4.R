# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Read in data
campusWeather <- read.csv("/cloud/project/activity04/campus_weather.csv", 
                          na.strings = "#N/A")

# Question 1
campusWeatherExcluded <- campusWeather %>%
  filter(Precip > 0) %>%
  filter(XLevel < 2.0 & YLevel < 2.0)

# Question 2
campusWeather$BatteryFlag <- ifelse(campusWeather$BatVolt < 8.5, 
                                    "Battery Voltage is below 8.5", 
                                    "Battery Voltage is above 8.5")
# Question 3
airTempOutliers <- function(airTempVector) {
  outliers <- airTempVector > (mean(airTempVector, na.rm = T) + 2*sd(airTempVector, na.rm = T)) | airTempVector < (mean(airTempVector, na.rm = T) - 2*sd(airTempVector, na.rm = T))
  airTempVector[na.omit(outliers)]
}

solRadOutliers <- function(solRadVector) {
  outliers <- solRadVector > (mean(solRadVector, na.rm = T) + 2*sd(solRadVector, na.rm = T)) | solRadVector < (mean(solRadVector, na.rm = T) - 2*sd(solRadVector, na.rm = T))
  solRadVector[na.omit(outliers)]
}


airTempOutliers(campusWeather$AirTemp)
solRadOutliers(campusWeather$SolRad)

# Question 4

# parse date
campusWeather$dateF <- mdy_hm(campusWeather$Date)
# create a month column
campusWeather$doy <- yday(campusWeather$dateF)
# create a year column
campusWeather$year <- year(campusWeather$dateF)

# examine precipitation using a bar plot
campusWeatherJanMar2021 <- campusWeather[campusWeather$doy >= 1 & campusWeather$doy <= 90 & campusWeather$year == 2021 ,]

ggplot(data = campusWeatherJanMar2021,
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()
