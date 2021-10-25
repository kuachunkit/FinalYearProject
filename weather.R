#This script perform data wrangling and transformation on the Australian Weather data 
#output from Togaware (https://togaware.com/the-weatheraus-dataset/)
#The script also takes the fire occurance data frame to merge it with the weather data


#Insatall library and packages

#install.packages("dplyr")
#this library is used for data transformation and wrangling
library(dplyr)
#install.packages("lubridate")
#this library is used to handle data type data in the data frame
library(lubridate)

#read Australia weather data
weatherAUS <- read.csv("weatherAUS.csv")

#read state location data and occurance data
#contains the state and region of each location in the Australia Weather data
locstateregion = read.csv("stateloc.csv")
occurance = read.csv("occurance.csv")

#exclude Williamtown because there are multiple locations with the same name in Australia
#exclude NorfolkIsland because it is not located in Australian Mainland 
weatherAUS = weatherAUS[!(weatherAUS$Location == "Williamtown" | weatherAUS$Location == "NorfolkIsland"),]

#merge the weather data and the state location data
#to include the state and the region of the respective location
weather.reg = merge(x = weatherAUS, y = locstateregion, by = "Location", all.x = TRUE)

#data wrangling for date data
weather.reg$Date = as.Date(weather.reg$Date)

#filter unused date
#the NASA fire data only contains data from 2012-01-20 to 2020-05-30
weather.nasa = weather.reg[weather.reg$Date >= "2012-01-20" & weather.reg$Date <= "2020-05-30", ]
weather.nasa$Month = month(weather.nasa$Date)

#merge the weather data with the fire occurance data 
#by date, states and region
weather.nasa.occurance = merge(x = weather.nasa, y = occurance, by = c("Date", "StateTerritory", "Region"), all.x = TRUE)

#set na values to 0 because there are no bushfire occurance in the respective rows
weather.nasa.occurance[is.na(weather.nasa.occurance$Amount),]$Amount = 0

#remove unused columns
drop = c("Evaporation", "Sunshine", "Cloud9am", "Cloud3pm", "Region", "Location")
bf = weather.nasa.occurance[,!(names(weather.nasa.occurance) %in% drop)]


#convert columns to the correct data type
for (i in 1:ncol(bf)){
  if (class(bf[,i]) == "character"){
    bf[,i] = as.factor(bf[,i])
  }
  if (class(bf[,i]) == "integer"){
    bf[,i] = as.numeric(bf[,i])
  }
}

#omit records with na values
bushfire.omit = bf[complete.cases(bf), ]

#change the occurance column to a binary column with
#yes or not 
colnames(bushfire.omit)[22] = "Occurance"
bushfire.omit[bushfire.omit$Occurance != 0,]$Occurance = "Yes"
bushfire.omit[bushfire.omit$Occurance == 0,]$Occurance = "No"
bushfire.omit$Occurance = as.factor(bushfire.omit$Occurance)

#create a new column called month which records the month of the row
bushfire.omit$Month = as.factor(bushfire.omit$Month)

#export csv
write.csv(bushfire.omit, file = "bushfire.csv", row.names = FALSE)



