#' Author: Ted Kwartler
#' Date: 10-21-2022
#' Purpose: Forecasting Basics
#'

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# libs
library(dygraphs)
library(readr)
library(dygraphs)
library(lubridate)

# Data
df <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/Amtrak.csv')

# Examine
head(df, 10)
tail(df,2)
class(df)
class(df$Month)

# Create a time series obj
riders <- ts(df$Ridership, 
             start     = c(1991, 1), 
             end       = c(2004, 3), 
             frequency = 12)

riders
class(riders)

# Level
riderLev <- mean(df$Ridership)

# Basic Viz
plot(riders, xlab = 'Months', ylab = "monthly Ridership in 000s")
abline(a = riderLev, b = 0, col='red')

# Dynamic to see cyclicality
dygraph(riders, main = "Amtrack Ridership") %>% 
  dyRangeSelector()

## Dealing with Dates
# Make a date obj
df$Month
wrongData <- mdy(df$Month) ##HINT: WRONG! Be sure to know the month, day, yr order!

# Now Check
tail(df$Month, 10) #why would it just be the first 12 days of a month?  The ORDER is different!
tail(wrongData, 10)  #Here you see 12 days, then nothing for a month, so be careful!

# Recreate correctly
df$Month[1:13]
cleanDate <- dmy(df$Month)
tail(df$Month, 10)
tail(cleanDate, 10)

# Chk class
class(cleanDate)

# Extract as standalone data vecs
df$day   <- day(cleanDate)
df$month <- month(cleanDate)
df$yr    <- year(cleanDate)

head(df)

# Calculate date differences w/difftime()
Sys.Date()
cleanDate[1]
difftime(Sys.Date(), cleanDate[1])
difftime(Sys.Date(), cleanDate[1], units = 'weeks')

# Nest difftime to engineer new data vectors
df$daysPassed   <- as.numeric(difftime(Sys.Date(), cleanDate))
df$monthsPassed <- as.numeric(difftime(Sys.Date(), cleanDate, units="weeks")) /4
df$yrsPassed    <- as.numeric(difftime(Sys.Date(), cleanDate, unit="weeks"))/52.25
head(df)

# End
