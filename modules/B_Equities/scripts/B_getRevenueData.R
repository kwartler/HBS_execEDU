#' Author: Ted Kwartler
#' Data: 10-21-2022
#' Purpose: Grab some time series revenue data and visualize it
#' https://ycharts.com/companies/AMZN/revenues

# Options
options(scipen=999)

# library
library(jsonlite)
library(lubridate)
library(xts)
library(ggplot2)
library(ggthemes)
library(readr)

# read in Data
qtrDF <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/AMZN_Qtr_Rev.csv')
qtrDF <- as.data.frame(qtrDF)
head(qtrDF)

# Change to date
qtrDF$date  <- as_datetime(qtrDF$unixTime)
tail(qtrDF)

# Change to a time series
stYr  <- year(qtrDF$date[1])
stQtr <- quarter(qtrDF$date[1])
st    <- c(stYr, stQtr)

# Base R time series
qtrTS2 <- ts(qtrDF$revMill, start = st, frequency = 4)
qtrTS2

# Basic ggplot2
p <- ggplot(qtrDF, aes(x=date, y=revMill)) +
  geom_line() + 
  xlab("") + 
  theme_hc() + 
  ggtitle('amzn qtr rev')
p

# End