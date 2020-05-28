

library(readr)
library(quantmod)
library(tidyverse)
library(xts)
library(lubridate)



#TX <- read.csv("C:\\Users\\User\\Desktop\\Data_from_TFE\\Daily_2020_05_27.csv")
TX <- read.csv("C:\\Users\\User\\Desktop\\Data_from_TFE\\Daily_2020_05_27_tem.csv")
colnames(TX) <- c("Date", "Symbol", "Delivery", "Time", "Price", "Volume", "", "", "")
TX <- TX[c(-7, -8, -9)]
#Row = which(TX$Symbol == "TX")
#TX1 = TX[Row, ]

is.na(TX) %>% sum()

#TX1 <- TX %>% filter(Symbol == "TX")
#View(TX1)

TX2 <- TX %>% filter(Date == 20200526)

TX2 <- TX %>% filter(as.numeric(TX$Time) >= 84500)
TX2 <- TX %>% filter(as.numeric(TX$Time) <= 134500)
TX2 <- TX %>% filter(as.numeric(TX$Time) >= 84500 & as.numeric(TX$Time) <= 134500)

timeCharVector = paste(TX$Date, TX$Time)
timeVector = strptime(timeCharVector, "%Y%m%d %H%M%S", tz = Sys.timezone())

DateTime <- ymd_hms(paste(TX$Date, TX$Time), tz = Sys.timezone())
test <- paste(DateTime, TX[c(5,6)])

TX3 <- TX2 %>% na.omit() 
timeVector <- timeVector %>% na.omit()

duration <- "2020-05-26 08:45:00::2020-05-26 13:45:00"

TX3 <- cbind(TX2[ ,c(5:6)], as.POSIXct(timeVector, duration, tz = Sys.timezone()))

TX4 <- xts(TX3[ ,c(5:6)], as.POSIXct(timeVector, duration, tz = Sys.timezone()))
#TX3 <- TX %>% filter(Delivery == 202006)
#TX3 <- TX[TX$Delivery == "202006", ]


