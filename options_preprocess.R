
setwd("F:/optionsdata")
dir("F:/optionsdata")

# install.packages("tidyverse")
# install.packages("readr")
# install.packages("quantmod")
library(readr)
library(tidyverse)
library(quantmod)

#for (i in length(dir("F:/optionsdata"))) {
#  # dir("F:/optionsdata")[i]
#  dir(i) <- read_csv(dir("F:/optionsdata")[i], col_names = FALSE, skip = 2)
#}
#  rowbind()


options <- read_csv('optionsDaily_2018_05_24.csv', col_names = FALSE, skip = 2)
options %>% glimpse()

colnames(options) = c("Date", "Symbol", "Deal", "Delivery","CallPut", "Time", "Price", "Volume", "")
typeof(options)
options = options[, c(-9)] 
head(options)

condition1 <- options$Deal == 11000
condition2 <- options$CallPut == "C"

Row = which(condition1 & condition2) 
length(Row)

optionsData1 = options[Row, ]
#View(optionsData)

# optionsHotData = optionsData[optionsData$Delivery == "201708", ]
deliverycol <- t(table(optionsData1$Delivery)) %>% colnames()
optionsData2 <- optionsData1 %>% filter(Delivery == deliverycol[1])
timeCharVector = paste(optionsData2$Date, optionsData2$Time)

timeVector = strptime(timeCharVector, "%Y%m%d %H%M%S", tz = Sys.timezone()) #%y for 2 digits of year %Y for 4 digits of year!!!
#View(optionsData2)

OPtsdata <- xts(optionsData2[, c(7:8)], as.POSIXct(timeVector))
#View(OPtsdata)
chartSeries(OPtsdata)

duration = "2018-05-24 08:45:00::2018-05-24 09:15:00"
chartSeries(OPtsdata[duration], theme = "black")


TXO_1s = to.period(OPtsdata, "seconds", 1)
chartSeries(TXO_1s[duration])

TXO_10s = to.period(OPtsdata, "seconds", 10)
chartSeries(TXO_10s)
View(TXO_10s)

OPtsdata$Volume = OPtsdata$Volume / 2
duration = "2018-05-24 08:45:00::2018-05-24 10:00:00"
TXO_2min = to.period(OPtsdata, "minutes", 2)
chartSeries(TXO_2min[duration])
colnames(TXO_2min) = c("Open", "High", "Low", "Close", "Volume")
View(TXO_2min)
chartSeries(TXO_2min)





