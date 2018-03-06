# if (!("quantmod" %in% as.character(installed.packages()[,1]))) {install.packages("quantmod")}
# remove.packages("quantmod")

# install.packages(c("tidyverse", "lubridate", "magrittr", "stringr"))
# install.packages(c("ggplot2"))
# install.packages(c("zoo", "xts"))
# install.packages(c("quantmod", "Quandl", "tidyquant", "PerformanceAnalytics", "TTR"))
# remotes::install_github("joshuaulrich/quantmod")
# devtools::install_github("joshuaulrich/quantmod")

library(tidyverse); library(lubridate); library(magrittr); library(stringr)
library(ggplot2)
library(zoo); library(xts)
library(quantmod); library(Quandl); library(tidyquant); library(PerformanceAnalytics); library(TTR)

# Sys.setenv(TZ = "Asia/Taipei")
# now()

setwd("F:/")
getwd()
# dir()

# Data from Taiwan Futures Exchange
# y <- "2018"
# m <- "02"
# d <- as.character(c(21:23))
# d <- c("01", "02", "05", "06", "07", "08", "09", "12", "21", "22", "23", "26", "27")
# d <- "22"

# d <- function(d) {
#   d <- paste("0", d, sep = "")
#   return(d)
# }
# d(6:7)

# Data <- tibble(
#   y = 2018, 
#   m = "02", 
#   d = 21:22,
# )

read.data.12 <- function(d) {
  paste("Daily_", "2017", "_", "12", "_", d, ".csv", sep = "") %>%
    read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE)
}
TX.12.22 <- read.data.12("22"); TX.12.25 <- read.data.12("25"); TX.12.26 <- read.data.12("26")
TX.12.27 <- read.data.12("27"); TX.12.28 <- read.data.12("28"); TX.12.29 <- read.data.12("29")

read.data.01 <- function(d) {
  paste("Daily_", "2018", "_", "01", "_", d, ".csv", sep = "") %>%
    read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE)
}
TX.01.02 <- read.data.01("02"); TX.01.03 <- read.data.01("03"); TX.01.04 <- read.data.01("04")
TX.01.05 <- read.data.01("05"); TX.01.08 <- read.data.01("08"); TX.01.09 <- read.data.01("09")
TX.01.10 <- read.data.01("10"); TX.01.11 <- read.data.01("11"); TX.01.12 <- read.data.01("12")
TX.01.15 <- read.data.01("15"); TX.01.16 <- read.data.01("16"); TX.01.17 <- read.data.01("17")
TX.01.18 <- read.data.01("18"); TX.01.19 <- read.data.01("19"); TX.01.22 <- read.data.01("22")
TX.01.23 <- read.data.01("23"); TX.01.24 <- read.data.01("24"); TX.01.25 <- read.data.01("25")
TX.01.26 <- read.data.01("26"); TX.01.29 <- read.data.01("29"); TX.01.30 <- read.data.01("30")
TX.01.31 <- read.data.01("31")

read.data.02 <- function(d) {
  paste("Daily_", "2018", "_", "02", "_", d, ".csv", sep = "") %>%
    read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE)
}
# TX <- read.data(d)
# paste.data.name <- function(d) {
#   tem.data <- paste("TX.", d, sep = "")
#   tem.data.1 <- read.data(d)
#   tem.data <- tem.data.1
# }
# paste.data.name(d)

# TX <- read.csv("Daily_2018_02_06.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# TX <- read.data(d)
TX.02.01 <- read.data.02("01"); TX.02.02 <- read.data.02("02"); TX.02.05 <- read.data.02("05")
TX.02.06 <- read.data.02("06"); TX.02.07 <- read.data.02("07"); TX.02.08 <- read.data.02("08")
TX.02.09 <- read.data.02("09"); TX.02.12 <- read.data.02("12"); TX.02.21 <- read.data.02("21")
TX.02.22 <- read.data.02("22"); TX.02.23 <- read.data.02("23"); TX.02.26 <- read.data.02("26")
TX.02.27 <- read.data.02("27")

read.data.03 <- function(d) {
  paste("Daily_", "2018", "_", "03", "_", d, ".csv", sep = "") %>%
    read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

TX.03.01 <- read.data.03("01"); TX.03.02 <- read.data.03("02"); TX.03.05 <- read.data.03("05")

TX <- rbind(TX.12.22, TX.12.25, TX.12.26, TX.12.27, TX.12.28, TX.12.29,
            TX.01.02, TX.01.03, TX.01.04, TX.01.05, TX.01.08, TX.01.09, TX.01.10, TX.01.11, TX.01.12, TX.01.15, TX.01.16, TX.01.17, TX.01.18, TX.01.19, TX.01.22, TX.01.23, TX.01.24, TX.01.25, TX.01.26, TX.01.29, TX.01.30, TX.01.31,
            TX.02.01, TX.02.02, TX.02.05, TX.02.06, TX.02.07, TX.02.08, TX.02.09, TX.02.12, TX.02.21, TX.02.22, TX.02.23, TX.02.26, TX.02.27,
            TX.03.01, TX.03.02, TX.03.05)

names(TX) <- c("Date", "Mark", "Month", "Time", "Price", "Volume")
TX <- TX[, 1:6]
# (TX <- select(TX, Date, Mark, Month, Time, Price, Volume))

str(TX); glimpse(TX); head(TX, 5)
# review <- function(x) {
#   return(class(x))
#   return(str(x))
#   return(glimpse(x))
# }
# review(TX)

# write(TX, "TX.txt")

table(str_detect(TX$Month, "^201803"), nchar(TX$Month) == 6)
table(str_detect(TX$Month, "^201803"), str_detect(TX$Mark, "^TX"))

remove.space <- function (x){
  gsub("^\\s+ | \\s+$", "", x)
}
TX$Month <- remove.space(TX$Month)
# TX <- TX[str_detect(TX$Month, "^201803"), ]
TX <- TX[nchar(TX$Month) == 6, ]

TX$Mark <- remove.space(TX$Mark)
TX <- TX[str_detect(TX$Mark, "^TX"), ]

TX <- TX %>% mutate(Pre = ymd(Date))
TX <- TX %>% mutate(pre.delete.Year = year(Pre))
TX <- TX %>% mutate(pre.delete.Month = month(Pre))
TX <- TX %>% mutate(pre.delete.Day = day(Pre))


TX.2018.01 <- TX %>% filter(Pre >= as.Date("2017-12-21") & Pre <= as.Date("2018-01-17") & Month == 201801)
TX.2018.02 <- TX %>% filter(Pre >= as.Date("2018-01-18") & Pre <= as.Date("2018-02-21") & Month == 201802)
TX.2018.03 <- TX %>% filter(Pre >= as.Date("2012-02-22") & Pre <= as.Date("2018-03-21") & Month == 201803)
TX <- rbind(TX.2018.01, TX.2018.02, TX.2018.03)

# TX <- subset(TX, TX$Mark == "^TX")
# TX <- TX$Month[TX$Month == "201802"]
# table(TX$Mark == "TX")

table(nchar(TX$Time) == 5)
where <- TX$Time[nchar(TX$Time) == 5]
TX$Time[nchar(TX$Time) == 5] <- paste(0, TX$Time[where], sep = "")

TX$Time <- paste(TX$Date, TX$Time, sep = " ")
TX <- select(TX, Time, everything())

# TX <- TX[TX$Date == 20180206, ]
# (TX <- filter(TX, Date == 20180206))

# TX <- TX[, c(1, 6, 7)]
TX <- select(TX, Time, Price, Volume)
TX <- na.omit(TX)

# TX$Date <- ymd(TX$Date)
# head(TX$Date)

# TX.OHLC <- TX
# TX <- arrange(TX, desc(Dime))
TX <- arrange(TX, Time)
TX <- TX[nchar(TX$Time) == 15, ]
table(nchar(TX$Time) == 15)

# write.table(TX, "F:/TX market trend analysis/TX_02_06.txt")

# TX$Time <- str_sub(TX$Time, 1, 13)
# TX$Time <- ymd_hms(TX$Time)

TX$Time <- strptime(TX$Time, format = "%Y%m%d %H%M%S")

TX.xts <- xts(TX[, -1], order.by = TX[, 1])
# TX.xts <- xts(TX[, c("Price", "Volume")], order.by = TX[, 1])

# TX$Time <- as.POSIXct(as.character(TX$Time), tz = "Asia/Taipei", format = "%Y%m%d%H%M%S")
class(TX.xts$Time); str(TX.xts); glimpse(TX.xts); head(TX.xts, 5); nrow(TX.xts)
# TX.xts <- na.omit(TX.xts)
# TX.xts <- unique(TX.xts)
# glimpse(TX.xts)

TX.xts %>% chartSeries()
TX.xts %>% chartSeries(up.col = "red", dn.col = "green", name = NULL)
TX.xts %>% to.daily() %>% chartSeries(up.col = "red", dn.col = "green", theme = "white")
TX.xts %>% to.hourly() %>% chartSeries()
TX.xts %>% to.hourly() %>% chartSeries(up.col = "red", dn.col = "green", theme = "white", name = NULL)
TX.xts %>% to.hourly() %>% chartSeries(TA = NULL, name = NULL)
TX.xts %>% to.minutes30() %>% chartSeries()
TX.xts %>% to.minutes15() %>% chartSeries()
TX.xts %>% to.minutes10() %>% chartSeries(up.col = "red", dn.col = "green", theme = "white")
TX.xts %>% to.minutes5() %>% chartSeries()
TX.xts %>% to.minutes() %>% chartSeries()
lineChart(TX.xts, line.type = "h")
addMACD()
addBBands(n = 20, sd = 2)
# Cl(TX.xts)
dailyReturn(TX.xts)
dailyReturn(TX.xts) %>% VaR(p = 0.95, method = "historical")
MA5 <- SMA(TX.xts$Price, n = 5)
MA20 <- SMA(TX.xts$Price, n = 20)
CCI20 <- CCI(TX.xts$Price, 20)
TX.xts %>% to.hourly() %>% chartSeries(up.col = "red", dn.col = "green", name = NULL, TA = "addSMA(20)")
TX.xts[MA5 > MA20] %>% index()
TX.xts %>% to.hourly() %>% chartSeries(TA = "addCCI(20)")

duration = "2018-02-06 03:00:00::2018-02-06 14:00:00"
# subset = "10:00:00::12:00:00"
candleChart(up.col = "red", dn.col = "green", theme = "white")
TX.xts[duration] %>% to.minutes10 () %>% candleChart(up.col = "red", dn.col = "green", theme = "white")
candleChart(TX.xts[duration], up.col = "red", dn.col = "green", theme = "white")
max(TX.xts$Price[duration])
min(TX.xts$Price[duration])
# seriesHi(TX.xts[duration])
# seriesLo(TX.xts)
last(first(TX.xts, "5 hours"), "30 minutes")

# TX.xts %>% index()

TX.xts$Price %>% to.hourly() %>% chartSeries()

TX.Day <- TX.xts$Price %>% to.daily() %>% data.frame()
TX.Day <- mutate(TX.Day, Time = rownames(TX.Day))
head(TX.Day, 10)

TX.Day <- TX.Day %>% mutate(Year = year(Time))
TX.Day <- TX.Day %>% mutate(Month = month(Time))
TX.Day <- TX.Day %>% mutate(Day = day(Time))
colnames(TX.Day) <- c("Open", "High", "Low", "Close", "Time", "Year", "Month", "Day")
TX.Day <- select(TX.Day, Time, Year, Month, Day, High, Low)

TX.Day <- TX.Day %>% select(Day, High, Low)

ggplot(TX.Day, aes(x = as.factor(Day), y = High)) + geom_point(size = 5)
# ggplot(TX.Day, aes(x = as.factor(Day), y = High)) + geom_point(size = 6, color = as.factor(Day))

TX.Hour <- TX.xts$Price %>% to.hourly() %>% data.frame()
TX.Hour <- mutate(TX.Hour, Time = rownames(TX.Hour))
head(TX.Hour, 10)

TX.Hour <- TX.Hour %>% mutate(Year = year(Time))
TX.Hour <- TX.Hour %>% mutate(Month = month(Time))
TX.Hour <- TX.Hour %>% mutate(Day = day(Time))
TX.Hour <- TX.Hour %>% mutate(Hour = hour(Time))
TX.Hour <- TX.Hour %>% mutate(Minute = minute(Time))
TX.Hour <- TX.Hour %>% mutate(Second = second(Time))
colnames(TX.Hour) <- c("Open", "High", "Low", "Close", "Time", "Year", "Month", "Day", "Hour", "Minute", "Second")
TX.Hour <- select(TX.Hour, Time, Year, Month, Day, Hour, Minute, Second, High, Low)
table(TX.Hour$Hour)
TX.Hour <- TX.Hour %>% filter(Hour >= 9)

Hour.Price <- TX.Hour %>% select(Month, Day, Hour, High, Low)

# ggplot(Hour.Price, aes(x = Day, y = High, color = Hour)) + geom_point()
# level <- as.factor(Hour.Price$Hour) %>% levels()
# Plots <- ggplot(Hour.Price, aes(x = as.factor(Day), y = High)) + geom_point()
Plots <- ggplot(Hour.Price, aes(x = as.factor(Day), y = High))
Plots <- Plots + geom_text(aes(label = Hour), size = 12)
Plots <- Plots + xlab("Day") + ylab("Day High")
Plots

Plots <- ggplot(Hour.Price, aes(x = as.factor(Day), y = Low))
Plots <- Plots + geom_text(aes(label = Hour), size = 12)
Plots <- Plots + xlab("Day") + ylab("Day Low")
Plots
  
# ddply(Hour.Price, c("Hour"), function(Hour.Price) mean(Hour.Price$High))
Hour.High.Mean <- Hour.Price %>% group_by(Hour) %>% summarize(High.Mean = mean(High))
Hour.High.Mean %>% 
  ggplot(aes(x = Hour, y = High.Mean)) +
  geom_line() +
  xlab("Hour") + ylab("Mean of Hour's Highest Price")

Hour.Low.Mean <- Hour.Price %>% group_by(Hour) %>% summarize(Low.Mean = mean(Low))
Hour.Low.Mean %>% 
  ggplot(aes(x = Hour, y = Low.Mean)) +
  geom_line() +
  xlab("Hour") + ylab("Mean of Hour's Lowest Price")

Hour.Range <- Hour.Price %>% group_by(Hour) %>% summarize(Range = mean(High) - mean(Low))
Hour.Range %>% 
  ggplot(aes(x = Hour, y = Range)) +
  geom_bar(stat = "identity") +
  xlab("Hour")

Day.Range <- Hour.Price %>% group_by(Day) %>% summarize(Range = mean(High) - mean(Low))
Day.Range %>% 
  ggplot(aes(x = as.factor(Day), y = Range)) +
  geom_bar(stat = "identity") +
  xlab("Day")

Day.MH <- Hour.Price %>% select(-Low) %>% group_by(Month, Day) %>% mutate(MH = max(High))
Day.MH$MH[Day.MH$MH == Day.MH$High] <- "Highest"
Day.MH$MH[Day.MH$MH != "Highest"] <- "Not Highest"
head(Day.MH, 12)
table(Day.MH$Hour, Day.MH$MH)

Day.ML <- Hour.Price %>% select(-High) %>% group_by(Month, Day) %>% mutate(ML = min(Low))
Day.ML$ML[Day.ML$ML == Day.ML$Low] <- "Lowest"
Day.ML$ML[Day.ML$ML != "Lowest"] <- "Not Lowest"
head(Day.ML, 12)
table(Day.ML$Hour, Day.ML$ML)

# install.packages("gridExtra")
library(gridExtra)
grid.table(table(Day.MH$Hour, Day.MH$MH))
grid.table(table(Day.ML$Hour, Day.ML$ML))


















# TX.hour %>% 
#   group_by(Month, Day) %>% 
#   select(Hour, High) -> Day.High

# Hour <- Day.High$Hour
# High <- Day.High$High
# Day.High.M <- matrix(nrow = length(Hour), ncol = 2)
# Day.High.M[, 1] <- Hour
# Day.High.M[, 2] <- High













TX[which(!is.finite(TX))] <- NA
TX <- na.omit(TX)

# TX <- as.xts(TX$Time, order.by = TX$Time, format = "%Y%m%d%H%M%S")
# TX <- as.xts(TX, order.by = TX$Time, format = "%Y%m%d%H%M%S")
# TX = as.xts(read.zoo(TX))

plot(TX$Time, TX$Price, type = "l")
duration = "2018-02-22 11:00:00"
TX$Problem <- filter(TX, time > hour(TX$time) & time < hour(TX$time))





TX$time <- ymd_hms(TX$time)
# TX$date.time <- as.Date(TX$date.time, format = "%Y-%m-%d")

# TX$time <- subset(TX, time <= 134500 & time >= 84500)
# (TX$time <- filter(TX, time <= 134500 & time >= 84500))
# period.cut <- c(84500, 90000, 91500, 93000, 94500, 100000)
# period.cut.name <- c("9:00", "9:15", "9:30", "9:45", "10:00")
# TX$period <- cut(TX$time, breaks = period.cut, labels = period.cut.name)

# TX.9 <- subset(TX, time < 100000 & time >= 90000)
# TX.9.15 <- TX$date.time["2018-02-06 08:45:00"/"2018-02-06 09:00:00"]

TX <- TX %>% mutate(year = year(time))
TX <- TX %>% mutate(month = month(time))
TX <- TX %>% mutate(day = day(time))
TX <- TX %>% mutate(hour = hour(time))
TX <- TX %>% mutate(minute = minute(time))
TX <- TX %>% mutate(second = second(time))
# TX$hour <- hour(TX$date.time)
# TX$minute <- minute(TX$date.time)
# TX$second <- second(TX$date.time)

# View(TX)
str(TX)
glimpse(TX)
summary(TX$day)
summary(TX$hour)
summary(TX$minute)
summary(TX$second)
TX <- na.omit(TX)

# TX.xts <- xts(x = TX, order.by = as.Date(TX$date.time) + 1:nrow(TX))
# glimpse(TX.xts)

TX %>% 
  ggplot(aes(x = time, y = price)) +
  geom_line() +
  xlab("Time") +
  ylab("Price") +
  theme(axis.title.x = element_text(size = 16, colour = "darkred", face = "italic")) +
  theme(axis.title.y = element_text(size = 16, colour = "darkred", face = "italic"))

TX %>% 
  filter(day == 23 & hour %in% c(8:13)) %>% 
  ggplot(aes(x = time, y = price)) +
  geom_line()

TX %>% 
  filter(day == 6 & hour == 10 & minute %in% c(0:14)) %>% 
  ggplot(aes(x = time, y = price)) +
  geom_line()

# table(TX$price < 5000)
# TX$price[TX$price < 5000] <- NA

TX.10.15 <- filter(TX, TX$hour == 10 & minute %in% c(0:14))
TX.10.30 <- filter(TX, TX$hour == 10 & minute %in% c(15:29))

plot(TX.10.15$time, TX.10.15$price, type = "l")
plot(TX.10.30$time, TX.10.30$price, type = "l")

# TX.9 <- TX.9[!duplicated(TX.9$time), ]
str(TX.9.15)
plot(TX.9.15$price ~ TX.9.15$time, type = "l")
(TX.9.15.plot <- ggplot(TX.9.15, aes(x = time, y = price)) + geom_line())

TX.9$time <- strptime(sprintf("%06d", TX.9$time), "%H%M%S")
TX$time <- hms(TX$time)
# TX$time <- as.Date(TX, order.by = TX$time, format = "%h%m%s")




### TX <- select(TX, c(date, mark, month, time, price, volume))
TX$ten <- subset(TX, TX$mark == "TX" & TX$month == 201802 & (TX$time>=90000 & TX$time<100000))
TX$hour[TX$time>=90000 & TX$time<100000] <- ten 
table(TX$hour)











# Daily Data from DQ2

setwd("F:/")
getwd()
# dir()

# TX.18.02 <- read.csv("TX_2018_02.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
TX.18.02 <- read.csv("F:/TX market trend analysis/TX_2018_02.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# TX.18.02 <- read.csv("TX_2018_02.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
head(TX.18.02, 15)
# TX.18.02 <- TX.18.02[-c(1:2), ]
colnames(TX.18.02) <- c("Date", "Time", "Open", "Close", "High", "Low", "Volume")
TX.18.02$Date <- ymd(TX.18.02$Date)
TX.18.02$Time <- hms(TX.18.02$Time)
TX.18.02$Time <- ymd_hms(paste(TX.18.02$Date, TX.18.02$Time, sep = " "))
TX.18.02 <- arrange(TX.18.02, Time)
TX.18.02 <- select(TX.18.02, Time, Open, High, Low, Close, Volume)
TX.18.02 <- TX.18.02 %>% na.omit()
# TX.18.02 <- TX.18.02 %>% na.omit() %>% read.zoo %>% as.xts()

TX.18.02.xts <- xts(TX.18.02[, c("Open", "Close", "High", "Low")], order.by = TX.18.02[, 1])
# TX.18.02.xts <- xts(TX.18.02[, c("Open", "Close", "High", "Low", "Volume")], order.by = TX.18.02[, 1])
# TX.18.02.xts <- xts(TX.18.02[, -1], order.by = TX.18.02[, 1])

# c(1:nrow(TX.18.02)) %>% as.character()
# rownames(TX.18.02.xts) <- TX.18.02[[1]]
# as.xts(TX.18.02.xts)
head(TX.18.02.xts, 20); first(TX.18.02.xts, "10 hours"); glimpse(TX.18.02.xts)

# plot(TX.18.02.xts[["Time"]], TX.18.02.xts[["Close"]], type = "l")
# TX.18.02.xts %>% 
#   ggplot(aes(x = Time, y = Close)) + geom_line()

chartSeries(TX.18.02.xts)
TX.18.02.xts %>% to.daily() %>% chartSeries()
TX.18.02.xts %>% to.hourly() %>% chartSeries()
TX.18.02.xts %>% to.minutes30() %>% chartSeries()
TX.18.02.xts %>% to.minutes10() %>% chartSeries()
TX.18.02.xts %>% to.minutes5() %>% chartSeries()
TX.18.02.xts %>% to.minutes3() %>% chartSeries()
lineChart(TX.18.02.xts, line.type = "h")
addMACD()
addBBands()
Cl(TX.18.02.xts)
dailyReturn(TX.18.02.xts)
dailyReturn(TX.18.02.xts) %>% VaR(p = 0.95, method = "historical")
MA5 <- SMA(TX.18.02.xts$Close, n = 5)
MA20 <- SMA(TX.18.02.xts$Close, n = 20)
CCI20 <- CCI(TX.18.02.xts$Close, 20)
chartSeries(TX.18.02.xts, TA = "addSMA(5);addSMA(20);addCCI(20)")
TX.18.02.xts[MA5 > MA20] %>% index()

subset = "2018-02-09 10:00:00::2018-02-09 12:00:00"
subset = "10:00:00::12:00:00"
candleChart(TX.18.02.xts, up.col = "red", dn.col = "green", theme = "white", TA = "addBBands()", subset = "2018-02-09 10:00:00::2018-02-09 12:00:00")
max(TX.18.02.xts[subset])
min(TX.18.02.xts[subset])
seriesHi(TX.18.02.xts[subset])
seriesLo(TX.18.02.xts)
last(first(TX.18.02.xts, "5 hours"), "30 minutes")

subset.1 = "09:00:00::09:14:59"
subset.2 = "09:15:00::09:29:59"
subset.3 = "09:30:00::09:44:59"
subset.4 = "09:45:00::09:59:59"

min.1 <- TX.18.02.xts[subset.1] %>% Cl() %>% min()
min.2 <- TX.18.02.xts[subset.2] %>% Cl() %>% min()
min.3 <- TX.18.02.xts[subset.3] %>% Cl() %>% min()
min.4 <- TX.18.02.xts[subset.4] %>% Cl() %>% min()

c(min.1, min.2, min.3, min.4) %>% min()
min(min.1, min.2, min.3, min.4)

endpoints(TX.18.02.xts, on = "hours")

TX.18.02.xts$MACD <- MACD(TX.18.02[, "Close"], 12, 26, 9, maType = "EMA")
TX.18.02.xts$RSI <- RSI(TX.18.02[,"Close"])
TX.18.02.xts$EMA <- EMA(TX.18.02[, "Cclose"], n = 20)
TX.18.02.xts$SMA <- SMA(TX.18.02[, "Close"], n = 20)

plot(TX.18.02.xts$Time, TX.18.02.xts$Close, type = "l")
par(new = TRUE)
plot(TX.18.02.xts$Time, TX.18.02.xts$SMA, type = "l", col = "red")
plot(TX.18.02.xts$Open, type = "l", xlab = "time", ylab = "price")

# par(mfrow = c(1, 2))
par(fig=c(0, 1, 0.3, 1), new = TRUE)
plot(TX.18.02.xts$close, type = "l", col = "red", xlab = "", ylab = "", pin=c(4, 5))
par(fig=c(0, 1, 0, 0.2), new = TRUE)
barplot(table(TX.18.02.xts$Volume), xlab = "", ylab = "", col = "green")



#######
duration = "2018-02-07::2018-02-09"

TX.18.02.xts %>% 
  ggplot(aes(x = duration, y = close)) + geom_line()



write.zoo(TX.18.02, file = "TX_2018_02_zoo.csv", sep = ",")
read.zoo(TX.18.02, format = "%Y-%m-%d %h:%m:%S")

setSymbolLookup(TX.18.02 = list(name = "TX.18.02", src = "csv", format = "%Y-%m-%d %h:%m:%S"))
saveSymbolLookup(file = "TX_2018_02_zoo.csv")
loadSymbolLookup(file = "TX_2018_02_zoo.csv")
# TX.18.02 <- getSymbols("TX_2018_02_zoo", auto.assign = FALSE)
# barChart(TX.18.02)






















TX.18.02 <- read.zoo("TX_2018_02_zoo.csv", sep = ",", format = "%Y-%m-%d %h:%m:%S", tz = "UTC", header = TRUE, index.column = 1)
head(TX.18.02)

xx<- as.xts(zz)

TX.18.02 = as.xts(read.zoo("TX_2018_02_zoo.csv", header = T))


# new sessions call loadSymbolLookup(file="mysymbols.rda")

getSymbols("SPY", src = "google")
write.zoo(SPY, file="SPY.csv", sep=",")

# set symbol lookup

# call getSymbols(.csv) with auto.assign=FALSE
spy <- getSymbols("SPY", auto.assign=FALSE)
barChart(spy)



SE
spy <- getSymbols("SPY", auto.assign=FALSE)
barChart(spy)



