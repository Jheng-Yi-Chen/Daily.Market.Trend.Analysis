
setwd("C:")
getwd()

library(quantmod)
library(zoo)

Data <- read.csv("Data.csv", header = TRUE)
keep <- seq(2, nrow(Data), by = 5)
Data <- Data[keep, ]
Data <- Data[, c(1, 4, 5, 6, 7, 10)]
colnames(Data) <- c("Date", "Open", "High", "Low", "Close", "Volume")
head(futures2016, 10)

Data$Date <- gsub("/", "-", Data$Date)
Data$Date <- as.Date(Data$Date, format = "%Y/%m/%d/")

