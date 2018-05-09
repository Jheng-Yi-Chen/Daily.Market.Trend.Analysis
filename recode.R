library(dplyr)
library(stringr)

##################################################

# P6A <- read.csv("#6A_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# P6B <- read.csv("#6B_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
P6A <- read.table("#6A_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
P6B <- read.table("#6B_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

##################################################

# P5A[is.na(P5A)] <- ""
# P5B[is.na(P5B)] <- ""

P6A <- select(P6A, -grep("x", names(P6A)))
P6B <- select(P6B, -grep("x", names(P6B)))

P6A <- P6A[-c(1, 2), ]
P6B <- P6B[-c(1, 2), ]

##################################################

# # P6A$P6Q5_11 <- as.numeric(as.character(P6A$P6Q5_11))
# P6A$P6Q5_11 # 308 is "|"
# class(P6A$P6Q5_11)
# where <-  nchar(as.vector(P6A$P6Q5_11)) > 1
# where
# # P6A$P6Q5_11 <- as.vector(P6A$P6Q5_11)
# P6A$P6Q5_11[where] <- "11"
# P6A$P6Q5_11
# P6A$P6Q5_11[P6A$P6Q5_11 == "｜"] <- "11"
# P6A$P6Q5_11

P6A$P6Q5_11[nchar(P6A$P6Q5_11) > 0] <- "11"
P6A$P6Q5_11

# where <- P6A$P6Q5_3 == "2"
# P6A$P6Q5_3[where] <- "3"

P6A$P6Q5_3[P6A$P6Q5_3 == "2"] <- "3"
P6A$P6Q5_4[P6A$P6Q5_4 == "3"] <- "4"

# for (x in P6A$P6Q5_11) {
#   x <-  nchar(as.vector(P6A$P6Q5_11)) > 1
#     if (x == TRUE) {
#       gsub(x, 11, P6A$P6Q5_11)
#   }
# }

# P6A$P6Q5_11 %>% 
#   as.numeric() %>% 
#   nchar() > 1 %>% 
#   recode(11)

# P6A$P6Q5_11[which(nchar(as.vector(P6A$P6Q5_11)) > 1)] <- "11"

# nchar(as.vector(P6A$P6Q5_11))

P6A$P6Q5 <- paste(P6A$P6Q5_1, P6A$P6Q5_3, P6A$P6Q5_4, P6A$P6Q5_5, P6A$P6Q5_6, P6A$P6Q5_7, P6A$P6Q5_8, P6A$P6Q5_9, P6A$P6Q5_10, P6A$P6Q5_11, sep = ",")
# P6A$P6Q5 <- str_c(P6A$P6Q5_1, P6A$P6Q5_3, P6A$P6Q5_4, P6A$P6Q5_5, P6A$P6Q5_6, P6A$P6Q5_7, P6A$P6Q5_8, P6A$P6Q5_9, P6A$P6Q5_10, P6A$P6Q5_11, sep = "")
P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5); P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5); P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5); P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5); P6A$P6Q5 <- gsub(",,", ",", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",3,", "3,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",4,", "4,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",5,", "5,", P6A$P6Q5)
# P6A$P6Q5 <- gsub(",7,", "7,", P6A$P6Q5)

P6A$P6Q5; class(P6A$P6Q5)

# str_count(P6A$P6Q5)
# P6A$P6Q5 <- strsplit(P6A$P6Q5, ",")
# P6A$P6Q5 <- grep(",,", "", P6A$P6Q5)

# P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","]
# P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","]

P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q5[str_sub(string = P6A$P6Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","] <- 
  str_replace(string =  P6A$P6Q5[str_sub(string = P6A$P6Q5, start = nchar(P6A$P6Q5), end = nchar(P6A$P6Q5)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q5

##################################################

P6B$P6Q5_11; class(P6B$P6Q5_11)
P6B$P6Q5_11[nchar(P6B$P6Q5_11) > 0] <- "11"
P6B$P6Q5_3[P6B$P6Q5_3 == "2"] <- "3"
P6B$P6Q5_4[P6B$P6Q5_4 == "3"] <- "4"
P6B$P6Q5_5[P6B$P6Q5_5 == "4"] <- "5"

P6B$P6Q5 <- paste(P6B$P6Q5_1, P6B$P6Q5_3, P6B$P6Q5_4, P6B$P6Q5_5, P6B$P6Q5_6, P6B$P6Q5_7, P6B$P6Q5_8, P6B$P6Q5_9, P6B$P6Q5_10, P6B$P6Q5_11, sep = ",")

P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5); P6B$P6Q5 <- gsub(",,", ",", P6B$P6Q5)
class(P6B$P6Q5)

P6B$P6Q5[str_sub(string = P6B$P6Q5, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q5[str_sub(string = P6B$P6Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q5[str_sub(string = P6B$P6Q5, start = nchar(P6B$P6Q5), end = nchar(P6B$P6Q5)) == ","] <- 
  str_replace(string =  P6B$P6Q5[str_sub(string = P6B$P6Q5, start = nchar(P6B$P6Q5), end = nchar(P6B$P6Q5)) == ","], pattern = "\\,$", replacement = "")

##################################################

P6A$P6Q12 <- paste(P6A$P6Q12_1, P6A$P6Q12_2, P6A$P6Q12_3, P6A$P6Q12_4, P6A$P6Q12_5, P6A$P6Q12_6, P6A$P6Q12_7, P6A$P6Q12_8, P6A$P6Q12_9, P6A$P6Q12_10, P6A$P6Q12_99, sep = ",")

P6A$P6Q12 <- gsub(",,", ",", P6A$P6Q12)
P6A$P6Q12 <- gsub(",,", ",", P6A$P6Q12)
P6A$P6Q12 <- gsub(",,", ",", P6A$P6Q12)

P6A$P6Q12[str_sub(string = P6A$P6Q12, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q12[str_sub(string = P6A$P6Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q12[str_sub(string = P6A$P6Q12, start = nchar(P6A$P6Q12), end = nchar(P6A$P6Q12)) == ","] <- 
  str_replace(string =  P6A$P6Q12[str_sub(string = P6A$P6Q12, start = nchar(P6A$P6Q12), end = nchar(P6A$P6Q12)) == ","], pattern = "\\,$", replacement = "")

##################################################

P6B$P6Q12 <- paste(P6B$P6Q12_1, P6B$P6Q12_2, P6B$P6Q12_3, P6B$P6Q12_4, P6B$P6Q12_5, P6B$P6Q12_6, P6B$P6Q12_7, P6B$P6Q12_8, P6B$P6Q12_9, P6B$P6Q12_10, P6B$P6Q12_99, sep = ",")

P6B$P6Q12 <- gsub(",,", ",", P6B$P6Q12)
P6B$P6Q12 <- gsub(",,", ",", P6B$P6Q12)

P6B$P6Q12[str_sub(string = P6B$P6Q12, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q12[str_sub(string = P6B$P6Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q12[str_sub(string = P6B$P6Q12, start = nchar(P6B$P6Q12), end = nchar(P6B$P6Q12)) == ","] <- 
  str_replace(string =  P6B$P6Q12[str_sub(string = P6B$P6Q12, start = nchar(P6B$P6Q12), end = nchar(P6B$P6Q12)) == ","], pattern = "\\,$", replacement = "")

##################################################

P6A$P6Q13_99[P6A$P6Q13_99 == "1"] <- "99"
P6A$P6Q13_1[P6A$P6Q13_1 == "2"] <- "1"
P6A$P6Q13_2[P6A$P6Q13_2 == "3"] <- "2"
P6A$P6Q13_3[P6A$P6Q13_3 == "4"] <- "3"
P6A$P6Q13_4[P6A$P6Q13_4 == "5"] <- "4"
P6A$P6Q13_5[P6A$P6Q13_5 == "6"] <- "5"
P6A$P6Q13_6[P6A$P6Q13_6 == "7"] <- "6"
P6A$P6Q13_7[P6A$P6Q13_7 == "8"] <- "7"
P6A$P6Q13_8[P6A$P6Q13_8 == "9"] <- "8"
P6A$P6Q13_9[P6A$P6Q13_9 == "10"] <- "9"
P6A$P6Q13_10[P6A$P6Q13_10 == "11"] <- "10"
P6A$P6Q13_98[nchar(P6A$P6Q13_98) > 0] <- "98"

P6A$P6Q13 <- paste(P6A$P6Q13_1, P6A$P6Q13_2, P6A$P6Q13_3, P6A$P6Q13_4, P6A$P6Q13_5, P6A$P6Q13_6, P6A$P6Q13_7, P6A$P6Q13_8, P6A$P6Q13_9, P6A$P6Q13_10, P6A$P6Q13_98, P6A$P6Q13_99, sep = ",")
P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13
P6A$P6Q13 <- gsub(",,", ",", P6A$P6Q13); P6A$P6Q13

P6A$P6Q13[str_sub(string = P6A$P6Q13, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q13[str_sub(string = P6A$P6Q13, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q13[str_sub(string = P6A$P6Q13, start = nchar(P6A$P6Q13), end = nchar(P6A$P6Q13)) == ","] <- 
  str_replace(string =  P6A$P6Q13[str_sub(string = P6A$P6Q13, start = nchar(P6A$P6Q13), end = nchar(P6A$P6Q13)) == ","], pattern = "\\,$", replacement = "")

##################################################

P6B$P6Q13_99[P6B$P6Q13_99 == "1"] <- "99"
P6B$P6Q13_1[P6B$P6Q13_1 == "2"] <- "1"
P6B$P6Q13_2[P6B$P6Q13_2 == "3"] <- "2"
P6B$P6Q13_3[P6B$P6Q13_3 == "4"] <- "3"
P6B$P6Q13_4[P6B$P6Q13_4 == "5"] <- "4"
P6B$P6Q13_5[P6B$P6Q13_5 == "6"] <- "5"
P6B$P6Q13_6[P6B$P6Q13_6 == "7"] <- "6"
P6B$P6Q13_7[P6B$P6Q13_7 == "8"] <- "7"
P6B$P6Q13_8[P6B$P6Q13_8 == "9"] <- "8"
P6B$P6Q13_9[P6B$P6Q13_9 == "10"] <- "9"
P6B$P6Q13_10[P6B$P6Q13_10 == "11"] <- "10"
P6B$P6Q13_98[nchar(P6B$P6Q13_98) > 0] <- "98"

P6B$P6Q13 <- paste(P6B$P6Q13_1, P6B$P6Q13_2, P6B$P6Q13_3, P6B$P6Q13_4, P6B$P6Q13_5, P6B$P6Q13_6, P6B$P6Q13_7, P6B$P6Q13_8, P6B$P6Q13_9, P6B$P6Q13_10, P6B$P6Q13_98, P6B$P6Q13_99, sep = ",")
P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13
P6B$P6Q13 <- gsub(",,", ",", P6B$P6Q13); P6B$P6Q13

P6B$P6Q13[str_sub(string = P6B$P6Q13, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q13[str_sub(string = P6B$P6Q13, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q13[str_sub(string = P6B$P6Q13, start = nchar(P6B$P6Q13), end = nchar(P6B$P6Q13)) == ","] <- 
  str_replace(string =  P6B$P6Q13[str_sub(string = P6B$P6Q13, start = nchar(P6B$P6Q13), end = nchar(P6B$P6Q13)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q13

##################################################
# P6A$P6Q14

P6A$P6Q14_99[P6A$P6Q14_99 == "1"] <- "99"
P6A$P6Q14_1[P6A$P6Q14_1 == "2"] <- "1"
P6A$P6Q14_2[P6A$P6Q14_2 == "3"] <- "2"
P6A$P6Q14_3[P6A$P6Q14_3 == "4"] <- "3"
P6A$P6Q14_4[P6A$P6Q14_4 == "5"] <- "4"
P6A$P6Q14_5[P6A$P6Q14_5 == "6"] <- "5"
P6A$P6Q14_6[P6A$P6Q14_6 == "7"] <- "6"
P6A$P6Q14_7[P6A$P6Q14_7 == "8"] <- "7"
P6A$P6Q14_8[P6A$P6Q14_8 == "9"] <- "8"
P6A$P6Q14_9[P6A$P6Q14_9 == "10"] <- "9"
P6A$P6Q14_10[P6A$P6Q14_10 == "11"] <- "10"
P6A$P6Q14_98[nchar(P6A$P6Q14_98) > 0] <- "98"

P6A$P6Q14 <- paste(P6A$P6Q14_1, P6A$P6Q14_2, P6A$P6Q14_3, P6A$P6Q14_4, P6A$P6Q14_5, P6A$P6Q14_6, P6A$P6Q14_7, P6A$P6Q14_8, P6A$P6Q14_9, P6A$P6Q14_10, P6A$P6Q14_98, P6A$P6Q14_99, sep = ",")
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14
P6A$P6Q14 <- gsub(",,", ",", P6A$P6Q14); P6A$P6Q14

P6A$P6Q14[str_sub(string = P6A$P6Q14, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q14[str_sub(string = P6A$P6Q14, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q14[str_sub(string = P6A$P6Q14, start = nchar(P6A$P6Q14), end = nchar(P6A$P6Q14)) == ","] <- 
  str_replace(string =  P6A$P6Q14[str_sub(string = P6A$P6Q14, start = nchar(P6A$P6Q14), end = nchar(P6A$P6Q14)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q14

##################################################
# P6B$P6Q14

P6B$P6Q14_99[P6B$P6Q14_99 == "1"] <- "99"
P6B$P6Q14_1[P6B$P6Q14_1 == "2"] <- "1"
P6B$P6Q14_2[P6B$P6Q14_2 == "3"] <- "2"
P6B$P6Q14_3[P6B$P6Q14_3 == "4"] <- "3"
P6B$P6Q14_4[P6B$P6Q14_4 == "5"] <- "4"
P6B$P6Q14_5[P6B$P6Q14_5 == "6"] <- "5"
P6B$P6Q14_6[P6B$P6Q14_6 == "7"] <- "6"
P6B$P6Q14_7[P6B$P6Q14_7 == "8"] <- "7"
P6B$P6Q14_8[P6B$P6Q14_8 == "9"] <- "8"
P6B$P6Q14_9[P6B$P6Q14_9 == "10"] <- "9"
P6B$P6Q14_10[P6B$P6Q14_10 == "11"] <- "10"
P6B$P6Q14_98[nchar(P6B$P6Q14_98) > 0] <- "98"

P6B$P6Q14 <- paste(P6B$P6Q14_1, P6B$P6Q14_2, P6B$P6Q14_3, P6B$P6Q14_4, P6B$P6Q14_5, P6B$P6Q14_6, P6B$P6Q14_7, P6B$P6Q14_8, P6B$P6Q14_9, P6B$P6Q14_10, P6B$P6Q14_98, P6B$P6Q14_99, sep = ",")
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14
P6B$P6Q14 <- gsub(",,", ",", P6B$P6Q14); P6B$P6Q14

P6B$P6Q14[str_sub(string = P6B$P6Q14, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q14[str_sub(string = P6B$P6Q14, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q14[str_sub(string = P6B$P6Q14, start = nchar(P6B$P6Q14), end = nchar(P6B$P6Q14)) == ","] <- 
  str_replace(string =  P6B$P6Q14[str_sub(string = P6B$P6Q14, start = nchar(P6B$P6Q14), end = nchar(P6B$P6Q14)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q14

##################################################
# P6A$P6Q15

P6A$P6Q15_99[P6A$P6Q15_99 == "1"] <- "99"
P6A$P6Q15_1[P6A$P6Q15_1 == "2"] <- "1"
P6A$P6Q15_2[P6A$P6Q15_2 == "3"] <- "2"
P6A$P6Q15_3[P6A$P6Q15_3 == "4"] <- "3"
P6A$P6Q15_4[P6A$P6Q15_4 == "5"] <- "4"
P6A$P6Q15_5[P6A$P6Q15_5 == "6"] <- "5"
P6A$P6Q15_6[P6A$P6Q15_6 == "7"] <- "6"
P6A$P6Q15_7[P6A$P6Q15_7 == "8"] <- "7"
P6A$P6Q15_8[P6A$P6Q15_8 == "9"] <- "8"
P6A$P6Q15_9[P6A$P6Q15_9 == "10"] <- "9"
P6A$P6Q15_10[P6A$P6Q15_10 == "11"] <- "10"
P6A$P6Q15_98[nchar(P6A$P6Q15_98) > 0] <- "98"

P6A$P6Q15 <- paste(P6A$P6Q15_1, P6A$P6Q15_2, P6A$P6Q15_3, P6A$P6Q15_4, P6A$P6Q15_5, P6A$P6Q15_6, P6A$P6Q15_7, P6A$P6Q15_8, P6A$P6Q15_9, P6A$P6Q15_10, P6A$P6Q15_98, P6A$P6Q15_99, sep = ",")
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15
P6A$P6Q15 <- gsub(",,", ",", P6A$P6Q15); P6A$P6Q15

P6A$P6Q15[str_sub(string = P6A$P6Q15, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q15[str_sub(string = P6A$P6Q15, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q15[str_sub(string = P6A$P6Q15, start = nchar(P6A$P6Q15), end = nchar(P6A$P6Q15)) == ","] <- 
  str_replace(string =  P6A$P6Q15[str_sub(string = P6A$P6Q15, start = nchar(P6A$P6Q15), end = nchar(P6A$P6Q15)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q15

##################################################
# P6B$P6Q15

P6B$P6Q15_99[P6B$P6Q15_99 == "1"] <- "99"
P6B$P6Q15_1[P6B$P6Q15_1 == "2"] <- "1"
P6B$P6Q15_2[P6B$P6Q15_2 == "3"] <- "2"
P6B$P6Q15_3[P6B$P6Q15_3 == "4"] <- "3"
P6B$P6Q15_4[P6B$P6Q15_4 == "5"] <- "4"
P6B$P6Q15_5[P6B$P6Q15_5 == "6"] <- "5"
P6B$P6Q15_6[P6B$P6Q15_6 == "7"] <- "6"
P6B$P6Q15_7[P6B$P6Q15_7 == "8"] <- "7"
P6B$P6Q15_8[P6B$P6Q15_8 == "9"] <- "8"
P6B$P6Q15_9[P6B$P6Q15_9 == "10"] <- "9"
P6B$P6Q15_10[P6B$P6Q15_10 == "11"] <- "10"
P6B$P6Q15_98[nchar(P6B$P6Q15_98) > 0] <- "98"

P6B$P6Q15 <- paste(P6B$P6Q15_1, P6B$P6Q15_2, P6B$P6Q15_3, P6B$P6Q15_4, P6B$P6Q15_5, P6B$P6Q15_6, P6B$P6Q15_7, P6B$P6Q15_8, P6B$P6Q15_9, P6B$P6Q15_10, P6B$P6Q15_98, P6B$P6Q15_99, sep = ",")
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15
P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15

P6B$P6Q15[str_sub(string = P6B$P6Q15, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q15[str_sub(string = P6B$P6Q15, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q15[str_sub(string = P6B$P6Q15, start = nchar(P6B$P6Q15), end = nchar(P6B$P6Q15)) == ","] <- 
  str_replace(string =  P6B$P6Q15[str_sub(string = P6B$P6Q15, start = nchar(P6B$P6Q15), end = nchar(P6B$P6Q15)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q15

##################################################
# P6A$P6Q16

# for(x in table) {
#   function() {
#     table_function <- tablb()
#   }; x <- seq(1:12)
# }
  
table(P6A$P6Q16_1)
table(P6A$P6Q16_2)
table(P6A$P6Q16_3)
table(P6A$P6Q16_4)
table(P6A$P6Q16_5)
table(P6A$P6Q16_6)
table(P6A$P6Q16_7)
table(P6A$P6Q16_8)
table(P6A$P6Q16_9)
table(P6A$P6Q16_10)
table(P6A$P6Q16_11)
table(P6A$P6Q16_12)

P6A$P6Q16_12[nchar(P6A$P6Q16_12) > 0] <- "12"

P6A$P6Q16 <- paste(P6A$P6Q16_1, P6A$P6Q16_2, P6A$P6Q16_3, P6A$P6Q16_4, P6A$P6Q16_5, P6A$P6Q16_6, P6A$P6Q16_7, P6A$P6Q16_8, P6A$P6Q16_9, P6A$P6Q16_10, P6A$P6Q16_11, P6A$P6Q16_12, sep = ",")
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16
P6A$P6Q16 <- gsub(",,", ",", P6A$P6Q16); P6A$P6Q16

P6A$P6Q16[str_sub(string = P6A$P6Q16, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q16[str_sub(string = P6A$P6Q16, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q16[str_sub(string = P6A$P6Q16, start = nchar(P6A$P6Q16), end = nchar(P6A$P6Q16)) == ","] <- 
  str_replace(string =  P6A$P6Q16[str_sub(string = P6A$P6Q16, start = nchar(P6A$P6Q16), end = nchar(P6A$P6Q16)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q16

##################################################
# P6B$P6Q16

table(P6B$P6Q16_1)
table(P6B$P6Q16_2)
table(P6B$P6Q16_3)
table(P6B$P6Q16_4)
table(P6B$P6Q16_5)
table(P6B$P6Q16_6)
table(P6B$P6Q16_7)
table(P6B$P6Q16_8)
table(P6B$P6Q16_9)
table(P6B$P6Q16_10)
table(P6B$P6Q16_11)
table(P6B$P6Q16_12)

P6B$P6Q16_12[nchar(P6B$P6Q16_12) > 0] <- "12"

P6B$P6Q16 <- paste(P6B$P6Q16_1, P6B$P6Q16_2, P6B$P6Q16_3, P6B$P6Q16_4, P6B$P6Q16_5, P6B$P6Q16_6, P6B$P6Q16_7, P6B$P6Q16_8, P6B$P6Q16_9, P6B$P6Q16_10, P6B$P6Q16_11, P6B$P6Q16_12, sep = ",")
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16
P6B$P6Q16 <- gsub(",,", ",", P6B$P6Q16); P6B$P6Q16

P6B$P6Q16[str_sub(string = P6B$P6Q16, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q16[str_sub(string = P6B$P6Q16, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q16[str_sub(string = P6B$P6Q16, start = nchar(P6B$P6Q16), end = nchar(P6B$P6Q16)) == ","] <- 
  str_replace(string =  P6B$P6Q16[str_sub(string = P6B$P6Q16, start = nchar(P6B$P6Q16), end = nchar(P6B$P6Q16)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q16

##################################################
# P6A$P6Q17

P6A$P6Q17_11[P6A$P6Q17_11 == "1"] <- "11"
P6A$P6Q17_12[P6A$P6Q17_12 == "2"] <- "12"
P6A$P6Q17_13[P6A$P6Q17_13 == "3"] <- "13"
P6A$P6Q17_14[P6A$P6Q17_14 == "4"] <- "14"
P6A$P6Q17_15[P6A$P6Q17_15 == "5"] <- "15"
P6A$P6Q17_16[P6A$P6Q17_16 == "6"] <- "16"
P6A$P6Q17_99[P6A$P6Q17_99 == "7"] <- "99"

P6A$P6Q17 <- paste(P6A$P6Q17_11, P6A$P6Q17_12, P6A$P6Q17_13, P6A$P6Q17_14, P6A$P6Q17_15, P6A$P6Q17_16, P6A$P6Q17_99, sep = ",")
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17
P6A$P6Q17 <- gsub(",,", ",", P6A$P6Q17); P6A$P6Q17

P6A$P6Q17[str_sub(string = P6A$P6Q17, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q17[str_sub(string = P6A$P6Q17, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q17[str_sub(string = P6A$P6Q17, start = nchar(P6A$P6Q17), end = nchar(P6A$P6Q17)) == ","] <- 
  str_replace(string =  P6A$P6Q17[str_sub(string = P6A$P6Q17, start = nchar(P6A$P6Q17), end = nchar(P6A$P6Q17)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q17

##################################################
# P6B$P6Q17

P6B$P6Q17_11[P6B$P6Q17_11 == "1"] <- "11"
P6B$P6Q17_12[P6B$P6Q17_12 == "2"] <- "12"
P6B$P6Q17_13[P6B$P6Q17_13 == "3"] <- "13"
P6B$P6Q17_14[P6B$P6Q17_14 == "4"] <- "14"
P6B$P6Q17_15[P6B$P6Q17_15 == "5"] <- "15"
P6B$P6Q17_16[P6B$P6Q17_16 == "6"] <- "16"
P6B$P6Q17_99[P6B$P6Q17_99 == "7"] <- "99"

P6B$P6Q17 <- paste(P6B$P6Q17_11, P6B$P6Q17_12, P6B$P6Q17_13, P6B$P6Q17_14, P6B$P6Q17_15, P6B$P6Q17_16, P6B$P6Q17_99, sep = ",")
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17
P6B$P6Q17 <- gsub(",,", ",", P6B$P6Q17); P6B$P6Q17

P6B$P6Q17[str_sub(string = P6B$P6Q17, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q17[str_sub(string = P6B$P6Q17, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q17[str_sub(string = P6B$P6Q17, start = nchar(P6B$P6Q17), end = nchar(P6B$P6Q17)) == ","] <- 
  str_replace(string =  P6B$P6Q17[str_sub(string = P6B$P6Q17, start = nchar(P6B$P6Q17), end = nchar(P6B$P6Q17)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q17

##################################################
# P6A$P6Q18

table(P6A$P6Q18_1)
table(P6A$P6Q18_2)
table(P6A$P6Q18_3)
table(P6A$P6Q18_4)
table(P6A$P6Q18_5)
table(P6A$P6Q18_6)
table(P6A$P6Q18_7)

P6A$P6Q18 <- paste(P6A$P6Q18_1, P6A$P6Q18_2, P6A$P6Q18_3, P6A$P6Q18_4, P6A$P6Q18_5, P6A$P6Q18_6, P6A$P6Q18_7, sep = ",")
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18
P6A$P6Q18 <- gsub(",,", ",", P6A$P6Q18); P6A$P6Q18

P6A$P6Q18[str_sub(string = P6A$P6Q18, start = 1, end = 1) == ","] <- str_replace(string =  P6A$P6Q18[str_sub(string = P6A$P6Q18, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6A$P6Q18[str_sub(string = P6A$P6Q18, start = nchar(P6A$P6Q18), end = nchar(P6A$P6Q18)) == ","] <- 
  str_replace(string =  P6A$P6Q18[str_sub(string = P6A$P6Q18, start = nchar(P6A$P6Q18), end = nchar(P6A$P6Q18)) == ","], pattern = "\\,$", replacement = "")

P6A$P6Q18

##################################################
# P6B$P6Q18

table(P6B$P6Q18_1)
table(P6B$P6Q18_2)
table(P6B$P6Q18_3)
table(P6B$P6Q18_4)
table(P6B$P6Q18_5)
table(P6B$P6Q18_6)
table(P6B$P6Q18_7)

P6B$P6Q18 <- paste(P6B$P6Q18_1, P6B$P6Q18_2, P6B$P6Q18_3, P6B$P6Q18_4, P6B$P6Q18_5, P6B$P6Q18_6, P6B$P6Q18_7, sep = ",")
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18
P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18

P6B$P6Q18[str_sub(string = P6B$P6Q18, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q18[str_sub(string = P6B$P6Q18, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q18[str_sub(string = P6B$P6Q18, start = nchar(P6B$P6Q18), end = nchar(P6B$P6Q18)) == ","] <- 
  str_replace(string =  P6B$P6Q18[str_sub(string = P6B$P6Q18, start = nchar(P6B$P6Q18), end = nchar(P6B$P6Q18)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q18

##################################################

P6A$ID6 <- P6A$MemberID6
P6B$ID6 <- P6B$MemberID6

##################################################

P6A$Qtype6 <- 61
P6B$Qtype6 <- 62

##################################################

table(P6A$P6Q2)
table(P6A$P6Q2_3)
P6A$P6Q2[nchar(P6A$P6Q2_3) > 0] <- "3"; table(P6A$P6Q2)

table(P6B$P6Q2)
table(P6B$P6Q2_3)
P6B$P6Q2[nchar(P6B$P6Q2_3) > 0] <- "3"; table(P6B$P6Q2)

##################################################

table(P6A$P6Q3)
table(P6A$P6Q3_3)
P6A$P6Q3[nchar(P6A$P6Q3_3) > 0] <- "3"; table(P6A$P6Q3)

table(P6B$P6Q3)
table(P6B$P6Q3_3)
P6B$P6Q3[nchar(P6B$P6Q3_3) > 0] <- "3"; table(P6B$P6Q3)
# P6B$P6Q3[98]

##################################################

P6A$P6Q1_D
P6A$P6Q1_M
P6A$P6Q1_Y

P6B$P6Q1_D
P6B$P6Q1_M
P6B$P6Q1_Y

##################################################

# sum(P6A$P6Q4_R_1 == "1")

P6A$P6Q4_1[P6A$P6Q4_R_1 == "1"] <- "1"
P6A$P6Q4_2[P6A$P6Q4_R_1 == "2"] <- "1"
P6A$P6Q4_3[P6A$P6Q4_R_1 == "3"] <- "1"

P6A$P6Q4_1[P6A$P6Q4_R_2 == "1"] <- "2"
P6A$P6Q4_2[P6A$P6Q4_R_2 == "2"] <- "2"
P6A$P6Q4_3[P6A$P6Q4_R_2 == "3"] <- "2"

P6A$P6Q4_1[P6A$P6Q4_R_3 == "1"] <- "3"
P6A$P6Q4_2[P6A$P6Q4_R_3 == "2"] <- "3"
P6A$P6Q4_3[P6A$P6Q4_R_3 == "3"] <- "3"

P6A$P6Q4_1[P6A$P6Q4_R_4 == "1"] <- "4"
P6A$P6Q4_2[P6A$P6Q4_R_4 == "2"] <- "4"
P6A$P6Q4_3[P6A$P6Q4_R_4 == "3"] <- "4"

P6A$P6Q4_1[P6A$P6Q4_R_5 == "1"] <- "5"
P6A$P6Q4_2[P6A$P6Q4_R_5 == "2"] <- "5"
P6A$P6Q4_3[P6A$P6Q4_R_5 == "3"] <- "5"

P6A$P6Q4_1[P6A$P6Q4_R_6 == "1"] <- "6"
P6A$P6Q4_2[P6A$P6Q4_R_6 == "2"] <- "6"
P6A$P6Q4_3[P6A$P6Q4_R_6 == "3"] <- "6"

P6A$P6Q4_1[P6A$P6Q4_R_7 == "1"] <- "7"
P6A$P6Q4_2[P6A$P6Q4_R_7 == "2"] <- "7"
P6A$P6Q4_3[P6A$P6Q4_R_7 == "3"] <- "7"

P6A$P6Q4_1[P6A$P6Q4_R_8 == "1"] <- "8"
P6A$P6Q4_2[P6A$P6Q4_R_8 == "2"] <- "8"
P6A$P6Q4_3[P6A$P6Q4_R_8 == "3"] <- "8"

P6A$P6Q4_1[P6A$P6Q4_R_9 == "1"] <- "9"
P6A$P6Q4_2[P6A$P6Q4_R_9 == "2"] <- "9"
P6A$P6Q4_3[P6A$P6Q4_R_9 == "3"] <- "9"

P6A$P6Q4_1[P6A$P6Q4_R_10 == "1"] <- "10"
P6A$P6Q4_2[P6A$P6Q4_R_10 == "2"] <- "10"
P6A$P6Q4_3[P6A$P6Q4_R_10 == "3"] <- "10"

P6A$P6Q4_1[P6A$P6Q4_R_11 == "1"] <- "11"
P6A$P6Q4_2[P6A$P6Q4_R_11 == "2"] <- "11"
P6A$P6Q4_3[P6A$P6Q4_R_11 == "3"] <- "11"

P6A$P6Q4_1[P6A$P6Q4_R_12 == "1"] <- "12"
P6A$P6Q4_2[P6A$P6Q4_R_12 == "2"] <- "12"
P6A$P6Q4_3[P6A$P6Q4_R_12 == "3"] <- "12"

P6A$P6Q4_1[P6A$P6Q4_R_13 == "1"] <- "13"
P6A$P6Q4_2[P6A$P6Q4_R_13 == "2"] <- "13"
P6A$P6Q4_3[P6A$P6Q4_R_13 == "3"] <- "13"

P6A$P6Q4_1[P6A$P6Q4_R_14 == "1"] <- "14"
P6A$P6Q4_2[P6A$P6Q4_R_14 == "2"] <- "14"
P6A$P6Q4_3[P6A$P6Q4_R_14 == "3"] <- "14"

P6A$P6Q4_1[P6A$P6Q4_R_15 == "1"] <- "15"
P6A$P6Q4_2[P6A$P6Q4_R_15 == "2"] <- "15"
P6A$P6Q4_3[P6A$P6Q4_R_15 == "3"] <- "15"

P6A$P6Q4_1[P6A$P6Q4_R_16 == "1"] <- "16"
P6A$P6Q4_2[P6A$P6Q4_R_16 == "2"] <- "16"
P6A$P6Q4_3[P6A$P6Q4_R_16 == "3"] <- "16"

# P6A$P6Q4_1[P6A$ID6 == "15945"]
# P6A$P6Q4_2[P6A$ID6 == "15945"]
# P6A$P6Q4_3[P6A$ID6 == "15945"]
# 
# P6A$P6Q4_1[P6A$ID6 == "40613"]
# P6A$P6Q4_2[P6A$ID6 == "40613"]
# P6A$P6Q4_3[P6A$ID6 == "40613"]
# 
# P6A$P6Q4_1[P6A$ID6 == "12918"]
# P6A$P6Q4_2[P6A$ID6 == "12918"]
# P6A$P6Q4_3[P6A$ID6 == "12918"]

##################################################
# P6B$P6Q4_1
# P6B$P6Q4_2
# P6B$P6Q4_3

P6B$P6Q4_1[P6B$P6Q4_R_1 == "1"] <- "1"
P6B$P6Q4_2[P6B$P6Q4_R_1 == "2"] <- "1"
P6B$P6Q4_3[P6B$P6Q4_R_1 == "3"] <- "1"

P6B$P6Q4_1[P6B$P6Q4_R_2 == "1"] <- "2"
P6B$P6Q4_2[P6B$P6Q4_R_2 == "2"] <- "2"
P6B$P6Q4_3[P6B$P6Q4_R_2 == "3"] <- "2"

P6B$P6Q4_1[P6B$P6Q4_R_3 == "1"] <- "3"
P6B$P6Q4_2[P6B$P6Q4_R_3 == "2"] <- "3"
P6B$P6Q4_3[P6B$P6Q4_R_3 == "3"] <- "3"

P6B$P6Q4_1[P6B$P6Q4_R_4 == "1"] <- "4"
P6B$P6Q4_2[P6B$P6Q4_R_4 == "2"] <- "4"
P6B$P6Q4_3[P6B$P6Q4_R_4 == "3"] <- "4"

P6B$P6Q4_1[P6B$P6Q4_R_5 == "1"] <- "5"
P6B$P6Q4_2[P6B$P6Q4_R_5 == "2"] <- "5"
P6B$P6Q4_3[P6B$P6Q4_R_5 == "3"] <- "5"

P6B$P6Q4_1[P6B$P6Q4_R_6 == "1"] <- "6"
P6B$P6Q4_2[P6B$P6Q4_R_6 == "2"] <- "6"
P6B$P6Q4_3[P6B$P6Q4_R_6 == "3"] <- "6"

P6B$P6Q4_1[P6B$P6Q4_R_7 == "1"] <- "7"
P6B$P6Q4_2[P6B$P6Q4_R_7 == "2"] <- "7"
P6B$P6Q4_3[P6B$P6Q4_R_7 == "3"] <- "7"

P6B$P6Q4_1[P6B$P6Q4_R_8 == "1"] <- "8"
P6B$P6Q4_2[P6B$P6Q4_R_8 == "2"] <- "8"
P6B$P6Q4_3[P6B$P6Q4_R_8 == "3"] <- "8"

P6B$P6Q4_1[P6B$P6Q4_R_9 == "1"] <- "9"
P6B$P6Q4_2[P6B$P6Q4_R_9 == "2"] <- "9"
P6B$P6Q4_3[P6B$P6Q4_R_9 == "3"] <- "9"

P6B$P6Q4_1[P6B$P6Q4_R_10 == "1"] <- "10"
P6B$P6Q4_2[P6B$P6Q4_R_10 == "2"] <- "10"
P6B$P6Q4_3[P6B$P6Q4_R_10 == "3"] <- "10"

P6B$P6Q4_1[P6B$P6Q4_R_11 == "1"] <- "11"
P6B$P6Q4_2[P6B$P6Q4_R_11 == "2"] <- "11"
P6B$P6Q4_3[P6B$P6Q4_R_11 == "3"] <- "11"

P6B$P6Q4_1[P6B$P6Q4_R_12 == "1"] <- "12"
P6B$P6Q4_2[P6B$P6Q4_R_12 == "2"] <- "12"
P6B$P6Q4_3[P6B$P6Q4_R_12 == "3"] <- "12"

P6B$P6Q4_1[P6B$P6Q4_R_13 == "1"] <- "13"
P6B$P6Q4_2[P6B$P6Q4_R_13 == "2"] <- "13"
P6B$P6Q4_3[P6B$P6Q4_R_13 == "3"] <- "13"

P6B$P6Q4_1[P6B$P6Q4_R_14 == "1"] <- "14"
P6B$P6Q4_2[P6B$P6Q4_R_14 == "2"] <- "14"
P6B$P6Q4_3[P6B$P6Q4_R_14 == "3"] <- "14"

P6B$P6Q4_1[P6B$P6Q4_R_15 == "1"] <- "15"
P6B$P6Q4_2[P6B$P6Q4_R_15 == "2"] <- "15"
P6B$P6Q4_3[P6B$P6Q4_R_15 == "3"] <- "15"

P6B$P6Q4_1[P6B$P6Q4_R_16 == "1"] <- "16"
P6B$P6Q4_2[P6B$P6Q4_R_16 == "2"] <- "16"
P6B$P6Q4_3[P6B$P6Q4_R_16 == "3"] <- "16"

##################################################

P6A$P6Q6_1[P6A$P6Q6_R_1 == "1"] <- "1"
P6A$P6Q6_2[P6A$P6Q6_R_1 == "2"] <- "1"
P6A$P6Q6_3[P6A$P6Q6_R_1 == "3"] <- "1"

P6A$P6Q6_1[P6A$P6Q6_R_2 == "1"] <- "2"
P6A$P6Q6_2[P6A$P6Q6_R_2 == "2"] <- "2"
P6A$P6Q6_3[P6A$P6Q6_R_2 == "3"] <- "2"

P6A$P6Q6_1[P6A$P6Q6_R_3 == "1"] <- "3"
P6A$P6Q6_2[P6A$P6Q6_R_3 == "2"] <- "3"
P6A$P6Q6_3[P6A$P6Q6_R_3 == "3"] <- "3"

P6A$P6Q6_1[P6A$P6Q6_R_4 == "1"] <- "4"
P6A$P6Q6_2[P6A$P6Q6_R_4 == "2"] <- "4"
P6A$P6Q6_3[P6A$P6Q6_R_4 == "3"] <- "4"

P6A$P6Q6_1[P6A$P6Q6_R_5 == "1"] <- "5"
P6A$P6Q6_2[P6A$P6Q6_R_5 == "2"] <- "5"
P6A$P6Q6_3[P6A$P6Q6_R_5 == "3"] <- "5"

P6A$P6Q6_1[P6A$P6Q6_R_6 == "1"] <- "6"
P6A$P6Q6_2[P6A$P6Q6_R_6 == "2"] <- "6"
P6A$P6Q6_3[P6A$P6Q6_R_6 == "3"] <- "6"

P6A$P6Q6_1[P6A$P6Q6_R_7 == "1"] <- "7"
P6A$P6Q6_2[P6A$P6Q6_R_7 == "2"] <- "7"
P6A$P6Q6_3[P6A$P6Q6_R_7 == "3"] <- "7"

P6A$P6Q6_1[P6A$P6Q6_R_8 == "1"] <- "8"
P6A$P6Q6_2[P6A$P6Q6_R_8 == "2"] <- "8"
P6A$P6Q6_3[P6A$P6Q6_R_8 == "3"] <- "8"

P6A$P6Q6_1[P6A$P6Q6_R_9 == "1"] <- "9"
P6A$P6Q6_2[P6A$P6Q6_R_9 == "2"] <- "9"
P6A$P6Q6_3[P6A$P6Q6_R_9 == "3"] <- "9"

P6A$P6Q6_1[P6A$P6Q6_R_10 == "1"] <- "10"
P6A$P6Q6_2[P6A$P6Q6_R_10 == "2"] <- "10"
P6A$P6Q6_3[P6A$P6Q6_R_10 == "3"] <- "10"

##################################################
# P6B$P6Q6_1
# P6B$P6Q6_2
# P6B$P6Q6_3

P6B$P6Q6_1[P6B$P6Q6_R_1 == "1"] <- "1"
P6B$P6Q6_2[P6B$P6Q6_R_1 == "2"] <- "1"
P6B$P6Q6_3[P6B$P6Q6_R_1 == "3"] <- "1"

P6B$P6Q6_1[P6B$P6Q6_R_2 == "1"] <- "2"
P6B$P6Q6_2[P6B$P6Q6_R_2 == "2"] <- "2"
P6B$P6Q6_3[P6B$P6Q6_R_2 == "3"] <- "2"

P6B$P6Q6_1[P6B$P6Q6_R_3 == "1"] <- "3"
P6B$P6Q6_2[P6B$P6Q6_R_3 == "2"] <- "3"
P6B$P6Q6_3[P6B$P6Q6_R_3 == "3"] <- "3"

P6B$P6Q6_1[P6B$P6Q6_R_4 == "1"] <- "4"
P6B$P6Q6_2[P6B$P6Q6_R_4 == "2"] <- "4"
P6B$P6Q6_3[P6B$P6Q6_R_4 == "3"] <- "4"

P6B$P6Q6_1[P6B$P6Q6_R_5 == "1"] <- "5"
P6B$P6Q6_2[P6B$P6Q6_R_5 == "2"] <- "5"
P6B$P6Q6_3[P6B$P6Q6_R_5 == "3"] <- "5"

P6B$P6Q6_1[P6B$P6Q6_R_6 == "1"] <- "6"
P6B$P6Q6_2[P6B$P6Q6_R_6 == "2"] <- "6"
P6B$P6Q6_3[P6B$P6Q6_R_6 == "3"] <- "6"

P6B$P6Q6_1[P6B$P6Q6_R_7 == "1"] <- "7"
P6B$P6Q6_2[P6B$P6Q6_R_7 == "2"] <- "7"
P6B$P6Q6_3[P6B$P6Q6_R_7 == "3"] <- "7"

P6B$P6Q6_1[P6B$P6Q6_R_8 == "1"] <- "8"
P6B$P6Q6_2[P6B$P6Q6_R_8 == "2"] <- "8"
P6B$P6Q6_3[P6B$P6Q6_R_8 == "3"] <- "8"

P6B$P6Q6_1[P6B$P6Q6_R_9 == "1"] <- "9"
P6B$P6Q6_2[P6B$P6Q6_R_9 == "2"] <- "9"
P6B$P6Q6_3[P6B$P6Q6_R_9 == "3"] <- "9"

P6B$P6Q6_1[P6B$P6Q6_R_10 == "1"] <- "10"
P6B$P6Q6_2[P6B$P6Q6_R_10 == "2"] <- "10"
P6B$P6Q6_3[P6B$P6Q6_R_10 == "3"] <- "10"

##################################################

P6A$P6Q19_1 <- P6A$P6Q19_Nestle
P6A$P6Q19_2 <- P6A$P6Q19_Centrum
P6A$P6Q19_3 <- P6A$P6Q19_Abbott
P6A$P6Q19_4 <- P6A$P6Q19_Mamacare
P6A$P6Q19_5 <- P6A$P6Q19_Blackmores
P6A$P6Q19_7 <- P6A$P6Q19_GNC

##################################################

P6B$P6Q19_1 <- P6B$P6Q19_Nestle
P6B$P6Q19_2 <- P6B$P6Q19_Centrum
P6B$P6Q19_3 <- P6B$P6Q19_Abbott
P6B$P6Q19_4 <- P6B$P6Q19_Mamacare
P6B$P6Q19_5 <- P6B$P6Q19_Blackmores
P6B$P6Q19_7 <- P6B$P6Q19_GNC

##################################################

P6A$P6Q20_11 <- P6A$P6Q20_NOW
P6A$P6Q20_12 <- P6A$P6Q20_Wyeth
P6A$P6Q20_13 <- P6A$P6Q20_Horeb
P6A$P6Q20_14 <- P6A$P6Q20_MJN
P6A$P6Q20_15 <- P6A$P6Q20_Nolbel
P6A$P6Q20_16 <- P6A$P6Q20_Mamacare

##################################################

P6B$P6Q20_11 <- P6B$P6Q20_NOW
P6B$P6Q20_12 <- P6B$P6Q20_Wyeth
P6B$P6Q20_13 <- P6B$P6Q20_Horeb
P6B$P6Q20_14 <- P6B$P6Q20_MJN
P6B$P6Q20_15 <- P6B$P6Q20_Nolbel
P6B$P6Q20_16 <- P6B$P6Q20_Mamacare

##################################################

P6A$P6Q21_1 <- P6A$P6Q21_MJN
P6A$P6Q21_2 <- P6A$P6Q21_Abbott
P6A$P6Q21_3 <- P6A$P6Q21_Meiji
P6A$P6Q21_4 <- P6A$P6Q21_Anmum
P6A$P6Q21_5 <- P6A$P6Q21_Quaker
P6A$P6Q21_6 <- P6A$P6Q21_Neoangelac

##################################################

P6B$P6Q21_1 <- P6B$P6Q21_MJN
P6B$P6Q21_2 <- P6B$P6Q21_Abbott
P6B$P6Q21_3 <- P6B$P6Q21_Meiji
P6B$P6Q21_4 <- P6B$P6Q21_Anmum
P6B$P6Q21_5 <- P6B$P6Q21_Quaker
P6B$P6Q21_6 <- P6B$P6Q21_Neoangelac

##################################################

P6A$P6Q22
P6B$P6Q22

##################################################

P6A$P6Q23
P6B$P6Q23

##################################################
# P6A$P6Q24_rank

P6A$P6Q24_rank1[P6A$P6Q24_Nestle == "1"] <- "1"
P6A$P6Q24_rank2[P6A$P6Q24_Nestle == "2"] <- "1"
P6A$P6Q24_rank3[P6A$P6Q24_Nestle == "3"] <- "1"
P6A$P6Q24_rank4[P6A$P6Q24_Nestle == "4"] <- "1"
P6A$P6Q24_rank5[P6A$P6Q24_Nestle == "5"] <- "1"
P6A$P6Q24_rank6[P6A$P6Q24_Nestle == "6"] <- "1"
P6A$P6Q24_rank7[P6A$P6Q24_Nestle == "7"] <- "1"

P6A$P6Q24_rank1[P6A$P6Q24_Wyeth == "1"] <- "2"
P6A$P6Q24_rank2[P6A$P6Q24_Wyeth == "2"] <- "2"
P6A$P6Q24_rank3[P6A$P6Q24_Wyeth == "3"] <- "2"
P6A$P6Q24_rank4[P6A$P6Q24_Wyeth == "4"] <- "2"
P6A$P6Q24_rank5[P6A$P6Q24_Wyeth == "5"] <- "2"
P6A$P6Q24_rank6[P6A$P6Q24_Wyeth == "6"] <- "2"
P6A$P6Q24_rank7[P6A$P6Q24_Wyeth == "7"] <- "2"

P6A$P6Q24_rank1[P6A$P6Q24_MJN == "1"] <- "3"
P6A$P6Q24_rank2[P6A$P6Q24_MJN == "2"] <- "3"
P6A$P6Q24_rank3[P6A$P6Q24_MJN == "3"] <- "3"
P6A$P6Q24_rank4[P6A$P6Q24_MJN == "4"] <- "3"
P6A$P6Q24_rank5[P6A$P6Q24_MJN == "5"] <- "3"
P6A$P6Q24_rank6[P6A$P6Q24_MJN == "6"] <- "3"
P6A$P6Q24_rank7[P6A$P6Q24_MJN == "7"] <- "3"

P6A$P6Q24_rank1[P6A$P6Q24_Abbott == "1"] <- "4"
P6A$P6Q24_rank2[P6A$P6Q24_Abbott == "2"] <- "4"
P6A$P6Q24_rank3[P6A$P6Q24_Abbott == "3"] <- "4"
P6A$P6Q24_rank4[P6A$P6Q24_Abbott == "4"] <- "4"
P6A$P6Q24_rank5[P6A$P6Q24_Abbott == "5"] <- "4"
P6A$P6Q24_rank6[P6A$P6Q24_Abbott == "6"] <- "4"
P6A$P6Q24_rank7[P6A$P6Q24_Abbott == "7"] <- "4"

P6A$P6Q24_rank1[P6A$P6Q24_Meiji == "1"] <- "5"
P6A$P6Q24_rank2[P6A$P6Q24_Meiji == "2"] <- "5"
P6A$P6Q24_rank3[P6A$P6Q24_Meiji == "3"] <- "5"
P6A$P6Q24_rank4[P6A$P6Q24_Meiji == "4"] <- "5"
P6A$P6Q24_rank5[P6A$P6Q24_Meiji == "5"] <- "5"
P6A$P6Q24_rank6[P6A$P6Q24_Meiji == "6"] <- "5"
P6A$P6Q24_rank7[P6A$P6Q24_Meiji == "7"] <- "5"

P6A$P6Q24_rank1[P6A$P6Q24_Quaker == "1"] <- "6"
P6A$P6Q24_rank2[P6A$P6Q24_Quaker == "2"] <- "6"
P6A$P6Q24_rank3[P6A$P6Q24_Quaker == "3"] <- "6"
P6A$P6Q24_rank4[P6A$P6Q24_Quaker == "4"] <- "6"
P6A$P6Q24_rank5[P6A$P6Q24_Quaker == "5"] <- "6"
P6A$P6Q24_rank6[P6A$P6Q24_Quaker == "6"] <- "6"
P6A$P6Q24_rank7[P6A$P6Q24_Quaker == "7"] <- "6"

P6A$P6Q24_rank1[P6A$P6Q24_Snow == "1"] <- "7"
P6A$P6Q24_rank2[P6A$P6Q24_Snow == "2"] <- "7"
P6A$P6Q24_rank3[P6A$P6Q24_Snow == "3"] <- "7"
P6A$P6Q24_rank4[P6A$P6Q24_Snow == "4"] <- "7"
P6A$P6Q24_rank5[P6A$P6Q24_Snow == "5"] <- "7"
P6A$P6Q24_rank6[P6A$P6Q24_Snow == "6"] <- "7"
P6A$P6Q24_rank7[P6A$P6Q24_Snow == "7"] <- "7"

##################################################

P6A$P6Q24_rank1[P6A$ID6 == "1100"]
P6A$P6Q24_rank2[P6A$ID6 == "1100"]
P6A$P6Q24_rank3[P6A$ID6 == "1100"]
P6A$P6Q24_rank4[P6A$ID6 == "1100"]
P6A$P6Q24_rank5[P6A$ID6 == "1100"]
P6A$P6Q24_rank6[P6A$ID6 == "1100"]
P6A$P6Q24_rank7[P6A$ID6 == "1100"]

P6A$P6Q27_rank1[P6A$ID6 == "1100"]
P6A$P6Q27_rank2[P6A$ID6 == "1100"]
P6A$P6Q27_rank3[P6A$ID6 == "1100"]
P6A$P6Q27_rank4[P6A$ID6 == "1100"]
P6A$P6Q27_rank5[P6A$ID6 == "1100"]
P6A$P6Q27_rank6[P6A$ID6 == "1100"]
P6A$P6Q27_rank7[P6A$ID6 == "1100"]
P6A$P6Q27_rank8[P6A$ID6 == "1100"]
P6A$P6Q27_rank9[P6A$ID6 == "1100"]
P6A$P6Q27_rank10[P6A$ID6 == "1100"]

P6A$P6Q26_Nestle[P6A$ID6 == "1100"]
P6A$P6Q26_Wyeth[P6A$ID6 == "1100"]
P6A$P6Q26_MJN[P6A$ID6 == "1100"]
P6A$P6Q26_Abbott[P6A$ID6 == "1100"]
P6A$P6Q26_Meiji[P6A$ID6 == "1100"]
P6A$P6Q26_Quaker[P6A$ID6 == "1100"]
P6A$P6Q26_Snow[P6A$ID6 == "1100"]
P6A$P6Q26_Karihome[P6A$ID6 == "1100"]
P6A$P6Q26_Babecare[P6A$ID6 == "1100"]
P6A$P6Q26_Neoangelac[P6A$ID6 == "1100"]

P6A$P6Q24_rank1[P6A$ID6 == "16167"]
P6A$P6Q24_rank2[P6A$ID6 == "16167"]
P6A$P6Q24_rank3[P6A$ID6 == "16167"]
P6A$P6Q24_rank4[P6A$ID6 == "16167"]
P6A$P6Q24_rank5[P6A$ID6 == "16167"]
P6A$P6Q24_rank6[P6A$ID6 == "16167"]
P6A$P6Q24_rank7[P6A$ID6 == "16167"]

P6A$P6Q27_rank1[P6A$ID6 == "16167"]
P6A$P6Q27_rank2[P6A$ID6 == "16167"]
P6A$P6Q27_rank3[P6A$ID6 == "16167"]
P6A$P6Q27_rank4[P6A$ID6 == "16167"]
P6A$P6Q27_rank5[P6A$ID6 == "16167"]
P6A$P6Q27_rank6[P6A$ID6 == "16167"]
P6A$P6Q27_rank7[P6A$ID6 == "16167"]
P6A$P6Q27_rank8[P6A$ID6 == "16167"]
P6A$P6Q27_rank9[P6A$ID6 == "16167"]
P6A$P6Q27_rank10[P6A$ID6 == "16167"]

P6A$P6Q26_Nestle[P6A$ID6 == "16167"]
P6A$P6Q26_Wyeth[P6A$ID6 == "16167"]
P6A$P6Q26_MJN[P6A$ID6 == "16167"]
P6A$P6Q26_Abbott[P6A$ID6 == "16167"]
P6A$P6Q26_Meiji[P6A$ID6 == "16167"]
P6A$P6Q26_Quaker[P6A$ID6 == "16167"]
P6A$P6Q26_Snow[P6A$ID6 == "16167"]
P6A$P6Q26_Karihome[P6A$ID6 == "16167"]
P6A$P6Q26_Babecare[P6A$ID6 == "16167"]
P6A$P6Q26_Neoangelac[P6A$ID6 == "16167"]

##################################################

# P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]
# P6A[order(P6A[, c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], decreasing = TRUE), ]
# A[order(A[,4],decreasing=T),]
# P6A %>% 
#   group_by(P6Q24_Nestle, P6Q24_Wyeth, P6Q24_MJN, P6Q24_Abbott, P6Q24_Meiji, P6Q24_Quaker, P6Q24_Snow) %>%
#   arrange() %>% 
#   # dplyr::filter(P6Q24_Nestle = 1 | P6Q24_Wyeth = 1 | P6Q24_MJN = 1 | P6Q24_Abbott = 1 | P6Q24_Meiji = 1 | P6Q24_Quaker = 1 | P6Q24_Snow = 1) %>% 
#   # dplyr::filter(%in% c("1")) %>% 
#   summarise(P6Q24_rank1, P6Q24_rank2, P6Q24_rank3, P6Q24_rank4, P6Q24_rank5, P6Q24_rank6, P6Q24_rank7) %>% 
#   head(10)

# P6AP6Q24_TABLE <- P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]

# P6AP6Q24_TABLE %>% 
#   apply(1, sort) %>% 
#   head(10)

# P6A %>% 
#   select(P6Q24_Nestle, P6Q24_Wyeth, P6Q24_MJN, P6Q24_Abbott, P6Q24_Meiji, P6Q24_Quaker, P6Q24_Snow) %>% 
#   as.data.frame() %>% 
#   apply(1, sort) %>% 
#   head(10) 

# P6A$P6Q24_order_table <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, order)

# P6A[ , c(P6A$P6Q24_Nestle, P6A$P6Q24_Wyeth, P6A$P6Q24_MJN, P6A$P6Q24_Abbott, P6A$P6Q24_Meiji, P6A$P6Q24_Quaker, P6A$P6Q24_Snow)]
# P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]

# P6A$P6Q24_rank1 <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, value == 1)

# P6A$P6Q24_rank1 <- P6A %>% 
#   summarise(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")])

# P6A[ , c(P6A$P6Q24_Nestle, P6A$P6Q24_Wyeth, P6A$P6Q24_MJN, P6A$P6Q24_Abbott, P6A$P6Q24_Meiji, P6A$P6Q24_Quaker, P6A$P6Q24_Snow)]

# function_P6Q24 <- function() {
#   P6A$P6Q24_rank1 <- apply(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")], 1, which(P6A[ , c("P6Q24_Nestle", "P6Q24_Wyeth", "P6Q24_MJN", "P6Q24_Abbott", "P6Q24_Meiji", "P6Q24_Quaker", "P6Q24_Snow")]) == 1),
# }

##################################################
# P6B$P6Q24_rank

P6B$P6Q24_rank1[P6B$P6Q24_Nestle == "1"] <- "1"
P6B$P6Q24_rank2[P6B$P6Q24_Nestle == "2"] <- "1"
P6B$P6Q24_rank3[P6B$P6Q24_Nestle == "3"] <- "1"
P6B$P6Q24_rank4[P6B$P6Q24_Nestle == "4"] <- "1"
P6B$P6Q24_rank5[P6B$P6Q24_Nestle == "5"] <- "1"
P6B$P6Q24_rank6[P6B$P6Q24_Nestle == "6"] <- "1"
P6B$P6Q24_rank7[P6B$P6Q24_Nestle == "7"] <- "1"

P6B$P6Q24_rank1[P6B$P6Q24_Wyeth == "1"] <- "2"
P6B$P6Q24_rank2[P6B$P6Q24_Wyeth == "2"] <- "2"
P6B$P6Q24_rank3[P6B$P6Q24_Wyeth == "3"] <- "2"
P6B$P6Q24_rank4[P6B$P6Q24_Wyeth == "4"] <- "2"
P6B$P6Q24_rank5[P6B$P6Q24_Wyeth == "5"] <- "2"
P6B$P6Q24_rank6[P6B$P6Q24_Wyeth == "6"] <- "2"
P6B$P6Q24_rank7[P6B$P6Q24_Wyeth == "7"] <- "2"

P6B$P6Q24_rank1[P6B$P6Q24_MJN == "1"] <- "3"
P6B$P6Q24_rank2[P6B$P6Q24_MJN == "2"] <- "3"
P6B$P6Q24_rank3[P6B$P6Q24_MJN == "3"] <- "3"
P6B$P6Q24_rank4[P6B$P6Q24_MJN == "4"] <- "3"
P6B$P6Q24_rank5[P6B$P6Q24_MJN == "5"] <- "3"
P6B$P6Q24_rank6[P6B$P6Q24_MJN == "6"] <- "3"
P6B$P6Q24_rank7[P6B$P6Q24_MJN == "7"] <- "3"

P6B$P6Q24_rank1[P6B$P6Q24_Abbott == "1"] <- "4"
P6B$P6Q24_rank2[P6B$P6Q24_Abbott == "2"] <- "4"
P6B$P6Q24_rank3[P6B$P6Q24_Abbott == "3"] <- "4"
P6B$P6Q24_rank4[P6B$P6Q24_Abbott == "4"] <- "4"
P6B$P6Q24_rank5[P6B$P6Q24_Abbott == "5"] <- "4"
P6B$P6Q24_rank6[P6B$P6Q24_Abbott == "6"] <- "4"
P6B$P6Q24_rank7[P6B$P6Q24_Abbott == "7"] <- "4"

P6B$P6Q24_rank1[P6B$P6Q24_Meiji == "1"] <- "5"
P6B$P6Q24_rank2[P6B$P6Q24_Meiji == "2"] <- "5"
P6B$P6Q24_rank3[P6B$P6Q24_Meiji == "3"] <- "5"
P6B$P6Q24_rank4[P6B$P6Q24_Meiji == "4"] <- "5"
P6B$P6Q24_rank5[P6B$P6Q24_Meiji == "5"] <- "5"
P6B$P6Q24_rank6[P6B$P6Q24_Meiji == "6"] <- "5"
P6B$P6Q24_rank7[P6B$P6Q24_Meiji == "7"] <- "5"

P6B$P6Q24_rank1[P6B$P6Q24_Quaker == "1"] <- "6"
P6B$P6Q24_rank2[P6B$P6Q24_Quaker == "2"] <- "6"
P6B$P6Q24_rank3[P6B$P6Q24_Quaker == "3"] <- "6"
P6B$P6Q24_rank4[P6B$P6Q24_Quaker == "4"] <- "6"
P6B$P6Q24_rank5[P6B$P6Q24_Quaker == "5"] <- "6"
P6B$P6Q24_rank6[P6B$P6Q24_Quaker == "6"] <- "6"
P6B$P6Q24_rank7[P6B$P6Q24_Quaker == "7"] <- "6"

##################################################

P6A$P6Q25
P6B$P6Q25

##################################################

P6A$P6Q26_Nestle
P6A$P6Q26_Wyeth
P6A$P6Q26_MJN
P6A$P6Q26_Abbott
P6A$P6Q26_Meiji
P6A$P6Q26_Quaker
P6A$P6Q26_Snow
P6A$P6Q26_Karihome
P6A$P6Q26_Babecare
P6A$P6Q26_Neoangelac

##################################################

P6B$P6Q26_Nestle
P6B$P6Q26_Wyeth
P6B$P6Q26_MJN
P6B$P6Q26_Abbott
P6B$P6Q26_Meiji
P6B$P6Q26_Quaker
P6B$P6Q26_Snow
P6B$P6Q26_Karihome
P6B$P6Q26_Babecare
P6B$P6Q26_Neoangelac

##################################################

rank <- apply(P6A[, c("P6Q26_Nestle", "P6Q26_Wyeth", "P6Q26_MJN", "P6Q26_Abbott", "P6Q26_Meiji", "P6Q26_Quaker", "P6Q26_Snow", "P6Q26_Karihome", "P6Q26_Babecare", "P6Q26_Neoangelac")], 1, function(x) rank(x))
P6A$P6Q26_Nestle_Rank 
P6A[1:dim(P6A)[1], c("P6Q26_Nestle", "P6Q26_Wyeth", "P6Q26_MJN", "P6Q26_Abbott", "P6Q26_Meiji", "P6Q26_Quaker", "P6Q26_Snow", "P6Q26_Karihome", "P6Q26_Babecare", "P6Q26_Neoangelac")]

for(i in (1:dim(P6A)[1])){
  rank26 <- P6A[i, c("P6Q26_Nestle", "P6Q26_Wyeth", "P6Q26_MJN", "P6Q26_Abbott", "P6Q26_Meiji", "P6Q26_Quaker", "P6Q26_Snow", "P6Q26_Karihome", "P6Q26_Babecare", "P6Q26_Neoangelac")] %>%
    rank() %>% 
    as.data.frame()
}

rank(P6A[4, c("P6Q26_Nestle", "P6Q26_Wyeth", "P6Q26_MJN", "P6Q26_Abbott", "P6Q26_Meiji", "P6Q26_Quaker", "P6Q26_Snow", "P6Q26_Karihome", "P6Q26_Babecare", "P6Q26_Neoangelac")])

P6B$P6Q26_Nestle_Rank <- 

##################################################

P6A$P6Q27_rank1[P6A$P6Q27_Nestle == "1"] <- "1"
P6A$P6Q27_rank2[P6A$P6Q27_Nestle == "2"] <- "1"
P6A$P6Q27_rank3[P6A$P6Q27_Nestle == "3"] <- "1"
P6A$P6Q27_rank4[P6A$P6Q27_Nestle == "4"] <- "1"
P6A$P6Q27_rank5[P6A$P6Q27_Nestle == "5"] <- "1"
P6A$P6Q27_rank6[P6A$P6Q27_Nestle == "6"] <- "1"
P6A$P6Q27_rank7[P6A$P6Q27_Nestle == "7"] <- "1"
P6A$P6Q27_rank8[P6A$P6Q27_Nestle == "8"] <- "1"
P6A$P6Q27_rank9[P6A$P6Q27_Nestle == "9"] <- "1"
P6A$P6Q27_rank10[P6A$P6Q27_Nestle == "10"] <- "1"

P6A$P6Q27_rank1[P6A$P6Q27_Wyeth == "1"] <- "2"
P6A$P6Q27_rank2[P6A$P6Q27_Wyeth == "2"] <- "2"
P6A$P6Q27_rank3[P6A$P6Q27_Wyeth == "3"] <- "2"
P6A$P6Q27_rank4[P6A$P6Q27_Wyeth == "4"] <- "2"
P6A$P6Q27_rank5[P6A$P6Q27_Wyeth == "5"] <- "2"
P6A$P6Q27_rank6[P6A$P6Q27_Wyeth == "6"] <- "2"
P6A$P6Q27_rank7[P6A$P6Q27_Wyeth == "7"] <- "2"
P6A$P6Q27_rank8[P6A$P6Q27_Wyeth == "8"] <- "2"
P6A$P6Q27_rank9[P6A$P6Q27_Wyeth == "9"] <- "2"
P6A$P6Q27_rank10[P6A$P6Q27_Wyeth == "10"] <- "2"

P6A$P6Q27_rank1[P6A$P6Q27_MJN == "1"] <- "3"
P6A$P6Q27_rank2[P6A$P6Q27_MJN == "2"] <- "3"
P6A$P6Q27_rank3[P6A$P6Q27_MJN == "3"] <- "3"
P6A$P6Q27_rank4[P6A$P6Q27_MJN == "4"] <- "3"
P6A$P6Q27_rank5[P6A$P6Q27_MJN == "5"] <- "3"
P6A$P6Q27_rank6[P6A$P6Q27_MJN == "6"] <- "3"
P6A$P6Q27_rank7[P6A$P6Q27_MJN == "7"] <- "3"
P6A$P6Q27_rank8[P6A$P6Q27_MJN == "8"] <- "3"
P6A$P6Q27_rank9[P6A$P6Q27_MJN == "9"] <- "3"
P6A$P6Q27_rank10[P6A$P6Q27_MJN == "10"] <- "3"

P6A$P6Q27_rank1[P6A$P6Q27_Abbott == "1"] <- "4"
P6A$P6Q27_rank2[P6A$P6Q27_Abbott == "2"] <- "4"
P6A$P6Q27_rank3[P6A$P6Q27_Abbott == "3"] <- "4"
P6A$P6Q27_rank4[P6A$P6Q27_Abbott == "4"] <- "4"
P6A$P6Q27_rank5[P6A$P6Q27_Abbott == "5"] <- "4"
P6A$P6Q27_rank6[P6A$P6Q27_Abbott == "6"] <- "4"
P6A$P6Q27_rank7[P6A$P6Q27_Abbott == "7"] <- "4"
P6A$P6Q27_rank8[P6A$P6Q27_Abbott == "8"] <- "4"
P6A$P6Q27_rank9[P6A$P6Q27_Abbott == "9"] <- "4"
P6A$P6Q27_rank10[P6A$P6Q27_Abbott == "10"] <- "4"

P6A$P6Q27_rank1[P6A$P6Q27_Meiji == "1"] <- "5"
P6A$P6Q27_rank2[P6A$P6Q27_Meiji == "2"] <- "5"
P6A$P6Q27_rank3[P6A$P6Q27_Meiji == "3"] <- "5"
P6A$P6Q27_rank4[P6A$P6Q27_Meiji == "4"] <- "5"
P6A$P6Q27_rank5[P6A$P6Q27_Meiji == "5"] <- "5"
P6A$P6Q27_rank6[P6A$P6Q27_Meiji == "6"] <- "5"
P6A$P6Q27_rank7[P6A$P6Q27_Meiji == "7"] <- "5"
P6A$P6Q27_rank8[P6A$P6Q27_Meiji == "8"] <- "5"
P6A$P6Q27_rank9[P6A$P6Q27_Meiji == "9"] <- "5"
P6A$P6Q27_rank10[P6A$P6Q27_Meiji == "10"] <- "5"

P6A$P6Q27_rank1[P6A$P6Q27_Quaker == "1"] <- "6"
P6A$P6Q27_rank2[P6A$P6Q27_Quaker == "2"] <- "6"
P6A$P6Q27_rank3[P6A$P6Q27_Quaker == "3"] <- "6"
P6A$P6Q27_rank4[P6A$P6Q27_Quaker == "4"] <- "6"
P6A$P6Q27_rank5[P6A$P6Q27_Quaker == "5"] <- "6"
P6A$P6Q27_rank6[P6A$P6Q27_Quaker == "6"] <- "6"
P6A$P6Q27_rank7[P6A$P6Q27_Quaker == "7"] <- "6"
P6A$P6Q27_rank8[P6A$P6Q27_Quaker == "8"] <- "6"
P6A$P6Q27_rank9[P6A$P6Q27_Quaker == "9"] <- "6"
P6A$P6Q27_rank10[P6A$P6Q27_Quaker == "10"] <- "6"

P6A$P6Q27_rank1[P6A$P6Q27_Snow == "1"] <- "7"
P6A$P6Q27_rank2[P6A$P6Q27_Snow == "2"] <- "7"
P6A$P6Q27_rank3[P6A$P6Q27_Snow == "3"] <- "7"
P6A$P6Q27_rank4[P6A$P6Q27_Snow == "4"] <- "7"
P6A$P6Q27_rank5[P6A$P6Q27_Snow == "5"] <- "7"
P6A$P6Q27_rank6[P6A$P6Q27_Snow == "6"] <- "7"
P6A$P6Q27_rank7[P6A$P6Q27_Snow == "7"] <- "7"
P6A$P6Q27_rank8[P6A$P6Q27_Snow == "8"] <- "7"
P6A$P6Q27_rank9[P6A$P6Q27_Snow == "9"] <- "7"
P6A$P6Q27_rank10[P6A$P6Q27_Snow == "10"] <- "7"

P6A$P6Q27_rank1[P6A$P6Q27_Karihome == "1"] <- "8"
P6A$P6Q27_rank2[P6A$P6Q27_Karihome == "2"] <- "8"
P6A$P6Q27_rank3[P6A$P6Q27_Karihome == "3"] <- "8"
P6A$P6Q27_rank4[P6A$P6Q27_Karihome == "4"] <- "8"
P6A$P6Q27_rank5[P6A$P6Q27_Karihome == "5"] <- "8"
P6A$P6Q27_rank6[P6A$P6Q27_Karihome == "6"] <- "8"
P6A$P6Q27_rank7[P6A$P6Q27_Karihome == "7"] <- "8"
P6A$P6Q27_rank8[P6A$P6Q27_Karihome == "8"] <- "8"
P6A$P6Q27_rank9[P6A$P6Q27_Karihome == "9"] <- "8"
P6A$P6Q27_rank10[P6A$P6Q27_Karihome == "10"] <- "8"

P6A$P6Q27_rank1[P6A$P6Q27_Babecare == "1"] <- "9"
P6A$P6Q27_rank2[P6A$P6Q27_Babecare == "2"] <- "9"
P6A$P6Q27_rank3[P6A$P6Q27_Babecare == "3"] <- "9"
P6A$P6Q27_rank4[P6A$P6Q27_Babecare == "4"] <- "9"
P6A$P6Q27_rank5[P6A$P6Q27_Babecare == "5"] <- "9"
P6A$P6Q27_rank6[P6A$P6Q27_Babecare == "6"] <- "9"
P6A$P6Q27_rank7[P6A$P6Q27_Babecare == "7"] <- "9"
P6A$P6Q27_rank8[P6A$P6Q27_Babecare == "8"] <- "9"
P6A$P6Q27_rank9[P6A$P6Q27_Babecare == "9"] <- "9"
P6A$P6Q27_rank10[P6A$P6Q27_Babecare == "10"] <- "9"

P6A$P6Q27_rank1[P6A$P6Q27_Neoangelac == "1"] <- "10"
P6A$P6Q27_rank2[P6A$P6Q27_Neoangelac == "2"] <- "10"
P6A$P6Q27_rank3[P6A$P6Q27_Neoangelac == "3"] <- "10"
P6A$P6Q27_rank4[P6A$P6Q27_Neoangelac == "4"] <- "10"
P6A$P6Q27_rank5[P6A$P6Q27_Neoangelac == "5"] <- "10"
P6A$P6Q27_rank6[P6A$P6Q27_Neoangelac == "6"] <- "10"
P6A$P6Q27_rank7[P6A$P6Q27_Neoangelac == "7"] <- "10"
P6A$P6Q27_rank8[P6A$P6Q27_Neoangelac == "8"] <- "10"
P6A$P6Q27_rank9[P6A$P6Q27_Neoangelac == "9"] <- "10"
P6A$P6Q27_rank10[P6A$P6Q27_Neoangelac == "10"] <- "10"

##################################################

P6B$P6Q27_rank1[P6B$P6Q27_Nestle == "1"] <- "1"
P6B$P6Q27_rank2[P6B$P6Q27_Nestle == "2"] <- "1"
P6B$P6Q27_rank3[P6B$P6Q27_Nestle == "3"] <- "1"
P6B$P6Q27_rank4[P6B$P6Q27_Nestle == "4"] <- "1"
P6B$P6Q27_rank5[P6B$P6Q27_Nestle == "5"] <- "1"
P6B$P6Q27_rank6[P6B$P6Q27_Nestle == "6"] <- "1"
P6B$P6Q27_rank7[P6B$P6Q27_Nestle == "7"] <- "1"
P6B$P6Q27_rank8[P6B$P6Q27_Nestle == "8"] <- "1"
P6B$P6Q27_rank9[P6B$P6Q27_Nestle == "9"] <- "1"
P6B$P6Q27_rank10[P6B$P6Q27_Nestle == "10"] <- "1"

P6B$P6Q27_rank1[P6B$P6Q27_Wyeth == "1"] <- "2"
P6B$P6Q27_rank2[P6B$P6Q27_Wyeth == "2"] <- "2"
P6B$P6Q27_rank3[P6B$P6Q27_Wyeth == "3"] <- "2"
P6B$P6Q27_rank4[P6B$P6Q27_Wyeth == "4"] <- "2"
P6B$P6Q27_rank5[P6B$P6Q27_Wyeth == "5"] <- "2"
P6B$P6Q27_rank6[P6B$P6Q27_Wyeth == "6"] <- "2"
P6B$P6Q27_rank7[P6B$P6Q27_Wyeth == "7"] <- "2"
P6B$P6Q27_rank8[P6B$P6Q27_Wyeth == "8"] <- "2"
P6B$P6Q27_rank9[P6B$P6Q27_Wyeth == "9"] <- "2"
P6B$P6Q27_rank10[P6B$P6Q27_Wyeth == "10"] <- "2"

P6B$P6Q27_rank1[P6B$P6Q27_MJN == "1"] <- "3"
P6B$P6Q27_rank2[P6B$P6Q27_MJN == "2"] <- "3"
P6B$P6Q27_rank3[P6B$P6Q27_MJN == "3"] <- "3"
P6B$P6Q27_rank4[P6B$P6Q27_MJN == "4"] <- "3"
P6B$P6Q27_rank5[P6B$P6Q27_MJN == "5"] <- "3"
P6B$P6Q27_rank6[P6B$P6Q27_MJN == "6"] <- "3"
P6B$P6Q27_rank7[P6B$P6Q27_MJN == "7"] <- "3"
P6B$P6Q27_rank8[P6B$P6Q27_MJN == "8"] <- "3"
P6B$P6Q27_rank9[P6B$P6Q27_MJN == "9"] <- "3"
P6B$P6Q27_rank10[P6B$P6Q27_MJN == "10"] <- "3"

P6B$P6Q27_rank1[P6B$P6Q27_Abbott == "1"] <- "4"
P6B$P6Q27_rank2[P6B$P6Q27_Abbott == "2"] <- "4"
P6B$P6Q27_rank3[P6B$P6Q27_Abbott == "3"] <- "4"
P6B$P6Q27_rank4[P6B$P6Q27_Abbott == "4"] <- "4"
P6B$P6Q27_rank5[P6B$P6Q27_Abbott == "5"] <- "4"
P6B$P6Q27_rank6[P6B$P6Q27_Abbott == "6"] <- "4"
P6B$P6Q27_rank7[P6B$P6Q27_Abbott == "7"] <- "4"
P6B$P6Q27_rank8[P6B$P6Q27_Abbott == "8"] <- "4"
P6B$P6Q27_rank9[P6B$P6Q27_Abbott == "9"] <- "4"
P6B$P6Q27_rank10[P6B$P6Q27_Abbott == "10"] <- "4"

P6B$P6Q27_rank1[P6B$P6Q27_Meiji == "1"] <- "5"
P6B$P6Q27_rank2[P6B$P6Q27_Meiji == "2"] <- "5"
P6B$P6Q27_rank3[P6B$P6Q27_Meiji == "3"] <- "5"
P6B$P6Q27_rank4[P6B$P6Q27_Meiji == "4"] <- "5"
P6B$P6Q27_rank5[P6B$P6Q27_Meiji == "5"] <- "5"
P6B$P6Q27_rank6[P6B$P6Q27_Meiji == "6"] <- "5"
P6B$P6Q27_rank7[P6B$P6Q27_Meiji == "7"] <- "5"
P6B$P6Q27_rank8[P6B$P6Q27_Meiji == "8"] <- "5"
P6B$P6Q27_rank9[P6B$P6Q27_Meiji == "9"] <- "5"
P6B$P6Q27_rank10[P6B$P6Q27_Meiji == "10"] <- "5"

P6B$P6Q27_rank1[P6B$P6Q27_Quaker == "1"] <- "6"
P6B$P6Q27_rank2[P6B$P6Q27_Quaker == "2"] <- "6"
P6B$P6Q27_rank3[P6B$P6Q27_Quaker == "3"] <- "6"
P6B$P6Q27_rank4[P6B$P6Q27_Quaker == "4"] <- "6"
P6B$P6Q27_rank5[P6B$P6Q27_Quaker == "5"] <- "6"
P6B$P6Q27_rank6[P6B$P6Q27_Quaker == "6"] <- "6"
P6B$P6Q27_rank7[P6B$P6Q27_Quaker == "7"] <- "6"
P6B$P6Q27_rank8[P6B$P6Q27_Quaker == "8"] <- "6"
P6B$P6Q27_rank9[P6B$P6Q27_Quaker == "9"] <- "6"
P6B$P6Q27_rank10[P6B$P6Q27_Quaker == "10"] <- "6"

P6B$P6Q27_rank1[P6B$P6Q27_Snow == "1"] <- "7"
P6B$P6Q27_rank2[P6B$P6Q27_Snow == "2"] <- "7"
P6B$P6Q27_rank3[P6B$P6Q27_Snow == "3"] <- "7"
P6B$P6Q27_rank4[P6B$P6Q27_Snow == "4"] <- "7"
P6B$P6Q27_rank5[P6B$P6Q27_Snow == "5"] <- "7"
P6B$P6Q27_rank6[P6B$P6Q27_Snow == "6"] <- "7"
P6B$P6Q27_rank7[P6B$P6Q27_Snow == "7"] <- "7"
P6B$P6Q27_rank8[P6B$P6Q27_Snow == "8"] <- "7"
P6B$P6Q27_rank9[P6B$P6Q27_Snow == "9"] <- "7"
P6B$P6Q27_rank10[P6B$P6Q27_Snow == "10"] <- "7"

P6B$P6Q27_rank1[P6B$P6Q27_Karihome == "1"] <- "8"
P6B$P6Q27_rank2[P6B$P6Q27_Karihome == "2"] <- "8"
P6B$P6Q27_rank3[P6B$P6Q27_Karihome == "3"] <- "8"
P6B$P6Q27_rank4[P6B$P6Q27_Karihome == "4"] <- "8"
P6B$P6Q27_rank5[P6B$P6Q27_Karihome == "5"] <- "8"
P6B$P6Q27_rank6[P6B$P6Q27_Karihome == "6"] <- "8"
P6B$P6Q27_rank7[P6B$P6Q27_Karihome == "7"] <- "8"
P6B$P6Q27_rank8[P6B$P6Q27_Karihome == "8"] <- "8"
P6B$P6Q27_rank9[P6B$P6Q27_Karihome == "9"] <- "8"
P6B$P6Q27_rank10[P6B$P6Q27_Karihome == "10"] <- "8"

P6B$P6Q27_rank1[P6B$P6Q27_Babecare == "1"] <- "9"
P6B$P6Q27_rank2[P6B$P6Q27_Babecare == "2"] <- "9"
P6B$P6Q27_rank3[P6B$P6Q27_Babecare == "3"] <- "9"
P6B$P6Q27_rank4[P6B$P6Q27_Babecare == "4"] <- "9"
P6B$P6Q27_rank5[P6B$P6Q27_Babecare == "5"] <- "9"
P6B$P6Q27_rank6[P6B$P6Q27_Babecare == "6"] <- "9"
P6B$P6Q27_rank7[P6B$P6Q27_Babecare == "7"] <- "9"
P6B$P6Q27_rank8[P6B$P6Q27_Babecare == "8"] <- "9"
P6B$P6Q27_rank9[P6B$P6Q27_Babecare == "9"] <- "9"
P6B$P6Q27_rank10[P6B$P6Q27_Babecare == "10"] <- "9"

P6B$P6Q27_rank1[P6B$P6Q27_Neoangelac == "1"] <- "10"
P6B$P6Q27_rank2[P6B$P6Q27_Neoangelac == "2"] <- "10"
P6B$P6Q27_rank3[P6B$P6Q27_Neoangelac == "3"] <- "10"
P6B$P6Q27_rank4[P6B$P6Q27_Neoangelac == "4"] <- "10"
P6B$P6Q27_rank5[P6B$P6Q27_Neoangelac == "5"] <- "10"
P6B$P6Q27_rank6[P6B$P6Q27_Neoangelac == "6"] <- "10"
P6B$P6Q27_rank7[P6B$P6Q27_Neoangelac == "7"] <- "10"
P6B$P6Q27_rank8[P6B$P6Q27_Neoangelac == "8"] <- "10"
P6B$P6Q27_rank9[P6B$P6Q27_Neoangelac == "9"] <- "10"
P6B$P6Q27_rank10[P6B$P6Q27_Neoangelac == "10"] <- "10"

##################################################

P6A$P6Q28
P6A$P6Q28_98

P6A$P6Q28[nchar(P6A$P6Q28_98) > 0] <- "98"
table(P6A$P6Q28)

##################################################

P6B$P6Q28
P6B$P6Q28_98

P6B$P6Q28[nchar(P6B$P6Q28_98) > 0] <- "98"
table(P6B$P6Q28)

##################################################

P6A$P6Q29
P6A$P6Q29_98

P6A$P6Q29[nchar(P6A$P6Q29_98) > 0] <- "98"
table(P6A$P6Q29)

##################################################

P6B$P6Q29
P6B$P6Q29_98

P6B$P6Q29[nchar(P6B$P6Q29_98) > 0] <- "98"
table(P6B$P6Q29)

##################################################

P6A$P6Q30
P6A$P6Q30_7

P6A$P6Q30[nchar(P6A$P6Q30_7) > 0] <- "7"
table(P6A$P6Q30)

##################################################

P6B$P6Q30
P6B$P6Q30_7

P6B$P6Q30[nchar(P6B$P6Q30_7) > 0] <- "7"
table(P6B$P6Q30)

##################################################

P6A$P6Q31
P6A$P6Q31_10

P6A$P6Q31[nchar(P6A$P6Q31_10) > 0] <- "10"
table(P6A$P6Q31)

##################################################

P6B$P6Q31
P6B$P6Q31_10

P6B$P6Q31[nchar(P6B$P6Q31_10) > 0] <- "10"
table(P6B$P6Q31)

##################################################

P6A$BirthDay6 <- as.Date(P6A$BirthDay6, format = "%Y/%m/%d")
P6A$BirthDay6 <- gsub("-", "/", P6A$BirthDay6); P6A$BirthDay6

P6B$BirthDay6 <- as.Date(P6B$BirthDay6, format = "%Y/%m/%d")
P6B$BirthDay6 <- gsub("-", "/", P6B$BirthDay6); P6B$BirthDay6

P6A$TakeDay6 <- as.Date(P6A$TakeDay6, format = "%Y/%m/%d")
P6A$TakeDay6 <- gsub("-", "/", P6A$TakeDay6); P6A$TakeDay6

P6B$TakeDay6 <- as.Date(P6B$TakeDay6, format = "%Y/%m/%d")
P6B$TakeDay6 <- gsub("-", "/", P6B$TakeDay6); P6B$TakeDay6

P6A$SendDay6 <- as.Date(P6A$SendDay6, format = "%Y/%m/%d")
P6A$SendDay6 <- gsub("-", "/", P6A$SendDay6); P6A$SendDay6

P6B$SendDay6 <- as.Date(P6B$SendDay6, format = "%Y/%m/%d")
P6B$SendDay6 <- gsub("-", "/", P6B$SendDay6); P6B$SendDay6

P6A$RegisterDay6 <- as.Date(P6A$RegisterDay6, format = "%Y/%m/%d")
P6A$RegisterDay6 <- gsub("-", "/", P6A$RegisterDay6); P6A$RegisterDay6

P6B$RegisterDay6 <- as.Date(P6B$RegisterDay6, format = "%Y/%m/%d")
P6B$RegisterDay6 <- gsub("-", "/", P6B$RegisterDay6); P6B$RegisterDay6

##################################################

P6A$StartDay6 <- as.Date(P6A$StartDay6, format = "%m/%d/%Y")
P6A$StartDay6 <- gsub("-", "/", P6A$StartDay6); P6A$StartDay6

P6B$StartDay6 <- as.Date(P6B$StartDay6, format = "%m/%d/%Y")
P6B$StartDay6 <- gsub("-", "/", P6B$StartDay6); P6B$StartDay6

P6A$EndDay6 <- as.Date(P6A$EndDay6, format = "%m/%d/%Y")
P6A$EndDay6 <- gsub("-", "/", P6A$EndDay6); P6A$EndDay6

P6B$EndDay6 <- as.Date(P6B$EndDay6, format = "%m/%d/%Y")
P6B$EndDay6 <- gsub("-", "/", P6B$EndDay6); P6B$EndDay6

##################################################

P6A$Month6 <- "6"
P6B$Month6 <- "6"

P6A$GETENDDAY6 <- as.Date("2017/05/31", format = "%Y/%m/%d")
P6A$GETENDDAY6 <- gsub("-", "/", P6A$GETENDDAY6); P6A$GETENDDAY6

P6B$GETENDDAY6 <- as.Date("2017/05/31", format = "%Y/%m/%d")
P6B$GETENDDAY6 <- gsub("-", "/", P6B$GETENDDAY6); P6B$GETENDDAY6

##################################################

P6A$MemberDay
P6B$MemberDay

##################################################

P6A$P6Q27_1
P6B$P6Q27_1

P6A$NP6Q26_Nestle_Rank
P6B$NP6Q26_Nestle_Rank

##################################################

P6A <- select(P6A, -P6Q1)
P6A <- select(P6A, -P6Q2_3)
P6A <- select(P6A, -P6Q3_3)
P6A <- select(P6A, -P6Q4_R_1, -P6Q4_R_2, -P6Q4_R_3, -P6Q4_R_4, -P6Q4_R_5, -P6Q4_R_6, -P6Q4_R_7, -P6Q4_R_8, -P6Q4_R_9, -P6Q4_R_10, -P6Q4_R_11, -P6Q4_R_12, -P6Q4_R_13, -P6Q4_R_14, -P6Q4_R_15, -P6Q4_R_16)
P6A <- select(P6A, -P6Q5_1, -P6Q5_3, -P6Q5_4, -P6Q5_5, -P6Q5_6, -P6Q5_7, -P6Q5_8, -P6Q5_9, -P6Q5_10, -P6Q5_11)
P6A <- select(P6A, -P6Q6_R_1, -P6Q6_R_3, -P6Q6_R_4, -P6Q6_R_5, -P6Q6_R_6, -P6Q6_R_7, -P6Q6_R_8, -P6Q6_R_9, -P6Q6_R_10)
P6A <- select(P6A, -P6Q12_1, -P6Q12_2, -P6Q12_3, -P6Q12_4, -P6Q12_5, -P6Q12_6, -P6Q12_7, -P6Q12_8, -P6Q12_9, -P6Q12_10, -P6Q12_99)
P6A <- select(P6A, -P6Q13_99, -P6Q13_1, -P6Q13_2, -P6Q13_3, -P6Q13_4, -P6Q13_5, -P6Q13_6, -P6Q13_7, -P6Q13_8, -P6Q13_9, -P6Q13_10, -P6Q13_98)
P6A <- select(P6A, -P6Q14_99, -P6Q14_1, -P6Q14_2, -P6Q14_3, -P6Q14_4, -P6Q14_5, -P6Q14_6, -P6Q14_7, -P6Q14_8, -P6Q14_9, -P6Q14_10, -P6Q14_98)
P6A <- select(P6A, -P6Q15_99, -P6Q15_1, -P6Q15_2, -P6Q15_3, -P6Q15_4, -P6Q15_5, -P6Q15_6, -P6Q15_7, -P6Q15_8, -P6Q15_9, -P6Q15_10, -P6Q15_98)
P6A <- select(P6A, -P6Q16_1, -P6Q16_2, -P6Q16_3, -P6Q16_4, -P6Q16_5, -P6Q16_6, -P6Q16_7, -P6Q16_8, -P6Q16_9, -P6Q16_10, -P6Q16_11, -P6Q16_12)
P6A <- select(P6A, -P6Q17_11, -P6Q17_12, -P6Q17_13, -P6Q17_14, -P6Q17_15, -P6Q17_16, -P6Q17_99)
P6A <- select(P6A, -P6Q18_1, -P6Q18_2, -P6Q18_3, -P6Q18_4, -P6Q18_5, -P6Q18_6, -P6Q18_7)
P6A <- select(P6A, -P6Q19_Never, -P6Q19_Nestle, -P6Q19_Centrum, -P6Q19_Abbott, -P6Q19_Mamacare, -P6Q19_Blackmores, -P6Q19_Nolbel, -P6Q19_GNC, -P6Q19_MA, -P6Q19_Sundown, -P6Q19_DK)
P6A <- select(P6A, -P6Q20_NOW, -P6Q20_Wyeth, -P6Q20_Horeb, -P6Q20_MJN, -P6Q20_Nolbel, -P6Q20_Mamacare)
P6A <- select(P6A, -P6Q21_MJN, -P6Q21_Abbott,	-P6Q21_Meiji,	-P6Q21_Anmum,	-P6Q21_Quaker, -P6Q21_Neoangelac)
P6A <- select(P6A, -P6Q24_Nestle, -P6Q24_Wyeth,	-P6Q24_MJN, -P6Q24_Abbott, -P6Q24_Meiji, -P6Q24_Quaker, -P6Q24_Snow)
P6A <- select(P6A, -P6Q27_Nestle, -P6Q27_Wyeth, -P6Q27_MJN, -P6Q27_Abbott, -P6Q27_Meiji, -P6Q27_Quaker, -P6Q27_Snow, -P6Q27_Karihome, -P6Q27_Babecare, -P6Q27_Neoangelac)
P6A <- select(P6A, -P6Q28_98)
P6A <- select(P6A, -P6Q29_98)
P6A <- select(P6A, -P6Q30_7)

P6A[is.na(P6A)] <- ""

write.csv(P6A, "P6A.csv", row.names = TRUE)
str(P6A)

##################################################

P6B <- select(P6B, -P6Q1)
P6B <- select(P6B, -P6Q2_3)
P6B <- select(P6B, -P6Q3_3)
P6B <- select(P6B, -P6Q4_R_1, -P6Q4_R_2, -P6Q4_R_3, -P6Q4_R_4, -P6Q4_R_5, -P6Q4_R_6, -P6Q4_R_7, -P6Q4_R_8, -P6Q4_R_9, -P6Q4_R_10, -P6Q4_R_11, -P6Q4_R_12, -P6Q4_R_13, -P6Q4_R_14, -P6Q4_R_15, -P6Q4_R_16)
P6B <- select(P6B, -P6Q5_1, -P6Q5_3, -P6Q5_4, -P6Q5_5, -P6Q5_6, -P6Q5_7, -P6Q5_8, -P6Q5_9, -P6Q5_10, -P6Q5_11)
P6B <- select(P6B, -P6Q6_R_1, -P6Q6_R_3, -P6Q6_R_4, -P6Q6_R_5, -P6Q6_R_6, -P6Q6_R_7, -P6Q6_R_8, -P6Q6_R_9, -P6Q6_R_10)
P6B <- select(P6B, -P6Q12_1, -P6Q12_2, -P6Q12_3, -P6Q12_4, -P6Q12_5, -P6Q12_6, -P6Q12_7, -P6Q12_8, -P6Q12_9, -P6Q12_10, -P6Q12_99)
P6B <- select(P6B, -P6Q13_99, -P6Q13_1, -P6Q13_2, -P6Q13_3, -P6Q13_4, -P6Q13_5, -P6Q13_6, -P6Q13_7, -P6Q13_8, -P6Q13_9, -P6Q13_10, -P6Q13_98)
P6B <- select(P6B, -P6Q14_99, -P6Q14_1, -P6Q14_2, -P6Q14_3, -P6Q14_4, -P6Q14_5, -P6Q14_6, -P6Q14_7, -P6Q14_8, -P6Q14_9, -P6Q14_10, -P6Q14_98)
P6B <- select(P6B, -P6Q15_99, -P6Q15_1, -P6Q15_2, -P6Q15_3, -P6Q15_4, -P6Q15_5, -P6Q15_6, -P6Q15_7, -P6Q15_8, -P6Q15_9, -P6Q15_10, -P6Q15_98)
P6B <- select(P6B, -P6Q16_1, -P6Q16_2, -P6Q16_3, -P6Q16_4, -P6Q16_5, -P6Q16_6, -P6Q16_7, -P6Q16_8, -P6Q16_9, -P6Q16_10, -P6Q16_11, -P6Q16_12)
P6B <- select(P6B, -P6Q17_11, -P6Q17_12, -P6Q17_13, -P6Q17_14, -P6Q17_15, -P6Q17_16, -P6Q17_99)
P6B <- select(P6B, -P6Q18_1, -P6Q18_2, -P6Q18_3, -P6Q18_4, -P6Q18_5, -P6Q18_6, -P6Q18_7)
P6B <- select(P6B, -P6Q19_Never, -P6Q19_Nestle, -P6Q19_Centrum, -P6Q19_Abbott, -P6Q19_Mamacare, -P6Q19_Blackmores, -P6Q19_Nolbel, -P6Q19_GNC, -P6Q19_MA, -P6Q19_Sundown, -P6Q19_DK)
P6B <- select(P6B, -P6Q20_NOW, -P6Q20_Wyeth, -P6Q20_Horeb, -P6Q20_MJN, -P6Q20_Nolbel, -P6Q20_Mamacare)
P6B <- select(P6B, -P6Q21_MJN, -P6Q21_Abbott,	-P6Q21_Meiji,	-P6Q21_Anmum,	-P6Q21_Quaker, -P6Q21_Neoangelac)
P6B <- select(P6B, -P6Q24_Nestle, -P6Q24_Wyeth,	-P6Q24_MJN, -P6Q24_Abbott, -P6Q24_Meiji, -P6Q24_Quaker)
P6B <- select(P6B, -P6Q27_Nestle, -P6Q27_Wyeth, -P6Q27_MJN, -P6Q27_Abbott, -P6Q27_Meiji, -P6Q27_Quaker, -P6Q27_Snow, -P6Q27_Karihome, -P6Q27_Babecare, -P6Q27_Neoangelac)
P6B <- select(P6B, -P6Q28_98)
P6B <- select(P6B, -P6Q29_98)
P6B <- select(P6B, -P6Q30_7)

P6B[is.na(P6B)] <- ""

write.csv(P6B, "P6B.csv", row.names = TRUE)
str(P6B)

##################################################

P6A$ID <- P6A$ID6
P6B$ID <- P6B$ID6

# P6 <- full_join(P6A, P6B, by = "ID")

P6 <- union(P6A, P6B)
P6[is.na(P6)] <- ""
P6 <- arrange(P6, ID)

write.csv(P6, "P6.csv", row.names = TRUE)

# remove .x and .y

##################################################

library(dplyr)
library(stringr)

##################################################

P5A <- read.table("#5A_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
P5B <- read.table("#5B_2017_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

##################################################

P5A <- select(P5A, -grep("x", names(P5A)))
P5B <- select(P5B, -grep("x", names(P5B)))

P5A <- P5A[-c(1, 2), ]
P5B <- P5B[-c(1, 2), ]

##################################################
# P5A$P5Q5

P5A$P5Q5_11[nchar(P5A$P5Q5_11) > 0] <- "11"
P5A$P5Q5_11

P5A$P5Q5_3[P5A$P5Q5_3 == "2"] <- "3"
P5A$P5Q5_4[P5A$P5Q5_4 == "3"] <- "4"
P5A$P5Q5_5[P5A$P5Q5_5 == "4"] <- "5"
P5A$P5Q5_6[P5A$P5Q5_6 == "5"] <- "6"
P5A$P5Q5_7[P5A$P5Q5_7 == "6"] <- "7"
P5A$P5Q5_8[P5A$P5Q5_8 == "7"] <- "8"
P5A$P5Q5_9[P5A$P5Q5_9 == "8"] <- "9"
P5A$P5Q5_10[P5A$P5Q5_10 == "9"] <- "10"

P5A$P5Q5 <- paste(P5A$P5Q5_1, P5A$P5Q5_3, P5A$P5Q5_4, P5A$P5Q5_5, P5A$P5Q5_6, P5A$P5Q5_7, P5A$P5Q5_8, P5A$P5Q5_9, P5A$P5Q5_10, P5A$P5Q5_11, sep = ",")
P5A$P5Q5 <- gsub(",,", ",", P5A$P5Q5); P5A$P5Q5 <- gsub(",,", ",", P5A$P5Q5); P5A$P5Q5 <- gsub(",,", ",", P5A$P5Q5); P5A$P5Q5 <- gsub(",,", ",", P5A$P5Q5); P5A$P5Q5 <- gsub(",,", ",", P5A$P5Q5)
P5A$P5Q5; class(P5A$P5Q5)

P5A$P5Q5[str_sub(string = P5A$P5Q5, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q5[str_sub(string = P5A$P5Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q5[str_sub(string = P5A$P5Q5, start = nchar(P5A$P5Q5), end = nchar(P5A$P5Q5)) == ","] <- 
  str_replace(string =  P5A$P5Q5[str_sub(string = P5A$P5Q5, start = nchar(P5A$P5Q5), end = nchar(P5A$P5Q5)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q5

##################################################
# P5B$P5Q5

P5B$P5Q5_11; class(P5B$P5Q5_11)
P5B$P5Q5_11[nchar(P5B$P5Q5_11) > 0] <- "11"
P5B$P5Q5_3[P5B$P5Q5_3 == "2"] <- "3"
P5B$P5Q5_4[P5B$P5Q5_4 == "3"] <- "4"
P5B$P5Q5_5[P5B$P5Q5_5 == "4"] <- "5"
P5B$P5Q5_6[P5B$P5Q5_6 == "5"] <- "6"
P5A$P5Q5_7[P5A$P5Q5_7 == "6"] <- "7"
P5B$P5Q5_8[P5B$P5Q5_8 == "7"] <- "8"
P5B$P5Q5_9[P5B$P5Q5_9 == "8"] <- "9"
P5B$P5Q5_10[P5B$P5Q5_10 == "9"] <- "10"

P5B$P5Q5 <- paste(P5B$P5Q5_1, P5B$P5Q5_3, P5B$P5Q5_4, P5B$P5Q5_5, P5B$P5Q5_6, P5B$P5Q5_7, P5B$P5Q5_8, P5B$P5Q5_9, P5B$P5Q5_10, P5B$P5Q5_11, sep = ",")
P5B$P5Q5

P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5); P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5); P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5); P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5); P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5); P5B$P5Q5 <- gsub(",,", ",", P5B$P5Q5)
P5B$P5Q5
class(P5B$P5Q5)

P5B$P5Q5[str_sub(string = P5B$P5Q5, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q5[str_sub(string = P5B$P5Q5, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q5[str_sub(string = P5B$P5Q5, start = nchar(P5B$P5Q5), end = nchar(P5B$P5Q5)) == ","] <- 
  str_replace(string =  P5B$P5Q5[str_sub(string = P5B$P5Q5, start = nchar(P5B$P5Q5), end = nchar(P5B$P5Q5)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q5

##################################################
# P5A$P5Q12

P5A$P5Q12 <- paste(P5A$P5Q12_1, P5A$P5Q12_2, P5A$P5Q12_3, P5A$P5Q12_4, P5A$P5Q12_5, P5A$P5Q12_6, P5A$P5Q12_7, P5A$P5Q12_8, P5A$P5Q12_9, P5A$P5Q12_10, P5A$P5Q12_99, sep = ",")
P5A$P5Q12
P5A$P5Q12 <- gsub(",,", ",", P5A$P5Q12)
P5A$P5Q12 <- gsub(",,", ",", P5A$P5Q12)
P5A$P5Q12 <- gsub(",,", ",", P5A$P5Q12)
P5A$P5Q12 <- gsub(",,", ",", P5A$P5Q12)
P5A$P5Q12 <- gsub(",,", ",", P5A$P5Q12)
P5A$P5Q12

P5A$P5Q12[str_sub(string = P5A$P5Q12, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q12[str_sub(string = P5A$P5Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q12[str_sub(string = P5A$P5Q12, start = nchar(P5A$P5Q12), end = nchar(P5A$P5Q12)) == ","] <- 
  str_replace(string =  P5A$P5Q12[str_sub(string = P5A$P5Q12, start = nchar(P5A$P5Q12), end = nchar(P5A$P5Q12)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q12

##################################################
# P5B$P5Q12

P5B$P5Q12 <- paste(P5B$P5Q12_1, P5B$P5Q12_2, P5B$P5Q12_3, P5B$P5Q12_4, P5B$P5Q12_5, P5B$P5Q12_6, P5B$P5Q12_7, P5B$P5Q12_8, P5B$P5Q12_9, P5B$P5Q12_10, P5B$P5Q12_99, sep = ",")

P5B$P5Q12 <- gsub(",,", ",", P5B$P5Q12)
P5B$P5Q12 <- gsub(",,", ",", P5B$P5Q12)
P5B$P5Q12 <- gsub(",,", ",", P5B$P5Q12)
P5B$P5Q12 <- gsub(",,", ",", P5B$P5Q12)
P5B$P5Q12 <- gsub(",,", ",", P5B$P5Q12)
P5B$P5Q12

P5B$P5Q12[str_sub(string = P5B$P5Q12, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q12[str_sub(string = P5B$P5Q12, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q12[str_sub(string = P5B$P5Q12, start = nchar(P5B$P5Q12), end = nchar(P5B$P5Q12)) == ","] <- 
  str_replace(string =  P5B$P5Q12[str_sub(string = P5B$P5Q12, start = nchar(P5B$P5Q12), end = nchar(P5B$P5Q12)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q12

##################################################
# P5A$P5Q13

P5A$P5Q13_99[P5A$P5Q13_99 == "1"] <- "99"
P5A$P5Q13_1[P5A$P5Q13_1 == "2"] <- "1"
P5A$P5Q13_2[P5A$P5Q13_2 == "3"] <- "2"
P5A$P5Q13_3[P5A$P5Q13_3 == "4"] <- "3"
P5A$P5Q13_4[P5A$P5Q13_4 == "5"] <- "4"
P5A$P5Q13_5[P5A$P5Q13_5 == "6"] <- "5"
P5A$P5Q13_6[P5A$P5Q13_6 == "7"] <- "6"
P5A$P5Q13_7[P5A$P5Q13_7 == "8"] <- "7"
P5A$P5Q13_8[P5A$P5Q13_8 == "9"] <- "8"
P5A$P5Q13_9[P5A$P5Q13_9 == "10"] <- "9"
P5A$P5Q13_10[P5A$P5Q13_10 == "11"] <- "10"
P5A$P5Q13_98[nchar(P5A$P5Q13_98) > 0] <- "98"

P5A$P5Q13 <- paste(P5A$P5Q13_1, P5A$P5Q13_2, P5A$P5Q13_3, P5A$P5Q13_4, P5A$P5Q13_5, P5A$P5Q13_6, P5A$P5Q13_7, P5A$P5Q13_8, P5A$P5Q13_9, P5A$P5Q13_10, P5A$P5Q13_98, P5A$P5Q13_99, sep = ",")
P5A$P5Q13
P5A$P5Q13 <- gsub(",,", ",", P5A$P5Q13); P5A$P5Q13
P5A$P5Q13 <- gsub(",,", ",", P5A$P5Q13); P5A$P5Q13
P5A$P5Q13 <- gsub(",,", ",", P5A$P5Q13); P5A$P5Q13
P5A$P5Q13 <- gsub(",,", ",", P5A$P5Q13); P5A$P5Q13
P5A$P5Q13 <- gsub(",,", ",", P5A$P5Q13); P5A$P5Q13

P5A$P5Q13[str_sub(string = P5A$P5Q13, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q13[str_sub(string = P5A$P5Q13, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q13[str_sub(string = P5A$P5Q13, start = nchar(P5A$P5Q13), end = nchar(P5A$P5Q13)) == ","] <- 
  str_replace(string =  P5A$P5Q13[str_sub(string = P5A$P5Q13, start = nchar(P5A$P5Q13), end = nchar(P5A$P5Q13)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q13

##################################################
# P5B$P5Q13

P5B$P5Q13_99[P5B$P5Q13_99 == "1"] <- "99"
P5B$P5Q13_1[P5B$P5Q13_1 == "2"] <- "1"
P5B$P5Q13_2[P5B$P5Q13_2 == "3"] <- "2"
P5B$P5Q13_3[P5B$P5Q13_3 == "4"] <- "3"
P5B$P5Q13_4[P5B$P5Q13_4 == "5"] <- "4"
P5B$P5Q13_5[P5B$P5Q13_5 == "6"] <- "5"
P5B$P5Q13_6[P5B$P5Q13_6 == "7"] <- "6"
P5B$P5Q13_7[P5B$P5Q13_7 == "8"] <- "7"
P5B$P5Q13_8[P5B$P5Q13_8 == "9"] <- "8"
P5B$P5Q13_9[P5B$P5Q13_9 == "10"] <- "9"
P5B$P5Q13_10[P5B$P5Q13_10 == "11"] <- "10"
P5B$P5Q13_98[nchar(P5B$P5Q13_98) > 0] <- "98"

P5B$P5Q13 <- paste(P5B$P5Q13_1, P5B$P5Q13_2, P5B$P5Q13_3, P5B$P5Q13_4, P5B$P5Q13_5, P5B$P5Q13_6, P5B$P5Q13_7, P5B$P5Q13_8, P5B$P5Q13_9, P5B$P5Q13_10, P5B$P5Q13_98, P5B$P5Q13_99, sep = ",")
P5B$P5Q13
P5B$P5Q13 <- gsub(",,", ",", P5B$P5Q13); P5B$P5Q13
P5B$P5Q13 <- gsub(",,", ",", P5B$P5Q13); P5B$P5Q13
P5B$P5Q13 <- gsub(",,", ",", P5B$P5Q13); P5B$P5Q13
P5B$P5Q13 <- gsub(",,", ",", P5B$P5Q13); P5B$P5Q13
P5B$P5Q13 <- gsub(",,", ",", P5B$P5Q13); P5B$P5Q13

P5B$P5Q13[str_sub(string = P5B$P5Q13, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q13[str_sub(string = P5B$P5Q13, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q13[str_sub(string = P5B$P5Q13, start = nchar(P5B$P5Q13), end = nchar(P5B$P5Q13)) == ","] <- 
  str_replace(string =  P5B$P5Q13[str_sub(string = P5B$P5Q13, start = nchar(P5B$P5Q13), end = nchar(P5B$P5Q13)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q13

##################################################
# P5A$P5Q14

P5A$P5Q14_99[P5A$P5Q14_99 == "1"] <- "99"
P5A$P5Q14_1[P5A$P5Q14_1 == "2"] <- "1"
P5A$P5Q14_2[P5A$P5Q14_2 == "3"] <- "2"
P5A$P5Q14_3[P5A$P5Q14_3 == "4"] <- "3"
P5A$P5Q14_4[P5A$P5Q14_4 == "5"] <- "4"
P5A$P5Q14_5[P5A$P5Q14_5 == "6"] <- "5"
P5A$P5Q14_6[P5A$P5Q14_6 == "7"] <- "6"
P5A$P5Q14_7[P5A$P5Q14_7 == "8"] <- "7"
P5A$P5Q14_8[P5A$P5Q14_8 == "9"] <- "8"
P5A$P5Q14_9[P5A$P5Q14_9 == "10"] <- "9"
P5A$P5Q14_10[P5A$P5Q14_10 == "11"] <- "10"
P5A$P5Q14_98[nchar(P5A$P5Q14_98) > 0] <- "98"

P5A$P5Q14 <- paste(P5A$P5Q14_1, P5A$P5Q14_2, P5A$P5Q14_3, P5A$P5Q14_4, P5A$P5Q14_5, P5A$P5Q14_6, P5A$P5Q14_7, P5A$P5Q14_8, P5A$P5Q14_9, P5A$P5Q14_10, P5A$P5Q14_98, P5A$P5Q14_99, sep = ",")
P5A$P5Q14 <- gsub(",,", ",", P5A$P5Q14); P5A$P5Q14
P5A$P5Q14 <- gsub(",,", ",", P5A$P5Q14); P5A$P5Q14
P5A$P5Q14 <- gsub(",,", ",", P5A$P5Q14); P5A$P5Q14
P5A$P5Q14 <- gsub(",,", ",", P5A$P5Q14); P5A$P5Q14
P5A$P5Q14 <- gsub(",,", ",", P5A$P5Q14); P5A$P5Q14

P5A$P5Q14[str_sub(string = P5A$P5Q14, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q14[str_sub(string = P5A$P5Q14, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q14[str_sub(string = P5A$P5Q14, start = nchar(P5A$P5Q14), end = nchar(P5A$P5Q14)) == ","] <- 
  str_replace(string =  P5A$P5Q14[str_sub(string = P5A$P5Q14, start = nchar(P5A$P5Q14), end = nchar(P5A$P5Q14)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q14

##################################################
# P5B$P5Q14

P5B$P5Q14_99[P5B$P5Q14_99 == "1"] <- "99"
P5B$P5Q14_1[P5B$P5Q14_1 == "2"] <- "1"
P5B$P5Q14_2[P5B$P5Q14_2 == "3"] <- "2"
P5B$P5Q14_3[P5B$P5Q14_3 == "4"] <- "3"
P5B$P5Q14_4[P5B$P5Q14_4 == "5"] <- "4"
P5B$P5Q14_5[P5B$P5Q14_5 == "6"] <- "5"
P5B$P5Q14_6[P5B$P5Q14_6 == "7"] <- "6"
P5B$P5Q14_7[P5B$P5Q14_7 == "8"] <- "7"
P5B$P5Q14_8[P5B$P5Q14_8 == "9"] <- "8"
P5B$P5Q14_9[P5B$P5Q14_9 == "10"] <- "9"
P5B$P5Q14_10[P5B$P5Q14_10 == "11"] <- "10"
P5B$P5Q14_98[nchar(P5B$P5Q14_98) > 0] <- "98"

P5B$P5Q14 <- paste(P5B$P5Q14_1, P5B$P5Q14_2, P5B$P5Q14_3, P5B$P5Q14_4, P5B$P5Q14_5, P5B$P5Q14_6, P5B$P5Q14_7, P5B$P5Q14_8, P5B$P5Q14_9, P5B$P5Q14_10, P5B$P5Q14_98, P5B$P5Q14_99, sep = ",")
P5B$P5Q14 <- gsub(",,", ",", P5B$P5Q14); P5B$P5Q14
P5B$P5Q14 <- gsub(",,", ",", P5B$P5Q14); P5B$P5Q14
P5B$P5Q14 <- gsub(",,", ",", P5B$P5Q14); P5B$P5Q14
P5B$P5Q14 <- gsub(",,", ",", P5B$P5Q14); P5B$P5Q14
P5B$P5Q14 <- gsub(",,", ",", P5B$P5Q14); P5B$P5Q14

P5B$P5Q14[str_sub(string = P5B$P5Q14, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q14[str_sub(string = P5B$P5Q14, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q14[str_sub(string = P5B$P5Q14, start = nchar(P5B$P5Q14), end = nchar(P5B$P5Q14)) == ","] <- 
  str_replace(string =  P5B$P5Q14[str_sub(string = P5B$P5Q14, start = nchar(P5B$P5Q14), end = nchar(P5B$P5Q14)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q14

##################################################
# P5A$P5Q15

P5A$P5Q15_99[P5A$P5Q15_99 == "1"] <- "99"
P5A$P5Q15_1[P5A$P5Q15_1 == "2"] <- "1"
P5A$P5Q15_2[P5A$P5Q15_2 == "3"] <- "2"
P5A$P5Q15_3[P5A$P5Q15_3 == "4"] <- "3"
P5A$P5Q15_4[P5A$P5Q15_4 == "5"] <- "4"
P5A$P5Q15_5[P5A$P5Q15_5 == "6"] <- "5"
P5A$P5Q15_6[P5A$P5Q15_6 == "7"] <- "6"
P5A$P5Q15_7[P5A$P5Q15_7 == "8"] <- "7"
P5A$P5Q15_8[P5A$P5Q15_8 == "9"] <- "8"
P5A$P5Q15_9[P5A$P5Q15_9 == "10"] <- "9"
P5A$P5Q15_10[P5A$P5Q15_10 == "11"] <- "10"
P5A$P5Q15_98[nchar(P5A$P5Q15_98) > 0] <- "98"

P5A$P5Q15 <- paste(P5A$P5Q15_1, P5A$P5Q15_2, P5A$P5Q15_3, P5A$P5Q15_4, P5A$P5Q15_5, P5A$P5Q15_6, P5A$P5Q15_7, P5A$P5Q15_8, P5A$P5Q15_9, P5A$P5Q15_10, P5A$P5Q15_98, P5A$P5Q15_99, sep = ",")
P5A$P5Q15 <- gsub(",,", ",", P5A$P5Q15); P5A$P5Q15
P5A$P5Q15 <- gsub(",,", ",", P5A$P5Q15); P5A$P5Q15
P5A$P5Q15 <- gsub(",,", ",", P5A$P5Q15); P5A$P5Q15
P5A$P5Q15 <- gsub(",,", ",", P5A$P5Q15); P5A$P5Q15
P5A$P5Q15 <- gsub(",,", ",", P5A$P5Q15); P5A$P5Q15

P5A$P5Q15[str_sub(string = P5A$P5Q15, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q15[str_sub(string = P5A$P5Q15, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q15[str_sub(string = P5A$P5Q15, start = nchar(P5A$P5Q15), end = nchar(P5A$P5Q15)) == ","] <- 
  str_replace(string =  P5A$P5Q15[str_sub(string = P5A$P5Q15, start = nchar(P5A$P5Q15), end = nchar(P5A$P5Q15)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q15

##################################################
# P5B$P5Q15

P5B$P5Q15_99[P5B$P5Q15_99 == "1"] <- "99"
P5B$P5Q15_1[P5B$P5Q15_1 == "2"] <- "1"
P5B$P5Q15_2[P5B$P5Q15_2 == "3"] <- "2"
P5B$P5Q15_3[P5B$P5Q15_3 == "4"] <- "3"
P5B$P5Q15_4[P5B$P5Q15_4 == "5"] <- "4"
P5B$P5Q15_5[P5B$P5Q15_5 == "6"] <- "5"
P5B$P5Q15_6[P5B$P5Q15_6 == "7"] <- "6"
P5B$P5Q15_7[P5B$P5Q15_7 == "8"] <- "7"
P5B$P5Q15_8[P5B$P5Q15_8 == "9"] <- "8"
P5B$P5Q15_9[P5B$P5Q15_9 == "10"] <- "9"
P5B$P5Q15_10[P5B$P5Q15_10 == "11"] <- "10"
P5B$P5Q15_98[nchar(P5B$P5Q15_98) > 0] <- "98"

P5B$P5Q15 <- paste(P5B$P5Q15_1, P5B$P5Q15_2, P5B$P5Q15_3, P5B$P5Q15_4, P5B$P5Q15_5, P5B$P5Q15_6, P5B$P5Q15_7, P5B$P5Q15_8, P5B$P5Q15_9, P5B$P5Q15_10, P5B$P5Q15_98, P5B$P5Q15_99, sep = ",")
P5B$P5Q15 <- gsub(",,", ",", P5B$P5Q15); P5B$P5Q15
P5B$P5Q15 <- gsub(",,", ",", P5B$P5Q15); P5B$P5Q15
P5B$P5Q15 <- gsub(",,", ",", P5B$P5Q15); P5B$P5Q15
P5B$P5Q15 <- gsub(",,", ",", P5B$P5Q15); P5B$P5Q15
P5B$P5Q15 <- gsub(",,", ",", P5B$P5Q15); P5B$P5Q15

P5B$P5Q15[str_sub(string = P5B$P5Q15, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q15[str_sub(string = P5B$P5Q15, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q15[str_sub(string = P5B$P5Q15, start = nchar(P5B$P5Q15), end = nchar(P5B$P5Q15)) == ","] <- 
  str_replace(string =  P5B$P5Q15[str_sub(string = P5B$P5Q15, start = nchar(P5B$P5Q15), end = nchar(P5B$P5Q15)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q15

##################################################
# P5A$P5Q16

table(P5A$P5Q16_1)
table(P5A$P5Q16_2)
table(P5A$P5Q16_3)
table(P5A$P5Q16_4)
table(P5A$P5Q16_5)
table(P5A$P5Q16_6)
table(P5A$P5Q16_7)
table(P5A$P5Q16_8)
table(P5A$P5Q16_9)
table(P5A$P5Q16_10)
table(P5A$P5Q16_11)
table(P5A$P5Q16_12)

P5A$P5Q16_12[nchar(P5A$P5Q16_12) > 0] <- "12"

P5A$P5Q16 <- paste(P5A$P5Q16_1, P5A$P5Q16_2, P5A$P5Q16_3, P5A$P5Q16_4, P5A$P5Q16_5, P5A$P5Q16_6, P5A$P5Q16_7, P5A$P5Q16_8, P5A$P5Q16_9, P5A$P5Q16_10, P5A$P5Q16_11, P5A$P5Q16_12, sep = ",")
P5A$P5Q16 <- gsub(",,", ",", P5A$P5Q16); P5A$P5Q16
P5A$P5Q16 <- gsub(",,", ",", P5A$P5Q16); P5A$P5Q16
P5A$P5Q16 <- gsub(",,", ",", P5A$P5Q16); P5A$P5Q16
P5A$P5Q16 <- gsub(",,", ",", P5A$P5Q16); P5A$P5Q16
P5A$P5Q16 <- gsub(",,", ",", P5A$P5Q16); P5A$P5Q16

P5A$P5Q16[str_sub(string = P5A$P5Q16, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q16[str_sub(string = P5A$P5Q16, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q16[str_sub(string = P5A$P5Q16, start = nchar(P5A$P5Q16), end = nchar(P5A$P5Q16)) == ","] <- 
  str_replace(string =  P5A$P5Q16[str_sub(string = P5A$P5Q16, start = nchar(P5A$P5Q16), end = nchar(P5A$P5Q16)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q16

##################################################
# P5B$P5Q16

table(P5B$P5Q16_1)
table(P5B$P5Q16_2)
table(P5B$P5Q16_3)
table(P5B$P5Q16_4)
table(P5B$P5Q16_5)
table(P5B$P5Q16_6)
table(P5B$P5Q16_7)
table(P5B$P5Q16_8)
table(P5B$P5Q16_9)
table(P5B$P5Q16_10)
table(P5B$P5Q16_11)
table(P5B$P5Q16_12)

P5B$P5Q16_12[nchar(P5B$P5Q16_12) > 0] <- "12"

P5B$P5Q16 <- paste(P5B$P5Q16_1, P5B$P5Q16_2, P5B$P5Q16_3, P5B$P5Q16_4, P5B$P5Q16_5, P5B$P5Q16_6, P5B$P5Q16_7, P5B$P5Q16_8, P5B$P5Q16_9, P5B$P5Q16_10, P5B$P5Q16_11, P5B$P5Q16_12, sep = ",")
P5B$P5Q16 <- gsub(",,", ",", P5B$P5Q16); P5B$P5Q16
P5B$P5Q16 <- gsub(",,", ",", P5B$P5Q16); P5B$P5Q16
P5B$P5Q16 <- gsub(",,", ",", P5B$P5Q16); P5B$P5Q16
P5B$P5Q16 <- gsub(",,", ",", P5B$P5Q16); P5B$P5Q16
P5B$P5Q16 <- gsub(",,", ",", P5B$P5Q16); P5B$P5Q16

P5B$P5Q16[str_sub(string = P5B$P5Q16, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q16[str_sub(string = P5B$P5Q16, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q16[str_sub(string = P5B$P5Q16, start = nchar(P5B$P5Q16), end = nchar(P5B$P5Q16)) == ","] <- 
  str_replace(string =  P5B$P5Q16[str_sub(string = P5B$P5Q16, start = nchar(P5B$P5Q16), end = nchar(P5B$P5Q16)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q16

##################################################
# P5A$P5Q17

P5A$P5Q17_11[P5A$P5Q17_11 == "1"] <- "11"
P5A$P5Q17_12[P5A$P5Q17_12 == "2"] <- "12"
P5A$P5Q17_13[P5A$P5Q17_13 == "3"] <- "13"
P5A$P5Q17_14[P5A$P5Q17_14 == "4"] <- "14"
P5A$P5Q17_15[P5A$P5Q17_15 == "5"] <- "15"
P5A$P5Q17_16[P5A$P5Q17_16 == "6"] <- "16"
P5A$P5Q17_99[P5A$P5Q17_99 == "7"] <- "99"

P5A$P5Q17 <- paste(P5A$P5Q17_11, P5A$P5Q17_12, P5A$P5Q17_13, P5A$P5Q17_14, P5A$P5Q17_15, P5A$P5Q17_16, P5A$P5Q17_99, sep = ",")
P5A$P5Q17 <- gsub(",,", ",", P5A$P5Q17); P5A$P5Q17
P5A$P5Q17 <- gsub(",,", ",", P5A$P5Q17); P5A$P5Q17
P5A$P5Q17 <- gsub(",,", ",", P5A$P5Q17); P5A$P5Q17
P5A$P5Q17 <- gsub(",,", ",", P5A$P5Q17); P5A$P5Q17
P5A$P5Q17 <- gsub(",,", ",", P5A$P5Q17); P5A$P5Q17

P5A$P5Q17[str_sub(string = P5A$P5Q17, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q17[str_sub(string = P5A$P5Q17, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q17[str_sub(string = P5A$P5Q17, start = nchar(P5A$P5Q17), end = nchar(P5A$P5Q17)) == ","] <- 
  str_replace(string =  P5A$P5Q17[str_sub(string = P5A$P5Q17, start = nchar(P5A$P5Q17), end = nchar(P5A$P5Q17)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q17

##################################################
# P5B$P5Q17

P5B$P5Q17_11[P5B$P5Q17_11 == "1"] <- "11"
P5B$P5Q17_12[P5B$P5Q17_12 == "2"] <- "12"
P5B$P5Q17_13[P5B$P5Q17_13 == "3"] <- "13"
P5B$P5Q17_14[P5B$P5Q17_14 == "4"] <- "14"
P5B$P5Q17_15[P5B$P5Q17_15 == "5"] <- "15"
P5B$P5Q17_16[P5B$P5Q17_16 == "6"] <- "16"
P5B$P5Q17_99[P5B$P5Q17_99 == "7"] <- "99"

P5B$P5Q17 <- paste(P5B$P5Q17_11, P5B$P5Q17_12, P5B$P5Q17_13, P5B$P5Q17_14, P5B$P5Q17_15, P5B$P5Q17_16, P5B$P5Q17_99, sep = ",")
P5B$P5Q17 <- gsub(",,", ",", P5B$P5Q17); P5B$P5Q17
P5B$P5Q17 <- gsub(",,", ",", P5B$P5Q17); P5B$P5Q17
P5B$P5Q17 <- gsub(",,", ",", P5B$P5Q17); P5B$P5Q17
P5B$P5Q17 <- gsub(",,", ",", P5B$P5Q17); P5B$P5Q17
P5B$P5Q17 <- gsub(",,", ",", P5B$P5Q17); P5B$P5Q17

P5B$P5Q17[str_sub(string = P5B$P5Q17, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q17[str_sub(string = P5B$P5Q17, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q17[str_sub(string = P5B$P5Q17, start = nchar(P5B$P5Q17), end = nchar(P5B$P5Q17)) == ","] <- 
  str_replace(string =  P5B$P5Q17[str_sub(string = P5B$P5Q17, start = nchar(P5B$P5Q17), end = nchar(P5B$P5Q17)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q17

##################################################
# P5A$P5Q18

table(P5A$P5Q18_1)
table(P5A$P5Q18_2)
table(P5A$P5Q18_3)
table(P5A$P5Q18_4)
table(P5A$P5Q18_5)
table(P5A$P5Q18_6)
table(P5A$P5Q18_7)

P5A$P5Q18 <- paste(P5A$P5Q18_1, P5A$P5Q18_2, P5A$P5Q18_3, P5A$P5Q18_4, P5A$P5Q18_5, P5A$P5Q18_6, P5A$P5Q18_7, sep = ",")
P5A$P5Q18 <- gsub(",,", ",", P5A$P5Q18); P5A$P5Q18
P5A$P5Q18 <- gsub(",,", ",", P5A$P5Q18); P5A$P5Q18
P5A$P5Q18 <- gsub(",,", ",", P5A$P5Q18); P5A$P5Q18
P5A$P5Q18 <- gsub(",,", ",", P5A$P5Q18); P5A$P5Q18
P5A$P5Q18 <- gsub(",,", ",", P5A$P5Q18); P5A$P5Q18

P5A$P5Q18[str_sub(string = P5A$P5Q18, start = 1, end = 1) == ","] <- str_replace(string =  P5A$P5Q18[str_sub(string = P5A$P5Q18, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5A$P5Q18[str_sub(string = P5A$P5Q18, start = nchar(P5A$P5Q18), end = nchar(P5A$P5Q18)) == ","] <- 
  str_replace(string =  P5A$P5Q18[str_sub(string = P5A$P5Q18, start = nchar(P5A$P5Q18), end = nchar(P5A$P5Q18)) == ","], pattern = "\\,$", replacement = "")

P5A$P5Q18

##################################################
# P5B$P5Q18

table(P5B$P5Q18_1)
table(P5B$P5Q18_2)
table(P5B$P5Q18_3)
table(P5B$P5Q18_4)
table(P5B$P5Q18_5)
table(P5B$P5Q18_6)
table(P5B$P5Q18_7)

P5B$P5Q18 <- paste(P5B$P5Q18_1, P5B$P5Q18_2, P5B$P5Q18_3, P5B$P5Q18_4, P5B$P5Q18_5, P5B$P5Q18_6, P5B$P5Q18_7, sep = ",")
P5B$P5Q18 <- gsub(",,", ",", P5B$P5Q18); P5B$P5Q18
P5B$P5Q18 <- gsub(",,", ",", P5B$P5Q18); P5B$P5Q18
P5B$P5Q18 <- gsub(",,", ",", P5B$P5Q18); P5B$P5Q18
P5B$P5Q18 <- gsub(",,", ",", P5B$P5Q18); P5B$P5Q18
P5B$P5Q18 <- gsub(",,", ",", P5B$P5Q18); P5B$P5Q18

P5B$P5Q18[str_sub(string = P5B$P5Q18, start = 1, end = 1) == ","] <- str_replace(string =  P5B$P5Q18[str_sub(string = P5B$P5Q18, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P5B$P5Q18[str_sub(string = P5B$P5Q18, start = nchar(P5B$P5Q18), end = nchar(P5B$P5Q18)) == ","] <- 
  str_replace(string =  P5B$P5Q18[str_sub(string = P5B$P5Q18, start = nchar(P5B$P5Q18), end = nchar(P5B$P5Q18)) == ","], pattern = "\\,$", replacement = "")

P5B$P5Q18

##################################################
# P5A$ID6
# P5B$ID6

P5A$ID5 <- P5A$MemberID5
P5B$ID5 <- P5B$MemberID5

##################################################

P5A$Qtype5 <- 51
P5B$Qtype5 <- 52

##################################################
# P5A$P5Q2
# P5B$P5Q2

table(P5A$P5Q2)
table(P5A$P5Q2_3)
P5A$P5Q2[nchar(P5A$P5Q2_3) > 0] <- "3"; table(P5A$P5Q2)

table(P5B$P5Q2)
table(P5B$P5Q2_3)
P5B$P5Q2[nchar(P5B$P5Q2_3) > 0] <- "3"; table(P5B$P5Q2)

##################################################
# P5A$P5Q3
# P5B$P5Q3

table(P5A$P5Q3)
table(P5A$P5Q3_3)
P5A$P5Q3[nchar(P5A$P5Q3_3) > 0] <- "3"; table(P5A$P5Q3)

table(P5B$P5Q3)
table(P5B$P5Q3_3)
P5B$P5Q3[nchar(P5B$P5Q3_3) > 0] <- "3"; table(P5B$P5Q3)
# P5B$P5Q3[98]

##################################################

P5A$P5Q1_D
P5A$P5Q1_M
P5A$P5Q1_Y

P5B$P5Q1_D
P5B$P5Q1_M
P5B$P5Q1_Y

##################################################

P5A$P5Q4_1[P5A$P5Q4_R_1 == "1"] <- "1"
P5A$P5Q4_2[P5A$P5Q4_R_1 == "2"] <- "1"
P5A$P5Q4_3[P5A$P5Q4_R_1 == "3"] <- "1"

P5A$P5Q4_1[P5A$P5Q4_R_2 == "1"] <- "2"
P5A$P5Q4_2[P5A$P5Q4_R_2 == "2"] <- "2"
P5A$P5Q4_3[P5A$P5Q4_R_2 == "3"] <- "2"

P5A$P5Q4_1[P5A$P5Q4_R_3 == "1"] <- "3"
P5A$P5Q4_2[P5A$P5Q4_R_3 == "2"] <- "3"
P5A$P5Q4_3[P5A$P5Q4_R_3 == "3"] <- "3"

P5A$P5Q4_1[P5A$P5Q4_R_4 == "1"] <- "4"
P5A$P5Q4_2[P5A$P5Q4_R_4 == "2"] <- "4"
P5A$P5Q4_3[P5A$P5Q4_R_4 == "3"] <- "4"

P5A$P5Q4_1[P5A$P5Q4_R_5 == "1"] <- "5"
P5A$P5Q4_2[P5A$P5Q4_R_5 == "2"] <- "5"
P5A$P5Q4_3[P5A$P5Q4_R_5 == "3"] <- "5"

P5A$P5Q4_1[P5A$P5Q4_R_6 == "1"] <- "6"
P5A$P5Q4_2[P5A$P5Q4_R_6 == "2"] <- "6"
P5A$P5Q4_3[P5A$P5Q4_R_6 == "3"] <- "6"

P5A$P5Q4_1[P5A$P5Q4_R_7 == "1"] <- "7"
P5A$P5Q4_2[P5A$P5Q4_R_7 == "2"] <- "7"
P5A$P5Q4_3[P5A$P5Q4_R_7 == "3"] <- "7"

P5A$P5Q4_1[P5A$P5Q4_R_8 == "1"] <- "8"
P5A$P5Q4_2[P5A$P5Q4_R_8 == "2"] <- "8"
P5A$P5Q4_3[P5A$P5Q4_R_8 == "3"] <- "8"

P5A$P5Q4_1[P5A$P5Q4_R_9 == "1"] <- "9"
P5A$P5Q4_2[P5A$P5Q4_R_9 == "2"] <- "9"
P5A$P5Q4_3[P5A$P5Q4_R_9 == "3"] <- "9"

P5A$P5Q4_1[P5A$P5Q4_R_10 == "1"] <- "10"
P5A$P5Q4_2[P5A$P5Q4_R_10 == "2"] <- "10"
P5A$P5Q4_3[P5A$P5Q4_R_10 == "3"] <- "10"

P5A$P5Q4_1[P5A$P5Q4_R_11 == "1"] <- "11"
P5A$P5Q4_2[P5A$P5Q4_R_11 == "2"] <- "11"
P5A$P5Q4_3[P5A$P5Q4_R_11 == "3"] <- "11"

P5A$P5Q4_1[P5A$P5Q4_R_12 == "1"] <- "12"
P5A$P5Q4_2[P5A$P5Q4_R_12 == "2"] <- "12"
P5A$P5Q4_3[P5A$P5Q4_R_12 == "3"] <- "12"

P5A$P5Q4_1[P5A$P5Q4_R_13 == "1"] <- "13"
P5A$P5Q4_2[P5A$P5Q4_R_13 == "2"] <- "13"
P5A$P5Q4_3[P5A$P5Q4_R_13 == "3"] <- "13"

P5A$P5Q4_1[P5A$P5Q4_R_14 == "1"] <- "14"
P5A$P5Q4_2[P5A$P5Q4_R_14 == "2"] <- "14"
P5A$P5Q4_3[P5A$P5Q4_R_14 == "3"] <- "14"

P5A$P5Q4_1[P5A$P5Q4_R_15 == "1"] <- "15"
P5A$P5Q4_2[P5A$P5Q4_R_15 == "2"] <- "15"
P5A$P5Q4_3[P5A$P5Q4_R_15 == "3"] <- "15"

P5A$P5Q4_1[P5A$P5Q4_R_16 == "1"] <- "16"
P5A$P5Q4_2[P5A$P5Q4_R_16 == "2"] <- "16"
P5A$P5Q4_3[P5A$P5Q4_R_16 == "3"] <- "16"

##################################################

P5B$P5Q4_1[P5B$P5Q4_R_1 == "1"] <- "1"
P5B$P5Q4_2[P5B$P5Q4_R_1 == "2"] <- "1"
P5B$P5Q4_3[P5B$P5Q4_R_1 == "3"] <- "1"

P5B$P5Q4_1[P5B$P5Q4_R_2 == "1"] <- "2"
P5B$P5Q4_2[P5B$P5Q4_R_2 == "2"] <- "2"
P5B$P5Q4_3[P5B$P5Q4_R_2 == "3"] <- "2"

P5B$P5Q4_1[P5B$P5Q4_R_3 == "1"] <- "3"
P5B$P5Q4_2[P5B$P5Q4_R_3 == "2"] <- "3"
P5B$P5Q4_3[P5B$P5Q4_R_3 == "3"] <- "3"

P5B$P5Q4_1[P5B$P5Q4_R_4 == "1"] <- "4"
P5B$P5Q4_2[P5B$P5Q4_R_4 == "2"] <- "4"
P5B$P5Q4_3[P5B$P5Q4_R_4 == "3"] <- "4"

P5B$P5Q4_1[P5B$P5Q4_R_5 == "1"] <- "5"
P5B$P5Q4_2[P5B$P5Q4_R_5 == "2"] <- "5"
P5B$P5Q4_3[P5B$P5Q4_R_5 == "3"] <- "5"

P5B$P5Q4_1[P5B$P5Q4_R_6 == "1"] <- "6"
P5B$P5Q4_2[P5B$P5Q4_R_6 == "2"] <- "6"
P5B$P5Q4_3[P5B$P5Q4_R_6 == "3"] <- "6"

P5B$P5Q4_1[P5B$P5Q4_R_7 == "1"] <- "7"
P5B$P5Q4_2[P5B$P5Q4_R_7 == "2"] <- "7"
P5B$P5Q4_3[P5B$P5Q4_R_7 == "3"] <- "7"

P5B$P5Q4_1[P5B$P5Q4_R_8 == "1"] <- "8"
P5B$P5Q4_2[P5B$P5Q4_R_8 == "2"] <- "8"
P5B$P5Q4_3[P5B$P5Q4_R_8 == "3"] <- "8"

P5B$P5Q4_1[P5B$P5Q4_R_9 == "1"] <- "9"
P5B$P5Q4_2[P5B$P5Q4_R_9 == "2"] <- "9"
P5B$P5Q4_3[P5B$P5Q4_R_9 == "3"] <- "9"

P5B$P5Q4_1[P5B$P5Q4_R_10 == "1"] <- "10"
P5B$P5Q4_2[P5B$P5Q4_R_10 == "2"] <- "10"
P5B$P5Q4_3[P5B$P5Q4_R_10 == "3"] <- "10"

P5B$P5Q4_1[P5B$P5Q4_R_11 == "1"] <- "11"
P5B$P5Q4_2[P5B$P5Q4_R_11 == "2"] <- "11"
P5B$P5Q4_3[P5B$P5Q4_R_11 == "3"] <- "11"

P5B$P5Q4_1[P5B$P5Q4_R_12 == "1"] <- "12"
P5B$P5Q4_2[P5B$P5Q4_R_12 == "2"] <- "12"
P5B$P5Q4_3[P5B$P5Q4_R_12 == "3"] <- "12"

P5B$P5Q4_1[P5B$P5Q4_R_13 == "1"] <- "13"
P5B$P5Q4_2[P5B$P5Q4_R_13 == "2"] <- "13"
P5B$P5Q4_3[P5B$P5Q4_R_13 == "3"] <- "13"

P5B$P5Q4_1[P5B$P5Q4_R_14 == "1"] <- "14"
P5B$P5Q4_2[P5B$P5Q4_R_14 == "2"] <- "14"
P5B$P5Q4_3[P5B$P5Q4_R_14 == "3"] <- "14"

P5B$P5Q4_1[P5B$P5Q4_R_15 == "1"] <- "15"
P5B$P5Q4_2[P5B$P5Q4_R_15 == "2"] <- "15"
P5B$P5Q4_3[P5B$P5Q4_R_15 == "3"] <- "15"

P5B$P5Q4_1[P5B$P5Q4_R_16 == "1"] <- "16"
P5B$P5Q4_2[P5B$P5Q4_R_16 == "2"] <- "16"
P5B$P5Q4_3[P5B$P5Q4_R_16 == "3"] <- "16"

##################################################

P5A$P5Q6_1[P5A$P5Q6_R_1 == "1"] <- "1"
P5A$P5Q6_2[P5A$P5Q6_R_1 == "2"] <- "1"
P5A$P5Q6_3[P5A$P5Q6_R_1 == "3"] <- "1"

P5A$P5Q6_1[P5A$P5Q6_R_2 == "1"] <- "2"
P5A$P5Q6_2[P5A$P5Q6_R_2 == "2"] <- "2"
P5A$P5Q6_3[P5A$P5Q6_R_2 == "3"] <- "2"

P5A$P5Q6_1[P5A$P5Q6_R_3 == "1"] <- "3"
P5A$P5Q6_2[P5A$P5Q6_R_3 == "2"] <- "3"
P5A$P5Q6_3[P5A$P5Q6_R_3 == "3"] <- "3"

P5A$P5Q6_1[P5A$P5Q6_R_4 == "1"] <- "4"
P5A$P5Q6_2[P5A$P5Q6_R_4 == "2"] <- "4"
P5A$P5Q6_3[P5A$P5Q6_R_4 == "3"] <- "4"

P5A$P5Q6_1[P5A$P5Q6_R_5 == "1"] <- "5"
P5A$P5Q6_2[P5A$P5Q6_R_5 == "2"] <- "5"
P5A$P5Q6_3[P5A$P5Q6_R_5 == "3"] <- "5"

P5A$P5Q6_1[P5A$P5Q6_R_6 == "1"] <- "6"
P5A$P5Q6_2[P5A$P5Q6_R_6 == "2"] <- "6"
P5A$P5Q6_3[P5A$P5Q6_R_6 == "3"] <- "6"

P5A$P5Q6_1[P5A$P5Q6_R_7 == "1"] <- "7"
P5A$P5Q6_2[P5A$P5Q6_R_7 == "2"] <- "7"
P5A$P5Q6_3[P5A$P5Q6_R_7 == "3"] <- "7"

P5A$P5Q6_1[P5A$P5Q6_R_8 == "1"] <- "8"
P5A$P5Q6_2[P5A$P5Q6_R_8 == "2"] <- "8"
P5A$P5Q6_3[P5A$P5Q6_R_8 == "3"] <- "8"

P5A$P5Q6_1[P5A$P5Q6_R_9 == "1"] <- "9"
P5A$P5Q6_2[P5A$P5Q6_R_9 == "2"] <- "9"
P5A$P5Q6_3[P5A$P5Q6_R_9 == "3"] <- "9"

P5A$P5Q6_1[P5A$P5Q6_R_10 == "1"] <- "10"
P5A$P5Q6_2[P5A$P5Q6_R_10 == "2"] <- "10"
P5A$P5Q6_3[P5A$P5Q6_R_10 == "3"] <- "10"

##################################################

P5B$P5Q6_1[P5B$P5Q6_R_1 == "1"] <- "1"
P5B$P5Q6_2[P5B$P5Q6_R_1 == "2"] <- "1"
P5B$P5Q6_3[P5B$P5Q6_R_1 == "3"] <- "1"

P5B$P5Q6_1[P5B$P5Q6_R_2 == "1"] <- "2"
P5B$P5Q6_2[P5B$P5Q6_R_2 == "2"] <- "2"
P5B$P5Q6_3[P5B$P5Q6_R_2 == "3"] <- "2"

P5B$P5Q6_1[P5B$P5Q6_R_3 == "1"] <- "3"
P5B$P5Q6_2[P5B$P5Q6_R_3 == "2"] <- "3"
P5B$P5Q6_3[P5B$P5Q6_R_3 == "3"] <- "3"

P5B$P5Q6_1[P5B$P5Q6_R_4 == "1"] <- "4"
P5B$P5Q6_2[P5B$P5Q6_R_4 == "2"] <- "4"
P5B$P5Q6_3[P5B$P5Q6_R_4 == "3"] <- "4"

P5B$P5Q6_1[P5B$P5Q6_R_5 == "1"] <- "5"
P5B$P5Q6_2[P5B$P5Q6_R_5 == "2"] <- "5"
P5B$P5Q6_3[P5B$P5Q6_R_5 == "3"] <- "5"

P5B$P5Q6_1[P5B$P5Q6_R_6 == "1"] <- "6"
P5B$P5Q6_2[P5B$P5Q6_R_6 == "2"] <- "6"
P5B$P5Q6_3[P5B$P5Q6_R_6 == "3"] <- "6"

P5B$P5Q6_1[P5B$P5Q6_R_7 == "1"] <- "7"
P5B$P5Q6_2[P5B$P5Q6_R_7 == "2"] <- "7"
P5B$P5Q6_3[P5B$P5Q6_R_7 == "3"] <- "7"

P5B$P5Q6_1[P5B$P5Q6_R_8 == "1"] <- "8"
P5B$P5Q6_2[P5B$P5Q6_R_8 == "2"] <- "8"
P5B$P5Q6_3[P5B$P5Q6_R_8 == "3"] <- "8"

P5B$P5Q6_1[P5B$P5Q6_R_9 == "1"] <- "9"
P5B$P5Q6_2[P5B$P5Q6_R_9 == "2"] <- "9"
P5B$P5Q6_3[P5B$P5Q6_R_9 == "3"] <- "9"

P5B$P5Q6_1[P5B$P5Q6_R_10 == "1"] <- "10"
P5B$P5Q6_2[P5B$P5Q6_R_10 == "2"] <- "10"
P5B$P5Q6_3[P5B$P5Q6_R_10 == "3"] <- "10"

##################################################

P5A$P5Q19_1 <- P5A$P5Q19_Nestle
P5A$P5Q19_2 <- P5A$P5Q19_Centrum
P5A$P5Q19_3 <- P5A$P5Q19_Abbott
P5A$P5Q19_4 <- P5A$P5Q19_Mamacare
P5A$P5Q19_5 <- P5A$P5Q19_Blackmores
P5A$P5Q19_7 <- P5A$P5Q19_GNC

##################################################

P5B$P5Q19_1 <- P5B$P5Q19_Nestle
P5B$P5Q19_2 <- P5B$P5Q19_Centrum
P5B$P5Q19_3 <- P5B$P5Q19_Abbott
P5B$P5Q19_4 <- P5B$P5Q19_Mamacare
P5B$P5Q19_5 <- P5B$P5Q19_Blackmores
P5B$P5Q19_7 <- P5B$P5Q19_GNC

##################################################

P5A$P5Q20_11 <- P5A$P5Q20_NOW
P5A$P5Q20_12 <- P5A$P5Q20_Wyeth
P5A$P5Q20_13 <- P5A$P5Q20_Horeb
P5A$P5Q20_14 <- P5A$P5Q20_MJN
P5A$P5Q20_15 <- P5A$P5Q20_Nolbel
P5A$P5Q20_16 <- P5A$P5Q20_Mamacare

##################################################

P5B$P5Q20_11 <- P5B$P5Q20_NOW
P5B$P5Q20_12 <- P5B$P5Q20_Wyeth
P5B$P5Q20_13 <- P5B$P5Q20_Horeb
P5B$P5Q20_14 <- P5B$P5Q20_MJN
P5B$P5Q20_15 <- P5B$P5Q20_Nolbel
P5B$P5Q20_16 <- P5B$P5Q20_Mamacare

##################################################

P5A$P5Q21_1 <- P5A$P5Q21_MJN
P5A$P5Q21_2 <- P5A$P5Q21_Abbott
P5A$P5Q21_3 <- P5A$P5Q21_Meiji
P5A$P5Q21_4 <- P5A$P5Q21_Anmum
P5A$P5Q21_5 <- P5A$P5Q21_Quaker
P5A$P5Q21_6 <- P5A$P5Q21_Neoangelac

##################################################

P5B$P5Q21_1 <- P5B$P5Q21_MJN
P5B$P5Q21_2 <- P5B$P5Q21_Abbott
P5B$P5Q21_3 <- P5B$P5Q21_Meiji
P5B$P5Q21_4 <- P5B$P5Q21_Anmum
P5B$P5Q21_5 <- P5B$P5Q21_Quaker
P5B$P5Q21_6 <- P5B$P5Q21_Neoangelac

##################################################

P5A$P5Q22
P5B$P5Q22

##################################################

P5A$P5Q23
P5B$P5Q23

##################################################

P5A$P5Q24_rank1[P5A$P5Q24_Nestle == "1"] <- "1"
P5A$P5Q24_rank2[P5A$P5Q24_Nestle == "2"] <- "1"
P5A$P5Q24_rank3[P5A$P5Q24_Nestle == "3"] <- "1"
P5A$P5Q24_rank4[P5A$P5Q24_Nestle == "4"] <- "1"
P5A$P5Q24_rank5[P5A$P5Q24_Nestle == "5"] <- "1"
P5A$P5Q24_rank6[P5A$P5Q24_Nestle == "6"] <- "1"
P5A$P5Q24_rank7[P5A$P5Q24_Nestle == "7"] <- "1"

P5A$P5Q24_rank1[P5A$P5Q24_Wyeth == "1"] <- "2"
P5A$P5Q24_rank2[P5A$P5Q24_Wyeth == "2"] <- "2"
P5A$P5Q24_rank3[P5A$P5Q24_Wyeth == "3"] <- "2"
P5A$P5Q24_rank4[P5A$P5Q24_Wyeth == "4"] <- "2"
P5A$P5Q24_rank5[P5A$P5Q24_Wyeth == "5"] <- "2"
P5A$P5Q24_rank6[P5A$P5Q24_Wyeth == "6"] <- "2"
P5A$P5Q24_rank7[P5A$P5Q24_Wyeth == "7"] <- "2"

P5A$P5Q24_rank1[P5A$P5Q24_MJN == "1"] <- "3"
P5A$P5Q24_rank2[P5A$P5Q24_MJN == "2"] <- "3"
P5A$P5Q24_rank3[P5A$P5Q24_MJN == "3"] <- "3"
P5A$P5Q24_rank4[P5A$P5Q24_MJN == "4"] <- "3"
P5A$P5Q24_rank5[P5A$P5Q24_MJN == "5"] <- "3"
P5A$P5Q24_rank6[P5A$P5Q24_MJN == "6"] <- "3"
P5A$P5Q24_rank7[P5A$P5Q24_MJN == "7"] <- "3"

P5A$P5Q24_rank1[P5A$P5Q24_Abbott == "1"] <- "4"
P5A$P5Q24_rank2[P5A$P5Q24_Abbott == "2"] <- "4"
P5A$P5Q24_rank3[P5A$P5Q24_Abbott == "3"] <- "4"
P5A$P5Q24_rank4[P5A$P5Q24_Abbott == "4"] <- "4"
P5A$P5Q24_rank5[P5A$P5Q24_Abbott == "5"] <- "4"
P5A$P5Q24_rank6[P5A$P5Q24_Abbott == "6"] <- "4"
P5A$P5Q24_rank7[P5A$P5Q24_Abbott == "7"] <- "4"

P5A$P5Q24_rank1[P5A$P5Q24_Meiji == "1"] <- "5"
P5A$P5Q24_rank2[P5A$P5Q24_Meiji == "2"] <- "5"
P5A$P5Q24_rank3[P5A$P5Q24_Meiji == "3"] <- "5"
P5A$P5Q24_rank4[P5A$P5Q24_Meiji == "4"] <- "5"
P5A$P5Q24_rank5[P5A$P5Q24_Meiji == "5"] <- "5"
P5A$P5Q24_rank6[P5A$P5Q24_Meiji == "6"] <- "5"
P5A$P5Q24_rank7[P5A$P5Q24_Meiji == "7"] <- "5"

P5A$P5Q24_rank1[P5A$P5Q24_Quaker == "1"] <- "6"
P5A$P5Q24_rank2[P5A$P5Q24_Quaker == "2"] <- "6"
P5A$P5Q24_rank3[P5A$P5Q24_Quaker == "3"] <- "6"
P5A$P5Q24_rank4[P5A$P5Q24_Quaker == "4"] <- "6"
P5A$P5Q24_rank5[P5A$P5Q24_Quaker == "5"] <- "6"
P5A$P5Q24_rank6[P5A$P5Q24_Quaker == "6"] <- "6"
P5A$P5Q24_rank7[P5A$P5Q24_Quaker == "7"] <- "6"

P5A$P5Q24_rank1[P5A$P5Q24_Snow == "1"] <- "7"
P5A$P5Q24_rank2[P5A$P5Q24_Snow == "2"] <- "7"
P5A$P5Q24_rank3[P5A$P5Q24_Snow == "3"] <- "7"
P5A$P5Q24_rank4[P5A$P5Q24_Snow == "4"] <- "7"
P5A$P5Q24_rank5[P5A$P5Q24_Snow == "5"] <- "7"
P5A$P5Q24_rank6[P5A$P5Q24_Snow == "6"] <- "7"
P5A$P5Q24_rank7[P5A$P5Q24_Snow == "7"] <- "7"

##################################################

P5B$P5Q24_rank1[P5B$P5Q24_Nestle == "1"] <- "1"
P5B$P5Q24_rank2[P5B$P5Q24_Nestle == "2"] <- "1"
P5B$P5Q24_rank3[P5B$P5Q24_Nestle == "3"] <- "1"
P5B$P5Q24_rank4[P5B$P5Q24_Nestle == "4"] <- "1"
P5B$P5Q24_rank5[P5B$P5Q24_Nestle == "5"] <- "1"
P5B$P5Q24_rank6[P5B$P5Q24_Nestle == "6"] <- "1"
P5B$P5Q24_rank7[P5B$P5Q24_Nestle == "7"] <- "1"

P5B$P5Q24_rank1[P5B$P5Q24_Wyeth == "1"] <- "2"
P5B$P5Q24_rank2[P5B$P5Q24_Wyeth == "2"] <- "2"
P5B$P5Q24_rank3[P5B$P5Q24_Wyeth == "3"] <- "2"
P5B$P5Q24_rank4[P5B$P5Q24_Wyeth == "4"] <- "2"
P5B$P5Q24_rank5[P5B$P5Q24_Wyeth == "5"] <- "2"
P5B$P5Q24_rank6[P5B$P5Q24_Wyeth == "6"] <- "2"
P5B$P5Q24_rank7[P5B$P5Q24_Wyeth == "7"] <- "2"

P5B$P5Q24_rank1[P5B$P5Q24_MJN == "1"] <- "3"
P5B$P5Q24_rank2[P5B$P5Q24_MJN == "2"] <- "3"
P5B$P5Q24_rank3[P5B$P5Q24_MJN == "3"] <- "3"
P5B$P5Q24_rank4[P5B$P5Q24_MJN == "4"] <- "3"
P5B$P5Q24_rank5[P5B$P5Q24_MJN == "5"] <- "3"
P5B$P5Q24_rank6[P5B$P5Q24_MJN == "6"] <- "3"
P5B$P5Q24_rank7[P5B$P5Q24_MJN == "7"] <- "3"

P5B$P5Q24_rank1[P5B$P5Q24_Abbott == "1"] <- "4"
P5B$P5Q24_rank2[P5B$P5Q24_Abbott == "2"] <- "4"
P5B$P5Q24_rank3[P5B$P5Q24_Abbott == "3"] <- "4"
P5B$P5Q24_rank4[P5B$P5Q24_Abbott == "4"] <- "4"
P5B$P5Q24_rank5[P5B$P5Q24_Abbott == "5"] <- "4"
P5B$P5Q24_rank6[P5B$P5Q24_Abbott == "6"] <- "4"
P5B$P5Q24_rank7[P5B$P5Q24_Abbott == "7"] <- "4"

P5B$P5Q24_rank1[P5B$P5Q24_Meiji == "1"] <- "5"
P5B$P5Q24_rank2[P5B$P5Q24_Meiji == "2"] <- "5"
P5B$P5Q24_rank3[P5B$P5Q24_Meiji == "3"] <- "5"
P5B$P5Q24_rank4[P5B$P5Q24_Meiji == "4"] <- "5"
P5B$P5Q24_rank5[P5B$P5Q24_Meiji == "5"] <- "5"
P5B$P5Q24_rank6[P5B$P5Q24_Meiji == "6"] <- "5"
P5B$P5Q24_rank7[P5B$P5Q24_Meiji == "7"] <- "5"

P5B$P5Q24_rank1[P5B$P5Q24_Quaker == "1"] <- "6"
P5B$P5Q24_rank2[P5B$P5Q24_Quaker == "2"] <- "6"
P5B$P5Q24_rank3[P5B$P5Q24_Quaker == "3"] <- "6"
P5B$P5Q24_rank4[P5B$P5Q24_Quaker == "4"] <- "6"
P5B$P5Q24_rank5[P5B$P5Q24_Quaker == "5"] <- "6"
P5B$P5Q24_rank6[P5B$P5Q24_Quaker == "6"] <- "6"
P5B$P5Q24_rank7[P5B$P5Q24_Quaker == "7"] <- "6"

##################################################

P5A$P5Q25
P5B$P5Q25

##################################################

P5A$P5Q26_Nestle
P5A$P5Q26_Wyeth
P5A$P5Q26_MJN
P5A$P5Q26_Abbott
P5A$P5Q26_Meiji
P5A$P5Q26_Quaker
P5A$P5Q26_Snow
P5A$P5Q26_Karihome
P5A$P5Q26_Babecare
P5A$P5Q26_Neoangelac

##################################################

P5B$P5Q26_Nestle
P5B$P5Q26_Wyeth
P5B$P5Q26_MJN
P5B$P5Q26_Abbott
P5B$P5Q26_Meiji
P5B$P5Q26_Quaker
P5B$P5Q26_Snow
P5B$P5Q26_Karihome
P5B$P5Q26_Babecare
P5B$P5Q26_Neoangelac

##################################################

P5A$P5Q27_rank1[P5A$P5Q27_Nestle == "1"] <- "1"
P5A$P5Q27_rank2[P5A$P5Q27_Nestle == "2"] <- "1"
P5A$P5Q27_rank3[P5A$P5Q27_Nestle == "3"] <- "1"
P5A$P5Q27_rank4[P5A$P5Q27_Nestle == "4"] <- "1"
P5A$P5Q27_rank5[P5A$P5Q27_Nestle == "5"] <- "1"
P5A$P5Q27_rank6[P5A$P5Q27_Nestle == "6"] <- "1"
P5A$P5Q27_rank7[P5A$P5Q27_Nestle == "7"] <- "1"
P5A$P5Q27_rank8[P5A$P5Q27_Nestle == "8"] <- "1"
P5A$P5Q27_rank9[P5A$P5Q27_Nestle == "9"] <- "1"
P5A$P5Q27_rank10[P5A$P5Q27_Nestle == "10"] <- "1"

P5A$P5Q27_rank1[P5A$P5Q27_Wyeth == "1"] <- "2"
P5A$P5Q27_rank2[P5A$P5Q27_Wyeth == "2"] <- "2"
P5A$P5Q27_rank3[P5A$P5Q27_Wyeth == "3"] <- "2"
P5A$P5Q27_rank4[P5A$P5Q27_Wyeth == "4"] <- "2"
P5A$P5Q27_rank5[P5A$P5Q27_Wyeth == "5"] <- "2"
P5A$P5Q27_rank6[P5A$P5Q27_Wyeth == "6"] <- "2"
P5A$P5Q27_rank7[P5A$P5Q27_Wyeth == "7"] <- "2"
P5A$P5Q27_rank8[P5A$P5Q27_Wyeth == "8"] <- "2"
P5A$P5Q27_rank9[P5A$P5Q27_Wyeth == "9"] <- "2"
P5A$P5Q27_rank10[P5A$P5Q27_Wyeth == "10"] <- "2"

P5A$P5Q27_rank1[P5A$P5Q27_MJN == "1"] <- "3"
P5A$P5Q27_rank2[P5A$P5Q27_MJN == "2"] <- "3"
P5A$P5Q27_rank3[P5A$P5Q27_MJN == "3"] <- "3"
P5A$P5Q27_rank4[P5A$P5Q27_MJN == "4"] <- "3"
P5A$P5Q27_rank5[P5A$P5Q27_MJN == "5"] <- "3"
P5A$P5Q27_rank6[P5A$P5Q27_MJN == "6"] <- "3"
P5A$P5Q27_rank7[P5A$P5Q27_MJN == "7"] <- "3"
P5A$P5Q27_rank8[P5A$P5Q27_MJN == "8"] <- "3"
P5A$P5Q27_rank9[P5A$P5Q27_MJN == "9"] <- "3"
P5A$P5Q27_rank10[P5A$P5Q27_MJN == "10"] <- "3"

P5A$P5Q27_rank1[P5A$P5Q27_Abbott == "1"] <- "4"
P5A$P5Q27_rank2[P5A$P5Q27_Abbott == "2"] <- "4"
P5A$P5Q27_rank3[P5A$P5Q27_Abbott == "3"] <- "4"
P5A$P5Q27_rank4[P5A$P5Q27_Abbott == "4"] <- "4"
P5A$P5Q27_rank5[P5A$P5Q27_Abbott == "5"] <- "4"
P5A$P5Q27_rank6[P5A$P5Q27_Abbott == "6"] <- "4"
P5A$P5Q27_rank7[P5A$P5Q27_Abbott == "7"] <- "4"
P5A$P5Q27_rank8[P5A$P5Q27_Abbott == "8"] <- "4"
P5A$P5Q27_rank9[P5A$P5Q27_Abbott == "9"] <- "4"
P5A$P5Q27_rank10[P5A$P5Q27_Abbott == "10"] <- "4"

P5A$P5Q27_rank1[P5A$P5Q27_Meiji == "1"] <- "5"
P5A$P5Q27_rank2[P5A$P5Q27_Meiji == "2"] <- "5"
P5A$P5Q27_rank3[P5A$P5Q27_Meiji == "3"] <- "5"
P5A$P5Q27_rank4[P5A$P5Q27_Meiji == "4"] <- "5"
P5A$P5Q27_rank5[P5A$P5Q27_Meiji == "5"] <- "5"
P5A$P5Q27_rank6[P5A$P5Q27_Meiji == "6"] <- "5"
P5A$P5Q27_rank7[P5A$P5Q27_Meiji == "7"] <- "5"
P5A$P5Q27_rank8[P5A$P5Q27_Meiji == "8"] <- "5"
P5A$P5Q27_rank9[P5A$P5Q27_Meiji == "9"] <- "5"
P5A$P5Q27_rank10[P5A$P5Q27_Meiji == "10"] <- "5"

P5A$P5Q27_rank1[P5A$P5Q27_Quaker == "1"] <- "6"
P5A$P5Q27_rank2[P5A$P5Q27_Quaker == "2"] <- "6"
P5A$P5Q27_rank3[P5A$P5Q27_Quaker == "3"] <- "6"
P5A$P5Q27_rank4[P5A$P5Q27_Quaker == "4"] <- "6"
P5A$P5Q27_rank5[P5A$P5Q27_Quaker == "5"] <- "6"
P5A$P5Q27_rank6[P5A$P5Q27_Quaker == "6"] <- "6"
P5A$P5Q27_rank7[P5A$P5Q27_Quaker == "7"] <- "6"
P5A$P5Q27_rank8[P5A$P5Q27_Quaker == "8"] <- "6"
P5A$P5Q27_rank9[P5A$P5Q27_Quaker == "9"] <- "6"
P5A$P5Q27_rank10[P5A$P5Q27_Quaker == "10"] <- "6"

P5A$P5Q27_rank1[P5A$P5Q27_Snow == "1"] <- "7"
P5A$P5Q27_rank2[P5A$P5Q27_Snow == "2"] <- "7"
P5A$P5Q27_rank3[P5A$P5Q27_Snow == "3"] <- "7"
P5A$P5Q27_rank4[P5A$P5Q27_Snow == "4"] <- "7"
P5A$P5Q27_rank5[P5A$P5Q27_Snow == "5"] <- "7"
P5A$P5Q27_rank6[P5A$P5Q27_Snow == "6"] <- "7"
P5A$P5Q27_rank7[P5A$P5Q27_Snow == "7"] <- "7"
P5A$P5Q27_rank8[P5A$P5Q27_Snow == "8"] <- "7"
P5A$P5Q27_rank9[P5A$P5Q27_Snow == "9"] <- "7"
P5A$P5Q27_rank10[P5A$P5Q27_Snow == "10"] <- "7"

P5A$P5Q27_rank1[P5A$P5Q27_Karihome == "1"] <- "8"
P5A$P5Q27_rank2[P5A$P5Q27_Karihome == "2"] <- "8"
P5A$P5Q27_rank3[P5A$P5Q27_Karihome == "3"] <- "8"
P5A$P5Q27_rank4[P5A$P5Q27_Karihome == "4"] <- "8"
P5A$P5Q27_rank5[P5A$P5Q27_Karihome == "5"] <- "8"
P5A$P5Q27_rank6[P5A$P5Q27_Karihome == "6"] <- "8"
P5A$P5Q27_rank7[P5A$P5Q27_Karihome == "7"] <- "8"
P5A$P5Q27_rank8[P5A$P5Q27_Karihome == "8"] <- "8"
P5A$P5Q27_rank9[P5A$P5Q27_Karihome == "9"] <- "8"
P5A$P5Q27_rank10[P5A$P5Q27_Karihome == "10"] <- "8"

P5A$P5Q27_rank1[P5A$P5Q27_Babecare == "1"] <- "9"
P5A$P5Q27_rank2[P5A$P5Q27_Babecare == "2"] <- "9"
P5A$P5Q27_rank3[P5A$P5Q27_Babecare == "3"] <- "9"
P5A$P5Q27_rank4[P5A$P5Q27_Babecare == "4"] <- "9"
P5A$P5Q27_rank5[P5A$P5Q27_Babecare == "5"] <- "9"
P5A$P5Q27_rank6[P5A$P5Q27_Babecare == "6"] <- "9"
P5A$P5Q27_rank7[P5A$P5Q27_Babecare == "7"] <- "9"
P5A$P5Q27_rank8[P5A$P5Q27_Babecare == "8"] <- "9"
P5A$P5Q27_rank9[P5A$P5Q27_Babecare == "9"] <- "9"
P5A$P5Q27_rank10[P5A$P5Q27_Babecare == "10"] <- "9"

P5A$P5Q27_rank1[P5A$P5Q27_Neoangelac == "1"] <- "10"
P5A$P5Q27_rank2[P5A$P5Q27_Neoangelac == "2"] <- "10"
P5A$P5Q27_rank3[P5A$P5Q27_Neoangelac == "3"] <- "10"
P5A$P5Q27_rank4[P5A$P5Q27_Neoangelac == "4"] <- "10"
P5A$P5Q27_rank5[P5A$P5Q27_Neoangelac == "5"] <- "10"
P5A$P5Q27_rank6[P5A$P5Q27_Neoangelac == "6"] <- "10"
P5A$P5Q27_rank7[P5A$P5Q27_Neoangelac == "7"] <- "10"
P5A$P5Q27_rank8[P5A$P5Q27_Neoangelac == "8"] <- "10"
P5A$P5Q27_rank9[P5A$P5Q27_Neoangelac == "9"] <- "10"
P5A$P5Q27_rank10[P5A$P5Q27_Neoangelac == "10"] <- "10"

##################################################

P5B$P5Q27_rank1[P5B$P5Q27_Nestle == "1"] <- "1"
P5B$P5Q27_rank2[P5B$P5Q27_Nestle == "2"] <- "1"
P5B$P5Q27_rank3[P5B$P5Q27_Nestle == "3"] <- "1"
P5B$P5Q27_rank4[P5B$P5Q27_Nestle == "4"] <- "1"
P5B$P5Q27_rank5[P5B$P5Q27_Nestle == "5"] <- "1"
P5B$P5Q27_rank6[P5B$P5Q27_Nestle == "6"] <- "1"
P5B$P5Q27_rank7[P5B$P5Q27_Nestle == "7"] <- "1"
P5B$P5Q27_rank8[P5B$P5Q27_Nestle == "8"] <- "1"
P5B$P5Q27_rank9[P5B$P5Q27_Nestle == "9"] <- "1"
P5B$P5Q27_rank10[P5B$P5Q27_Nestle == "10"] <- "1"

P5B$P5Q27_rank1[P5B$P5Q27_Wyeth == "1"] <- "2"
P5B$P5Q27_rank2[P5B$P5Q27_Wyeth == "2"] <- "2"
P5B$P5Q27_rank3[P5B$P5Q27_Wyeth == "3"] <- "2"
P5B$P5Q27_rank4[P5B$P5Q27_Wyeth == "4"] <- "2"
P5B$P5Q27_rank5[P5B$P5Q27_Wyeth == "5"] <- "2"
P5B$P5Q27_rank6[P5B$P5Q27_Wyeth == "6"] <- "2"
P5B$P5Q27_rank7[P5B$P5Q27_Wyeth == "7"] <- "2"
P5B$P5Q27_rank8[P5B$P5Q27_Wyeth == "8"] <- "2"
P5B$P5Q27_rank9[P5B$P5Q27_Wyeth == "9"] <- "2"
P5B$P5Q27_rank10[P5B$P5Q27_Wyeth == "10"] <- "2"

P5B$P5Q27_rank1[P5B$P5Q27_MJN == "1"] <- "3"
P5B$P5Q27_rank2[P5B$P5Q27_MJN == "2"] <- "3"
P5B$P5Q27_rank3[P5B$P5Q27_MJN == "3"] <- "3"
P5B$P5Q27_rank4[P5B$P5Q27_MJN == "4"] <- "3"
P5B$P5Q27_rank5[P5B$P5Q27_MJN == "5"] <- "3"
P5B$P5Q27_rank6[P5B$P5Q27_MJN == "6"] <- "3"
P5B$P5Q27_rank7[P5B$P5Q27_MJN == "7"] <- "3"
P5B$P5Q27_rank8[P5B$P5Q27_MJN == "8"] <- "3"
P5B$P5Q27_rank9[P5B$P5Q27_MJN == "9"] <- "3"
P5B$P5Q27_rank10[P5B$P5Q27_MJN == "10"] <- "3"

P5B$P5Q27_rank1[P5B$P5Q27_Abbott == "1"] <- "4"
P5B$P5Q27_rank2[P5B$P5Q27_Abbott == "2"] <- "4"
P5B$P5Q27_rank3[P5B$P5Q27_Abbott == "3"] <- "4"
P5B$P5Q27_rank4[P5B$P5Q27_Abbott == "4"] <- "4"
P5B$P5Q27_rank5[P5B$P5Q27_Abbott == "5"] <- "4"
P5B$P5Q27_rank6[P5B$P5Q27_Abbott == "6"] <- "4"
P5B$P5Q27_rank7[P5B$P5Q27_Abbott == "7"] <- "4"
P5B$P5Q27_rank8[P5B$P5Q27_Abbott == "8"] <- "4"
P5B$P5Q27_rank9[P5B$P5Q27_Abbott == "9"] <- "4"
P5B$P5Q27_rank10[P5B$P5Q27_Abbott == "10"] <- "4"

P5B$P5Q27_rank1[P5B$P5Q27_Meiji == "1"] <- "5"
P5B$P5Q27_rank2[P5B$P5Q27_Meiji == "2"] <- "5"
P5B$P5Q27_rank3[P5B$P5Q27_Meiji == "3"] <- "5"
P5B$P5Q27_rank4[P5B$P5Q27_Meiji == "4"] <- "5"
P5B$P5Q27_rank5[P5B$P5Q27_Meiji == "5"] <- "5"
P5B$P5Q27_rank6[P5B$P5Q27_Meiji == "6"] <- "5"
P5B$P5Q27_rank7[P5B$P5Q27_Meiji == "7"] <- "5"
P5B$P5Q27_rank8[P5B$P5Q27_Meiji == "8"] <- "5"
P5B$P5Q27_rank9[P5B$P5Q27_Meiji == "9"] <- "5"
P5B$P5Q27_rank10[P5B$P5Q27_Meiji == "10"] <- "5"

P5B$P5Q27_rank1[P5B$P5Q27_Quaker == "1"] <- "6"
P5B$P5Q27_rank2[P5B$P5Q27_Quaker == "2"] <- "6"
P5B$P5Q27_rank3[P5B$P5Q27_Quaker == "3"] <- "6"
P5B$P5Q27_rank4[P5B$P5Q27_Quaker == "4"] <- "6"
P5B$P5Q27_rank5[P5B$P5Q27_Quaker == "5"] <- "6"
P5B$P5Q27_rank6[P5B$P5Q27_Quaker == "6"] <- "6"
P5B$P5Q27_rank7[P5B$P5Q27_Quaker == "7"] <- "6"
P5B$P5Q27_rank8[P5B$P5Q27_Quaker == "8"] <- "6"
P5B$P5Q27_rank9[P5B$P5Q27_Quaker == "9"] <- "6"
P5B$P5Q27_rank10[P5B$P5Q27_Quaker == "10"] <- "6"

P5B$P5Q27_rank1[P5B$P5Q27_Snow == "1"] <- "7"
P5B$P5Q27_rank2[P5B$P5Q27_Snow == "2"] <- "7"
P5B$P5Q27_rank3[P5B$P5Q27_Snow == "3"] <- "7"
P5B$P5Q27_rank4[P5B$P5Q27_Snow == "4"] <- "7"
P5B$P5Q27_rank5[P5B$P5Q27_Snow == "5"] <- "7"
P5B$P5Q27_rank6[P5B$P5Q27_Snow == "6"] <- "7"
P5B$P5Q27_rank7[P5B$P5Q27_Snow == "7"] <- "7"
P5B$P5Q27_rank8[P5B$P5Q27_Snow == "8"] <- "7"
P5B$P5Q27_rank9[P5B$P5Q27_Snow == "9"] <- "7"
P5B$P5Q27_rank10[P5B$P5Q27_Snow == "10"] <- "7"

P5B$P5Q27_rank1[P5B$P5Q27_Karihome == "1"] <- "8"
P5B$P5Q27_rank2[P5B$P5Q27_Karihome == "2"] <- "8"
P5B$P5Q27_rank3[P5B$P5Q27_Karihome == "3"] <- "8"
P5B$P5Q27_rank4[P5B$P5Q27_Karihome == "4"] <- "8"
P5B$P5Q27_rank5[P5B$P5Q27_Karihome == "5"] <- "8"
P5B$P5Q27_rank6[P5B$P5Q27_Karihome == "6"] <- "8"
P5B$P5Q27_rank7[P5B$P5Q27_Karihome == "7"] <- "8"
P5B$P5Q27_rank8[P5B$P5Q27_Karihome == "8"] <- "8"
P5B$P5Q27_rank9[P5B$P5Q27_Karihome == "9"] <- "8"
P5B$P5Q27_rank10[P5B$P5Q27_Karihome == "10"] <- "8"

P5B$P5Q27_rank1[P5B$P5Q27_Babecare == "1"] <- "9"
P5B$P5Q27_rank2[P5B$P5Q27_Babecare == "2"] <- "9"
P5B$P5Q27_rank3[P5B$P5Q27_Babecare == "3"] <- "9"
P5B$P5Q27_rank4[P5B$P5Q27_Babecare == "4"] <- "9"
P5B$P5Q27_rank5[P5B$P5Q27_Babecare == "5"] <- "9"
P5B$P5Q27_rank6[P5B$P5Q27_Babecare == "6"] <- "9"
P5B$P5Q27_rank7[P5B$P5Q27_Babecare == "7"] <- "9"
P5B$P5Q27_rank8[P5B$P5Q27_Babecare == "8"] <- "9"
P5B$P5Q27_rank9[P5B$P5Q27_Babecare == "9"] <- "9"
P5B$P5Q27_rank10[P5B$P5Q27_Babecare == "10"] <- "9"

P5B$P5Q27_rank1[P5B$P5Q27_Neoangelac == "1"] <- "10"
P5B$P5Q27_rank2[P5B$P5Q27_Neoangelac == "2"] <- "10"
P5B$P5Q27_rank3[P5B$P5Q27_Neoangelac == "3"] <- "10"
P5B$P5Q27_rank4[P5B$P5Q27_Neoangelac == "4"] <- "10"
P5B$P5Q27_rank5[P5B$P5Q27_Neoangelac == "5"] <- "10"
P5B$P5Q27_rank6[P5B$P5Q27_Neoangelac == "6"] <- "10"
P5B$P5Q27_rank7[P5B$P5Q27_Neoangelac == "7"] <- "10"
P5B$P5Q27_rank8[P5B$P5Q27_Neoangelac == "8"] <- "10"
P5B$P5Q27_rank9[P5B$P5Q27_Neoangelac == "9"] <- "10"
P5B$P5Q27_rank10[P5B$P5Q27_Neoangelac == "10"] <- "10"

##################################################

P5A$P5Q28
P5A$P5Q28_98

P5A$P5Q28[nchar(P5A$P5Q28_98) > 0] <- "98"
table(P5A$P5Q28)

##################################################

P5B$P5Q28
P5B$P5Q28_98

P5B$P5Q28[nchar(P5B$P5Q28_98) > 0] <- "98"
table(P5B$P5Q28)

##################################################

P5A$P5Q29
P5A$P5Q29_98

P5A$P5Q29[nchar(P5A$P5Q29_98) > 0] <- "98"
table(P5A$P5Q29)

##################################################

P5B$P5Q29
P5B$P5Q29_98

P5B$P5Q29[nchar(P5B$P5Q29_98) > 0] <- "98"
table(P5B$P5Q29)

##################################################

P5A$P5Q30
P5A$P5Q30_7

P5A$P5Q30[nchar(P5A$P5Q30_7) > 0] <- "7"
table(P5A$P5Q30)

##################################################

P5B$P5Q30
P5B$P5Q30_7

P5B$P5Q30[nchar(P5B$P5Q30_7) > 0] <- "7"
table(P5B$P5Q30)

##################################################

P5A$P5Q31
P5A$P5Q31_10

P5A$P5Q31[nchar(P5A$P5Q31_10) > 0] <- "10"
table(P5A$P5Q31)

##################################################

P5B$P5Q31
P5B$P5Q31_10

P5B$P5Q31[nchar(P5B$P5Q31_10) > 0] <- "10"
table(P5B$P5Q31)

##################################################

P5A$BirthDay5 <- as.Date(P5A$BirthDay5, format = "%Y/%m/%d")
P5A$BirthDay5 <- gsub("-", "/", P5A$BirthDay5); P5A$BirthDay5

P5B$BirthDay5 <- as.Date(P5B$BirthDay5, format = "%Y/%m/%d")
P5B$BirthDay5 <- gsub("-", "/", P5B$BirthDay5); P5B$BirthDay5

P5A$TakeDay5 <- as.Date(P5A$TakeDay5, format = "%Y/%m/%d")
P5A$TakeDay5 <- gsub("-", "/", P5A$TakeDay5); P5A$TakeDay5

P5B$TakeDay5 <- as.Date(P5B$TakeDay5, format = "%Y/%m/%d")
P5B$TakeDay5 <- gsub("-", "/", P5B$TakeDay5); P5B$TakeDay5

P5A$SendDay5 <- as.Date(P5A$SendDay5, format = "%Y/%m/%d")
P5A$SendDay5 <- gsub("-", "/", P5A$SendDay5); P5A$SendDay5

P5B$SendDay5 <- as.Date(P5B$SendDay5, format = "%Y/%m/%d")
P5B$SendDay5 <- gsub("-", "/", P5B$SendDay5); P5B$SendDay5

P5A$RegisterDay5 <- as.Date(P5A$RegisterDay5, format = "%Y/%m/%d")
P5A$RegisterDay5 <- gsub("-", "/", P5A$RegisterDay5); P5A$RegisterDay5

P5B$RegisterDay5 <- as.Date(P5B$RegisterDay5, format = "%Y/%m/%d")
P5B$RegisterDay5 <- gsub("-", "/", P5B$RegisterDay5); P5B$RegisterDay5

##################################################

P5A$StartDay5 <- as.Date(P5A$StartDay5, format = "%m/%d/%Y")
P5A$StartDay5 <- gsub("-", "/", P5A$StartDay5); P5A$StartDay5

P5B$StartDay5 <- as.Date(P5B$StartDay5, format = "%m/%d/%Y")
P5B$StartDay5 <- gsub("-", "/", P5B$StartDay5); P5B$StartDay5

P5A$EndDay5 <- as.Date(P5A$EndDay5, format = "%m/%d/%Y")
P5A$EndDay5 <- gsub("-", "/", P5A$EndDay5); P5A$EndDay5

P5B$EndDay5 <- as.Date(P5B$EndDay5, format = "%m/%d/%Y")
P5B$EndDay5 <- gsub("-", "/", P5B$EndDay5); P5B$EndDay5

##################################################

P5A$Month5 <- "5"
P5B$Month5 <- "5"

P5A$GETENDDay5 <- as.Date("2017/04/30", format = "%Y/%m/%d")
P5A$GETENDDay5 <- gsub("-", "/", P5A$GETENDDay5); P5A$GETENDDay5

P5B$GETENDDay5 <- as.Date("2017/04/30", format = "%Y/%m/%d")
P5B$GETENDDay5 <- gsub("-", "/", P5B$GETENDDay5); P5B$GETENDDay5

##################################################

P5A$MemberDay
P5B$MemberDay

P5A$P5Q27_1
P5B$P5Q27_1

P5A$P5Q26_Nestle_Rank
P5B$P5Q26_Nestle_Rank

P5A$NP5Q26_Nestle_Rank
P5B$NP5Q26_Nestle_Rank

##################################################

P5A <- select(P5A, -P5Q1)
P5A <- select(P5A, -P5Q2_3)
P5A <- select(P5A, -P5Q3_3)
P5A <- select(P5A, -P5Q4_R_1, -P5Q4_R_2, -P5Q4_R_3, -P5Q4_R_4, -P5Q4_R_5, -P5Q4_R_6, -P5Q4_R_7, -P5Q4_R_8, -P5Q4_R_9, -P5Q4_R_10, -P5Q4_R_11, -P5Q4_R_12, -P5Q4_R_13, -P5Q4_R_14, -P5Q4_R_15, -P5Q4_R_16)
P5A <- select(P5A, -P5Q5_1, -P5Q5_3, -P5Q5_4, -P5Q5_5, -P5Q5_6, -P5Q5_7, -P5Q5_8, -P5Q5_9, -P5Q5_10, -P5Q5_11)
P5A <- select(P5A, -P5Q6_R_1, -P5Q6_R_3, -P5Q6_R_4, -P5Q6_R_5, -P5Q6_R_6, -P5Q6_R_7, -P5Q6_R_8, -P5Q6_R_9, -P5Q6_R_10)
P5A <- select(P5A, -P5Q12_1, -P5Q12_2, -P5Q12_3, -P5Q12_4, -P5Q12_5, -P5Q12_6, -P5Q12_7, -P5Q12_8, -P5Q12_9, -P5Q12_10, -P5Q12_99)
P5A <- select(P5A, -P5Q13_99, -P5Q13_1, -P5Q13_2, -P5Q13_3, -P5Q13_4, -P5Q13_5, -P5Q13_6, -P5Q13_7, -P5Q13_8, -P5Q13_9, -P5Q13_10, -P5Q13_98)
P5A <- select(P5A, -P5Q14_99, -P5Q14_1, -P5Q14_2, -P5Q14_3, -P5Q14_4, -P5Q14_5, -P5Q14_6, -P5Q14_7, -P5Q14_8, -P5Q14_9, -P5Q14_10, -P5Q14_98)
P5A <- select(P5A, -P5Q15_99, -P5Q15_1, -P5Q15_2, -P5Q15_3, -P5Q15_4, -P5Q15_5, -P5Q15_6, -P5Q15_7, -P5Q15_8, -P5Q15_9, -P5Q15_10, -P5Q15_98)
P5A <- select(P5A, -P5Q16_1, -P5Q16_2, -P5Q16_3, -P5Q16_4, -P5Q16_5, -P5Q16_6, -P5Q16_7, -P5Q16_8, -P5Q16_9, -P5Q16_10, -P5Q16_11, -P5Q16_12)
P5A <- select(P5A, -P5Q17_11, -P5Q17_12, -P5Q17_13, -P5Q17_14, -P5Q17_15, -P5Q17_16, -P5Q17_99)
P5A <- select(P5A, -P5Q18_1, -P5Q18_2, -P5Q18_3, -P5Q18_4, -P5Q18_5, -P5Q18_6, -P5Q18_7)
P5A <- select(P5A, -P5Q19_Never, -P5Q19_Nestle, -P5Q19_Centrum, -P5Q19_Abbott, -P5Q19_Mamacare, -P5Q19_Blackmores, -P5Q19_Nolbel, -P5Q19_GNC, -P5Q19_MA, -P5Q19_Sundown, -P5Q19_DK)
P5A <- select(P5A, -P5Q20_NOW, -P5Q20_Wyeth, -P5Q20_Horeb, -P5Q20_MJN, -P5Q20_Nolbel, -P5Q20_Mamacare)
P5A <- select(P5A, -P5Q21_MJN, -P5Q21_Abbott,	-P5Q21_Meiji,	-P5Q21_Anmum,	-P5Q21_Quaker, -P5Q21_Neoangelac)
P5A <- select(P5A, -P5Q24_Nestle, -P5Q24_Wyeth,	-P5Q24_MJN, -P5Q24_Abbott, -P5Q24_Meiji, -P5Q24_Quaker, -P5Q24_Snow)
P5A <- select(P5A, -P5Q27_Nestle, -P5Q27_Wyeth, -P5Q27_MJN, -P5Q27_Abbott, -P5Q27_Meiji, -P5Q27_Quaker, -P5Q27_Snow, -P5Q27_Karihome, -P5Q27_Babecare, -P5Q27_Neoangelac)
P5A <- select(P5A, -P5Q28_98)
P5A <- select(P5A, -P5Q29_98)
P5A <- select(P5A, -P5Q30_7)

P5A[is.na(P5A)] <- ""

write.csv(P5A, "P5A.csv", row.names = TRUE)
str(P5A)

##################################################

P5B <- select(P5B, -P5Q1)
P5B <- select(P5B, -P5Q2_3)
P5B <- select(P5B, -P5Q3_3)
P5B <- select(P5B, -P5Q4_R_1, -P5Q4_R_2, -P5Q4_R_3, -P5Q4_R_4, -P5Q4_R_5, -P5Q4_R_6, -P5Q4_R_7, -P5Q4_R_8, -P5Q4_R_9, -P5Q4_R_10, -P5Q4_R_11, -P5Q4_R_12, -P5Q4_R_13, -P5Q4_R_14, -P5Q4_R_15, -P5Q4_R_16)
P5B <- select(P5B, -P5Q5_1, -P5Q5_3, -P5Q5_4, -P5Q5_5, -P5Q5_6, -P5Q5_7, -P5Q5_8, -P5Q5_9, -P5Q5_10, -P5Q5_11)
P5B <- select(P5B, -P5Q6_R_1, -P5Q6_R_3, -P5Q6_R_4, -P5Q6_R_5, -P5Q6_R_6, -P5Q6_R_7, -P5Q6_R_8, -P5Q6_R_9, -P5Q6_R_10)
P5B <- select(P5B, -P5Q12_1, -P5Q12_2, -P5Q12_3, -P5Q12_4, -P5Q12_5, -P5Q12_6, -P5Q12_7, -P5Q12_8, -P5Q12_9, -P5Q12_10, -P5Q12_99)
P5B <- select(P5B, -P5Q13_99, -P5Q13_1, -P5Q13_2, -P5Q13_3, -P5Q13_4, -P5Q13_5, -P5Q13_6, -P5Q13_7, -P5Q13_8, -P5Q13_9, -P5Q13_10, -P5Q13_98)
P5B <- select(P5B, -P5Q14_99, -P5Q14_1, -P5Q14_2, -P5Q14_3, -P5Q14_4, -P5Q14_5, -P5Q14_6, -P5Q14_7, -P5Q14_8, -P5Q14_9, -P5Q14_10, -P5Q14_98)
P5B <- select(P5B, -P5Q15_99, -P5Q15_1, -P5Q15_2, -P5Q15_3, -P5Q15_4, -P5Q15_5, -P5Q15_6, -P5Q15_7, -P5Q15_8, -P5Q15_9, -P5Q15_10, -P5Q15_98)
P5B <- select(P5B, -P5Q16_1, -P5Q16_2, -P5Q16_3, -P5Q16_4, -P5Q16_5, -P5Q16_6, -P5Q16_7, -P5Q16_8, -P5Q16_9, -P5Q16_10, -P5Q16_11, -P5Q16_12)
P5B <- select(P5B, -P5Q17_11, -P5Q17_12, -P5Q17_13, -P5Q17_14, -P5Q17_15, -P5Q17_16, -P5Q17_99)
P5B <- select(P5B, -P5Q18_1, -P5Q18_2, -P5Q18_3, -P5Q18_4, -P5Q18_5, -P5Q18_6, -P5Q18_7)
P5B <- select(P5B, -P5Q19_Never, -P5Q19_Nestle, -P5Q19_Centrum, -P5Q19_Abbott, -P5Q19_Mamacare, -P5Q19_Blackmores, -P5Q19_Nolbel, -P5Q19_GNC, -P5Q19_MA, -P5Q19_Sundown, -P5Q19_DK)
P5B <- select(P5B, -P5Q20_NOW, -P5Q20_Wyeth, -P5Q20_Horeb, -P5Q20_MJN, -P5Q20_Nolbel, -P5Q20_Mamacare)
P5B <- select(P5B, -P5Q21_MJN, -P5Q21_Abbott,	-P5Q21_Meiji,	-P5Q21_Anmum,	-P5Q21_Quaker, -P5Q21_Neoangelac)
P5B <- select(P5B, -P5Q24_Nestle, -P5Q24_Wyeth,	-P5Q24_MJN, -P5Q24_Abbott, -P5Q24_Meiji, -P5Q24_Quaker)
P5B <- select(P5B, -P5Q27_Nestle, -P5Q27_Wyeth, -P5Q27_MJN, -P5Q27_Abbott, -P5Q27_Meiji, -P5Q27_Quaker, -P5Q27_Snow, -P5Q27_Karihome, -P5Q27_Babecare, -P5Q27_Neoangelac)
P5B <- select(P5B, -P5Q28_98)
P5B <- select(P5B, -P5Q29_98)
P5B <- select(P5B, -P5Q30_7)

P5B[is.na(P5B)] <- ""

write.csv(P5B, "P5B.csv", row.names = TRUE)
str(P5B)

##################################################

P5A$ID <- P5A$ID5
P5B$ID <- P5B$ID5

P5 <- union(P5A, P5B)
P5[is.na(P5)] <- ""
P5 <- arrange(P5, ID)

write.csv(P5, "P5.csv", row.names = TRUE)

##################################################

P6P5 <- full_join(P6, P5, by = "ID")
P6P5[is.na(P6P5)] <- ""
write.csv(P6P5, "P6P5.csv", row.names = TRUE)

##################################################

P1 <- read.csv("#1_2017.csv", header=TRUE, sep=",")

##################################################

P2A <- read.csv("#2A_2017.csv", header=TRUE, sep=",")

##################################################

P3A <- read.csv("#3A_2017.csv", header=TRUE, sep=",")

##################################################

P4A <- read.csv("#4A_2017.csv", header=TRUE, sep=",")
P4B <- read.csv("#4B_2017.csv", header=TRUE, sep=",")
© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
About
Press h to open a hovercard with more details.
