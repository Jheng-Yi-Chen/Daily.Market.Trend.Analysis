library(dplyr)
library(stringr)

##################################################

P6A <- read.table("#6A_2017_May_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
P6B <- read.table("#6B_2017_May_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

##################################################

P6A[is.na(P6A)] <- ""
P6B[is.na(P6B)] <- ""

P6A <- select(P6A, -grep("x", names(P6A)))
P6B <- select(P6B, -grep("x", names(P6B)))

P6A <- P6A[-c(1), ]
P6B <- P6B[-c(1), ]

##################################################

P6B$P6Q15_99[P6B$P6Q15_99 == "1"] <- "99"
P6B$P6Q15_1[P6B$P6Q15_1 == "2"] <- "1"
P6B$P6Q15_2[P6B$P6Q15_2 == "3"] <- "2"
P6B$P6Q15_3[P6B$P6Q15_3 == "4"] <- "3"
P6B$P6Q15_6[P6B$P6Q15_6 == "7"] <- "6"
P6B$P6Q15_7[P6B$P6Q15_7 == "8"] <- "7"
P6B$P6Q15_8[P6B$P6Q15_8 == "9"] <- "8"
P6B$P6Q15_9[P6B$P6Q15_9 == "10"] <- "9"
P6B$P6Q15_10[P6B$P6Q15_10 == "11"] <- "10"
P6B$P6Q15_98[nchar(P6B$P6Q15_98) > 0] <- "98"

P6B$P6Q15 <- paste(P6B$P6Q15_1, P6B$P6Q15_2, P6B$P6Q15_3, P6B$P6Q15_4, P6B$P6Q15_5, P6B$P6Q15_6, P6B$P6Q15_7, P6B$P6Q15_8, P6B$P6Q15_9, P6B$P6Q15_10, P6B$P6Q15_98, P6B$P6Q15_99, sep = ",")
for(i in 1:5) {
  P6B$P6Q15 <- gsub(",,", ",", P6B$P6Q15); P6B$P6Q15 
}

P6B$P6Q15[str_sub(string = P6B$P6Q15, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q15[str_sub(string = P6B$P6Q15, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q15[str_sub(string = P6B$P6Q15, start = nchar(P6B$P6Q15), end = nchar(P6B$P6Q15)) == ","] <- 
  str_replace(string =  P6B$P6Q15[str_sub(string = P6B$P6Q15, start = nchar(P6B$P6Q15), end = nchar(P6B$P6Q15)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q15

##################################################

P6B$P6Q18 <- paste(P6B$P6Q18_1, P6B$P6Q18_2, P6B$P6Q18_3, P6B$P6Q18_4, P6B$P6Q18_5, P6B$P6Q18_6, P6B$P6Q18_7, sep = ",")

for (i in 1:5){
  P6B$P6Q18 <- gsub(",,", ",", P6B$P6Q18); P6B$P6Q18
}

P6B$P6Q18[str_sub(string = P6B$P6Q18, start = 1, end = 1) == ","] <- str_replace(string =  P6B$P6Q18[str_sub(string = P6B$P6Q18, start = 1, end = 1) == ","], pattern = "^\\,", replacement = "")
P6B$P6Q18[str_sub(string = P6B$P6Q18, start = nchar(P6B$P6Q18), end = nchar(P6B$P6Q18)) == ","] <- 
  str_replace(string =  P6B$P6Q18[str_sub(string = P6B$P6Q18, start = nchar(P6B$P6Q18), end = nchar(P6B$P6Q18)) == ","], pattern = "\\,$", replacement = "")

P6B$P6Q18

##################################################

table(P6A$P6Q2_3)
P6A$P6Q2[nchar(P6A$P6Q2_3) > 0] <- "3"; table(P6A$P6Q2)

table(P6B$P6Q2)
table(P6B$P6Q2_3)
P6B$P6Q2[nchar(P6B$P6Q2_3) > 0] <- "3"; table(P6B$P6Q2)

##################################################

table(P6A$P6Q3_3)
P6A$P6Q3[nchar(P6A$P6Q3_3) > 0] <- "3"; table(P6A$P6Q3)

table(P6B$P6Q3)
table(P6B$P6Q3_3)
P6B$P6Q3[nchar(P6B$P6Q3_3) > 0] <- "3"; table(P6B$P6Q3)

##################################################

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

##################################################

P6A$P6Q30; P6A$P6Q30_7

P6A$P6Q30[nchar(P6A$P6Q30_7) > 0] <- "7"
table(P6A$P6Q30)

##################################################

P6B$P6Q31; P6B$P6Q31_10

P6B$P6Q31[nchar(P6B$P6Q31_10) > 0] <- "10"
table(P6B$P6Q31)

##################################################

P6A$SendDay6 <- as.Date(P6A$SendDay6, format = "%Y/%m/%d")
# P6A$SendDay6 <- gsub("-", "/", P6A$SendDay6); P6A$SendDay6
P6A$EndDay6 <- as.Date(P6A$EndDay6, format = "%m/%d/%Y")
# P6A$EndDay6 <- gsub("-", "/", P6A$EndDay6); P6A$EndDay6

##################################################

P6A <- select(P6A, -P6Q1)
P6A <- select(P6A, -P6Q2_3)
P6A <- select(P6A, -P6Q3_3)

P6A[is.na(P6A)] <- ""

write.csv(P6A, "P6A.csv", row.names = TRUE)
str(P6A)

##################################################

library(dplyr)

P1 <- read.csv("pro#1-2.csv", header=TRUE, sep=",")
P2 <- read.csv("pro#2-2.csv", header=TRUE, sep=",")
P3 <- read.csv("pro#3-2.csv", header=TRUE, sep=",")
P4 <- read.csv("pro#4-2.csv", header=TRUE, sep=",")
P5 <- read.csv("pro#5-2.csv", header=TRUE, sep=",")
P6 <- read.csv("pro#6-2.csv", header=TRUE, sep=",")

##################################################

P1 <- select(P1, -grep("X.", names(P1)))
P2 <- select(P2, -grep("X.", names(P2)))
P3 <- select(P3, -grep("X.", names(P3)))
P4 <- select(P4, -grep("X.", names(P4)))
P5 <- select(P5, -grep("X.", names(P5)))
P6 <- select(P6, -grep("X.", names(P6)))

P1 <- pro_1[-c(1, 2), ]
P2 <- pro_2[-c(1, 2), ]
P3 <- pro_3[-c(1, 2), ]
P4 <- pro_4[-c(1, 2), ]
P5 <- pro_5[-c(1, 2), ]
P6 <- pro_6[-c(1, 2), ]

##################################################

P1$ID <- P1$ID
P2$ID <- P2$ID2
P3$ID <- P3$ID3
P4$ID <- P4$ID4
P5$ID <- P5$ID5
P6$ID <- P6$ID6

final_file <- full_join(P1, P2, by = c("ID" = "ID"))
final_file <- full_join(final_file, P3, by = c("ID" = "ID"))
final_file <- full_join(final_file, P4, by = c("ID" = "ID"))
final_file <- full_join(final_file, P5, by = c("ID" = "ID"))
final_file <- full_join(final_file, P6, by = c("ID" = "ID"))

final_file <- select(final_file, -grep("X.", names(final_file)))
final_file <- select(final_file, -grep("IF.", names(final_file)))

final_file[is.na(final_file)] <- ""

final_file <- arrange(final_file, ID)

write.csv(final_file, "final_file.csv", row.names = TRUE)
