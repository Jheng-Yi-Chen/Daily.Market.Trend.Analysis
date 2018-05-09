library(dplyr)

##################################################

pro_1 <- select(pro_1, -grep("X.", names(pro_1)))
pro_2 <- select(pro_2, -grep("X.", names(pro_2)))
pro_3 <- select(pro_3, -grep("X.", names(pro_3)))
pro_4 <- select(pro_4, -grep("X.", names(pro_4)))
pro_5 <- select(pro_5, -grep("X.", names(pro_5)))
pro_6 <- select(pro_6, -grep("X.", names(pro_6)))

# pro_1 <- pro_1[-c(1, 2), ]
# pro_2 <- pro_2[-c(1, 2), ]
# pro_3 <- pro_3[-c(1, 2), ]
# pro_4 <- pro_4[-c(1, 2), ]
# pro_5 <- pro_5[-c(1, 2), ]
# pro_6 <- pro_6[-c(1, 2), ]

##################################################

pro_1$ID <- pro_1$ID
pro_2$ID <- pro_2$ID2
pro_3$ID <- pro_3$ID3
pro_4$ID <- pro_4$ID4
pro_5$ID <- pro_5$ID5
pro_6$ID <- pro_6$ID6

final_file <- full_join(pro_1, pro_2, by = c("ID" = "ID"))
final_file <- full_join(final_file, pro_3, by = c("ID" = "ID"))
final_file <- full_join(final_file, pro_4, by = c("ID" = "ID"))
final_file <- full_join(final_file, pro_5, by = c("ID" = "ID"))
final_file <- full_join(final_file, pro_6, by = c("ID" = "ID"))

final_file <- select(final_file, -grep("X.", names(final_file)))
final_file <- select(final_file, -grep("IF.", names(final_file)))

final_file[is.na(final_file)] <- ""
final_file <- arrange(final_file, ID)

write.csv(final_file, "", row.names = TRUE)

##################################################

DF1 <- as.data.frame(pro_1$ID)
DF2 <- as.data.frame(pro_2$ID2)
DF3 <- as.data.frame(pro_3$ID3)
DF4 <- as.data.frame(pro_4$ID4)
DF5 <- as.data.frame(pro_5$ID5)
DF6 <- as.data.frame(pro_6$ID6)

names(DF1) <- c("ID")
names(DF2) <- c("ID")
names(DF3) <- c("ID")
names(DF4) <- c("ID")
names(DF5) <- c("ID")
names(DF6) <- c("ID")

final_DF <- rbind(DF1, DF2, DF3, DF4, DF5, DF6)

##################################################

class(pro_1$ID)
class(pro_2$ID)
class(pro_3$ID)
class(pro_4$ID)
class(pro_5$ID)
class(pro_6$ID)
class(final_DF$ID)

# final_DF$ID_I <- as.integer(final_DF$ID)
# pro_5$ID5_I <- as.integer(pro_5$ID5)
# pro_6$ID6_I <- as.integer(pro_6$ID6)

# left_join(df2, df1, by = c("e"="a"))
final1 <- left_join(x = final_DF, y = pro_1, by = c("ID" = "ID"))
final2 <- left_join(x = final1, y = pro_2, by = c("ID" = "ID"))
final3 <- left_join(x = final2, y = pro_3, by = c("ID" = "ID"))
final4 <- left_join(x = final3, y = pro_4, by = c("ID" = "ID"))
final5 <- left_join(x = final4, y = pro_5, by = c("ID" = "ID"))
final6 <- left_join(x = final5, y = pro_6, by = c("ID" = "ID"))

##################################################

# write.csv(final6, "", row.names = TRUE)

# final_1 <- order(final6, final6$ID_I)
# final6[order(final6$ID_I),]
final_1 <- arrange(final6, ID)

# unique(final_1, nmax = 1)
# final_3 <- as.data.frame(final_2)

final_2 <- unique(final_1, nmax = 1)
# select(final_2, -ID_I)

grep("X.", names(final_2))
grep("IF.", names(final_2))
length(final_2[1, grep("X.", names(final_2))])
final_2[1:2, grep("X.", names(final_2))]

final_3 <- select(final_2, -grep("X.", names(final_2)))
final_4 <- select(final_3, -grep("IF.", names(final_3)))

# names(final_2)[grep("X.", names(final_2))] <- c(".")

# final_3 <- select(final_2, -grep('X.', names(final_2)))
# sub(pattern = "X.", replacement = ".", text) 

str(final_4)

final_4[is.na(final_4)] <- ""
# final_4 <- as.data.frame(gsub("NA", replacement = "", final_3))

write.csv(final_4, "", row.names = TRUE)
# write.table(final_2, "", row.names = TRUE, col.names = FALSE, na = ".", quote = FALSE, sep = "")

# names(table(final_2$ID_I)[table(final_2$ID_I) > 1])
# names(table(final_2$mid)[table(final_2$mid) > 1])

# gsub("x.$", "", final_2[1, ])
# sub("X.", ".", final_2[1, ])
# gsub("_","","123_456_789")
