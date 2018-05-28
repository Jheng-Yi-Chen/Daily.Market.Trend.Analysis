
############################################################

library(tm)
library(dplyr)
library(ggplot2)
library(grDevices)

############################################################

news1 <- readLines("news1.txt")
news1a <- removePunctuation(news1)
news1b <- removeNumbers(news1a)
rowlength1 <- length(news1b)
news1c <- paste(news1b[1:rowlength1], collapse = "")
news1d <- gsub(" ", "", news1c)

news1_result2 <- NULL
n1 <- nchar(news1d) # 計算字數

# two character

for (i in 1:n1-1) {
  news1_result2 <- c(news1_result2, substr(news1d, start = i, stop = i+1))
}

news1_result2
news1_result2table <- table(news1_result2)
news1_result2table[1:20]
news1_result2table1 <- sort(news1_result2table, decreasing = TRUE)
news1_result2table1[1:20]

news1_result2table1a <- data.frame("字詞" = names(news1_result2table1), "次數" = as.numeric(news1_result2table1))
news1_result2table1a[1:10, ]

# three character

news1_result3 <- NULL

for (i in 1:n1-1) {
  news1_result3 <- c(news1_result3, substr(news1d, start = i, stop = i+2))
}

news1_result3
news1_result3table <- table(news1_result3)
news1_result3table[1:20]
news1_result3table1 <- sort(news1_result3table, decreasing = TRUE)
news1_result3table1[1:20]

news1_result3table1a <- data.frame("字詞" = names(news1_result3table1), "次數" = as.numeric(news1_result3table1))
news1_result3table1a[1:10, ]

# combining

news1_result2_3 <- rbind(news1_result2table1a[1:10, ], news1_result3table1a[1:10, ])
news1_result2_3

news1_result2_3a <- slice(news1_result2_3, c(1,2,3,4,10,12,17))

# plot

news1_result2_3a %>% 
  ggplot(aes(x = reorder(news1_result2_3a$字詞, -news1_result2_3a$次數), y = news1_result2_3a$次數)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = news1_result2_3a$次數), vjust = 1.5, colour = "white") +
  xlab("字詞") + ylab("次數") +
  theme(axis.text.x = element_text(face = "italic", size = rel(1.5), colour = "darkred")) +
  theme(axis.text.y = element_text(face = "italic", size = rel(1.5), colour = "darkred")) +
  theme(axis.title.x = element_text(size = 16, colour = "darkred", face = "italic")) +
  theme(axis.title.y = element_text(size = 16, colour = "darkred", face = "italic"))

############################################################

news2 <- readLines("news2.txt")
news2a <- removePunctuation(news2)
news2b <- removeNumbers(news2a)
rowlength2 <- length(news2b)
news2c <- paste(news2b[1:rowlength2], collapse = "")
news2d <- gsub(" ", "", news2c)

n2 <- nchar(news2d) # 計算字數

# two character

news2_result2 <- NULL

for (i in 1:n2-1) {
  news2_result2 <- c(news2_result2, substr(news2d, start = i, stop = i+1))
}

news2_result2
news2_result2table <- table(news2_result2)
news2_result2table[1:20]
news2_result2table1 <- sort(news2_result2table, decreasing = TRUE)
news2_result2table1[1:20]

news2_result2table1a <- data.frame("字詞" = names(news2_result2table1), "次數" = as.numeric(news2_result2table1))
news2_result2table1a[1:10, ]

# three character

news2_result3 <- NULL

for (i in 1:n2-1) {
  news2_result3 <- c(news2_result3, substr(news2d, start = i, stop = i+2))
}

news2_result3
news2_result3table <- table(news2_result3)
news2_result3table[1:20]
news2_result3table1 <- sort(news2_result3table, decreasing = TRUE)
news2_result3table1[1:20]

news2_result3table1a <- data.frame("字詞" = names(news2_result3table1), "次數" = as.numeric(news2_result3table1))
news2_result3table1a[1:10, ]

# combining

news2_result2_3 <- rbind(news2_result2table1a[1:10, ], news2_result3table1a[1:10, ])
news2_result2_3

news2_result2_3a <- slice(news2_result2_3, c(1,5,6,9,12,16,17))

# plot

news2_result2_3a %>% 
  ggplot(aes(x = reorder(news2_result2_3a$字詞, -news2_result2_3a$次數), y = news2_result2_3a$次數)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = news2_result2_3a$次數), vjust = 1.5, colour = "white") +
  xlab("字詞") + ylab("次數") +
  theme(axis.text.x = element_text(face = "italic", size = rel(1.5), colour = "blue")) +
  theme(axis.text.y = element_text(face = "italic", size = rel(1.5), colour = "blue")) +
  theme(axis.title.x = element_text(size = 16, colour = "blue4", face = "italic")) +
  theme(axis.title.y = element_text(size = 16, colour = "blue4", face = "italic"))

news2_result2_3a %>% 
  ggplot(aes(x = reorder(news2_result2_3a$字詞, -news2_result2_3a$次數), y = news2_result2_3a$次數)) +
  geom_point(size = 5) +
  xlab("字詞") + ylab("次數") +
  theme(axis.text.x = element_text(face = "italic", size = rel(1.5), colour = "blue")) +
  theme(axis.text.y = element_text(face = "italic", size = rel(1.5), colour = "blue")) +
  theme(axis.title.x = element_text(size = 16, colour = "blue4", face = "italic")) +
  theme(axis.title.y = element_text(size = 16, colour = "blue4", face = "italic")) +
  theme(panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank())

############################################################

news3 <- readLines("news3.txt")
news3a <- removePunctuation(news3)
news3b <- removeNumbers(news3a)
rowlength3 <- length(news3b)
news3c <- paste(news3b[1:rowlength3], collapse = "")
news3d <- gsub(" ", "", news3c)

n3 <- nchar(news3d) # 計算字數

# two character

news3_result2 <- NULL

for (i in 1:n3-1) {
  news3_result2 <- c(news3_result2, substr(news3d, start = i, stop = i+1))
}

news3_result2
news3_result2table <- table(news3_result2)
news3_result2table[1:20]
news3_result2table1 <- sort(news3_result2table, decreasing = TRUE)
news3_result2table1[1:20]

news3_result2table1a <- data.frame("字詞" = names(news3_result2table1), "次數" = as.numeric(news3_result2table1))
news3_result2table1a[1:10, ]

# three character

news3_result3 <- NULL

for (i in 1:n3-1) {
  news3_result3 <- c(news3_result3, substr(news3d, start = i, stop = i+2))
}

news3_result3
news3_result3table <- table(news3_result3)
news3_result3table[1:20]
news3_result3table1 <- sort(news3_result3table, decreasing = TRUE)
news3_result3table1[1:20]

news3_result3table1a <- data.frame("字詞" = names(news3_result3table1), "次數" = as.numeric(news3_result3table1))
news3_result3table1a[1:10, ]

# four character

news3_result4 <- NULL

for (i in 1:n3-1) {
  news3_result4 <- c(news3_result4, substr(news3d, start = i, stop = i+3))
}

news3_result4
news3_result4table <- table(news3_result4)
news3_result4table[1:20]
news3_result4table1 <- sort(news3_result4table, decreasing = TRUE)
news3_result4table1[1:20]

news3_result4table1a <- data.frame("字詞" = names(news3_result4table1), "次數" = as.numeric(news3_result4table1))
news3_result4table1a[1:10, ]

# combining

news3_result2_3_4 <- rbind(news3_result2table1a[1:10, ], news3_result3table1a[1:10, ], news3_result4table1a[1:10, ])
news3_result2_3_4
news3_result2_3_4 <- mutate(news3_result2_3_4, char = NA)
news3_result2_3_4$char[1:10] <- "2"
news3_result2_3_4$char[11:20] <- "3"
news3_result2_3_4$char[21:30] <- "4"


news3_result2_3_4a <- slice(news3_result2_3_4, c(1,2,3,5,6,7,8,10,19,21,22,23,28,30))

# plot

news3_result2_3_4a %>% 
  ggplot(aes(x = reorder(news3_result2_3_4a$字詞, -news3_result2_3_4a$次數), y = news3_result2_3_4a$次數, fill = news3_result2_3_4a$char)) +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = news3_result2_3_4a$次數), vjust = 1.5, colour = "white") +
  xlab("字詞") + ylab("次數") +
  labs(fill = "字數") +
  theme(legend.position = c(0.8,0.7)) +
  theme(legend.background = element_blank()) +
  theme(legend.title = element_text(face = "italic", size = 16)) +
  theme(legend.text = element_text(face = "italic", size = 14)) +
  theme(axis.text.x = element_text(face = "italic", size = rel(1.5), colour = "black", angle = 40)) +
  theme(axis.text.y = element_text(face = "italic", size = rel(1.5), colour = "black")) +
  theme(axis.title.x = element_text(size = 18, colour = "black", face = "italic")) +
  theme(axis.title.y = element_text(size = 18, colour = "black", face = "italic"))
