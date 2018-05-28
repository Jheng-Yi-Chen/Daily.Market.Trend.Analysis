
library(foreign)
library(readstata13)

es1 <- read.dta("ES-Indicators-Database-.dta")
qog1 <- read.dta("qog_std_ts_jan17.dta", convert.factors = FALSE)
# tfp <- read.dta("Firm Level TFP Estimates and Factor Ratios_February_21_2017.dta")

####################################################################################################

qog1$cy <- qog1$ccode * qog1$year
es1$cy <- es1$ccode * es1$year

mr1 <- merge(x = es1, y = qog1, by = "cy")

####################################################################################################

library(lattice)
library(ggplot2)
library(plyr)
library(dplyr)

table(mr1$car4)

mr1soe <- mr1 %>% filter(!is.na(soe))
# mr1 %>% select(c(1,2,3)) -> test1

# na.fail(mr1$soe)
# is.null(mr1$soe) ; is.na(mr1$soe) ; any(is.na(mr1$soe))
# mr1$soe1 <- na.exclude(mr1$soe)
# mr1$soe1 <- mr1[na.exclude(mr1$soe), ]
# table(mr1$soe1)
# summary(mr1$soe1)

(count_soe <- ggplot(mr1soe, aes(x = mr1soe$soe)) + 
  geom_bar() + 
  xlab("Numbers of Soes and Non Soes") +
  ylab("Count"))
ggsave("count_soe.png")

####################################################################################################

es2 <- mr1[ which(mr1$car4 >= 1), ]

####################################################################################################

# es2$size1 <- NA
# es2$size1[es2$size == "small(<20)" ] <- 1
# es2$size1[es2$size == "medium(20-99)" ] <- 2
# es2$size1[es2$size == "large(100 and over)" ] <- 3
# table(es2$size1) # company size, 1 is small, 2 is medium, 3 is large

table(es2$size)
# es2$size1 = es2$size[!is.na(es2$size),]

es2 %>% filter(!is.na(size)) -> es2size

(company_size <- ggplot(es2size, aes(x = size)) + 
  geom_bar() + 
  xlab("Compnay Size") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)))
ggsave("company_size.png")

(tab1 <- table(es2$size, es2$exporter))

# si <- ddply(es2, "size", transform, Percent_Size = as.numeric(size) / sum(as.numeric(size)) * 100)
# ggplot(si, aes(x = size, y = Percent_Size, fill = exporter)) + geom_bar(stat = "identity")

# comapnysize <- ggplot(es2, aes(es2$size)) +
#   geom_bar(aes(color = es2$size)) +
#   coord_flip() +
#   scale_fill_manual(values = c("es2$size")) +
#   labs(x = "company size")
# ggsave("company_size.png")

####################################################################################################

es2 %>% filter(!is.na(car1)) -> es2age

summary(es2age$car1)

comapny_age <- ggplot(es2age, aes(car1)) +
  geom_bar(aes(color = car1)) +
  coord_flip() +
  scale_fill_manual(values = c("car1")) +
  labs(x = "Company Age", y = "Count")
comapny_age
ggsave("company_age.png")

####################################################################################################

table(es2$exporter)

expper <- qplot(es2$perf1, es2$car6, data = es2, colour = factor(es2$exporter)) + 
  scale_colour_manual(values = c("red","green"))

####################################################################################################

mr1 %>%
  filter(!is.na(soe)) -> mr1soe
  filter(!is.na(size)) -> es2size
es2 %>% filter(!is.na(car1)) -> es2age
es2 %>% filter(!is.na(exporter)) -> es2exporter
es2 %>% filter(!is.na(perf1)) -> es2perf1
es2 %>% filter(!is.na(perf2)) -> es2perf2
es2 %>% filter(!is.na(obst1)) -> es2obst1
es2 %>% filter(!is.na(obst2)) -> es2obst2
es2 %>% filter(!is.na(obst3)) -> es2obst3
es2 %>% filter(!is.na(obst4)) -> es2obst4
es2 %>% filter(!is.na(obst5)) -> es2obst5
es2 %>% filter(!is.na(obst6)) -> es2obst6

summarise(es2obst15, mean = mean(as.numeric(es2obst15)), min = min(as.numeric(es2obst15)), max = max(as.numeric(es2obst15)))

####################################################################################################

es2manufacturing <- es2[ which(es2$stra_sector == "Manufacturing"), ]

es3 <- mr1[ which(mr1$region == "EAP"), ]

write.csv(es2, file = "es2.csv")
write.dta(es2, file = "es2.dta")
write.dta(mr1, file = "mr1.dta")

####################################################################################################

table(es2$idstd)
table(es2$year)
table(es2$country2)
table(es2$ownership)
table(es2$secfpct1)
table(es2$car1)
table(es2$car2)
table(es2$car3)
table(es2$car4)
table(es2$car6)

####################################################################################################

table(es2$stra_sector)

es2 %>% filter(!is.na(stra_sector)) -> es2sector

sector_amount <- as.data.frame(table(es2sector$stra_sector))
colnames(sector_amount) <- c("Sector", "Count")
sector_amount5 <- sector_amount[ which(sector_amount$Count > 9), ]
ggplot(sector_amount, aes(x = Count, y = Sector)) + geom_point()
company_sector <- ggplot(sector_amount5, aes(x = Count, y = Sector)) + geom_point()
ggsave("company_sector.png")

####################################################################################################

summary(table(es2$perf1))
summary(table(es2$perf2))

####################################################################################################

t(as.matrix(table(es2$obst1)))
t(as.matrix(table(es2$obst2)))
t(as.matrix(table(es2$obst3)))
t(as.matrix(table(es2$obst4)))
t(as.matrix(table(es2$obst5)))
t(as.matrix(table(es2$obst6)))

obsttable <- rbind(t(as.matrix(table(es2$obst1))), t(as.matrix(table(es2$obst2))), t(as.matrix(table(es2$obst3))),
                   t(as.matrix(table(es2$obst4))), t(as.matrix(table(es2$obst5))), t(as.matrix(table(es2$obst6))),
                   t(as.matrix(table(es2$obst7))), t(as.matrix(table(es2$obst8))), t(as.matrix(table(es2$obst9))),
                   t(as.matrix(table(es2$obst10))), t(as.matrix(table(es2$obst11))), t(as.matrix(table(es2$obst12))),
                   t(as.matrix(table(es2$obst13))), t(as.matrix(table(es2$obst14))), t(as.matrix(table(es2$obst15))))

colnames(obsttable) <- c("NO", "YES")
rownames(obsttable) <- c("access to finance", "access to land", "business licensing and permits", "corruption", "courts", "crime, theft and disorder", "customs and trade regulations", "electricity", "inadequately educated workforce", "labor regulations", "political instability", "practices of competitors in the informal sector", "tax administration", "tax rates", "transport")
write.csv(obsttable, file = "obsttable.csv")

####################################################################################################

table(es2$obst1)
table(es2$obst2)
table(es2$obst3)
table(es2$obst4)
table(es2$obst5)
table(es2$obst6)
table(es2$obst7)

####################################################################################################

table(es2$fin2)
table(es2$stra_sector)
table(es2$fh_rol)
es2$fh_rol2 <- es2$fh_rol + 1
table(es2$fh_rol2)
table(es2$pwt_hci)
table(es2$dpi_system)
table(es2$fh_polity2)
table(es2$ht_regtype)
table(es2$p_polity)
table(es2$p_polity2)

summary(es2$pwt_hci)
summary(es2$fh_rol2)
summary(es2$p_polity2)

t(as.matrix(summary(es2$pwt_hci)))
t(as.matrix(summary(es2$fh_rol2)))
t(as.matrix(summary(es2$p_polity2)))
statetable <- rbind(t(as.matrix(summary(es2$pwt_hci))), t(as.matrix(summary(es2$fh_rol2))), t(as.matrix(summary(es2$p_polity2))))
colnames(statetable) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's")
rownames(statetable) <- c("Human Capital", "Rule of Law", "Polity")
write.csv(statetable, file = "statetable.csv")

####################################################################################################

(table1 <- table(es2$stra_sector, es2$obst8))
boxplot(es2$perf1 ~ es2$country2)
qplot(factor(es2$size1))

boxplot(es3$perf1 ~ es3$country2)
barchart(es3$country2 ~ es3$perf1, data = es3)
plot(es3$perf1, es3$car6)
xyplot(es3$perf1 ~ es3$car6 | es3$country2, data = es3)

(perff1 <- ggplot(es3, aes(x = car6, y = perf1)) + geom_point(aes(color = country2)))
(perff2 <- ggplot(es3, aes(x = country2, y = perf1)) + geom_boxplot())

####################################################################################################

ggplot(es2, aes(x=es2$obst1)) + geom_bar()
ggplot(es2, aes(x = factor(es2$obst1), y=es2$perf1)) + geom_boxplot()

ggplot(es$soe, aes(x=factor(es$soe), y=es$per)) +  
  geom_boxplot() +
  scale_fill_manual(values = c("yellow", "orange")) + 
  ggtitle("SOE and Performance") + 
  theme(axis.text.x = element_text(angle = 90, face = "bold", colour = "black"))

mosaicplot( ~ obst1 + car1, data = es2, main = "Survival on The Titanic", color = T)

####################################################################################################

library(lme4)

model1 <- lmer(perf1 ~ exporter + size + car6 + factor(obst1) + factor(obst11) + (1|country2), data = es2)
summary(model1)
print(model1)

model4 <- lmer(perf1 ~ stra_sector + exporter + size + factor(obst1) + factor(obst11) + (1|country2), data = es2)
summary(model4)
print(model4)

model5 <- lmer(perf1 ~ exporter + exporter:fh_rol2 + exporter:pwt_hci + fh_rol2 + pwt_hci + (1|country2), data = es2)
summary(model5)
print(model5)
coef(model5)

####################################################################################################

summary(es2$exporter)
summary(es2$size)

summary(es2$fh_rol2); length(es2$fh_rol2)
summary(es2$pwt_hci); length(es2$pwt_hci)

table(es2$obst1)
table(es2$obst11)
table(es2$country2)
