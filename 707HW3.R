library(readr)
> bankdata_csv_all <- read_csv("Desktop/bankdata_csv_all.csv")

#Check data dimensions 
dim(bankdata_csv_all)

#Identify Problems
str(bankdata_csv_all)

#Check for missing dT in the dataset
colSums(is.na(bankdata_csv_all))

#age data
summary(bankdata_csv_all$age)

#income data
summary(bankdata_csv_all$income)

#age histogram
library(ggplot2)
library(tidyverse)
hist(bankdata_csv_all$age)
ageHist<- bankdata_csv_all %>%
  ggplot(aes(x =age)) +
  geom_histogram(bins = 6, fill=blues9, col="white")



#Income histogram
hist(bankdata_csv_all$income)
incomeHist<- bankdata_csv_all %>% ggplot(aes(income)) +
  geom_histogram()

#remove id field
bankdata_csv_all$id<- NULL

#convert char features to factor
bankdata_csv_all$sex<- factor(bankdata_csv_all$sex)
bankdata_csv_all$region<- factor(bankdata_csv_all$region)
bankdata_csv_all$married<- factor(bankdata_csv_all$married)
bankdata_csv_all$car <- factor(bankdata_csv_all$car)
bankdata_csv_all$save_act<- factor(bankdata_csv_all$save_act)
bankdata_csv_all$current_act<- factor(bankdata_csv_all$current_act)
bankdata_csv_all$mortgage<- factor(bankdata_csv_all$mortgage)
bankdata_csv_all$pep<- factor(bankdata_csv_all$pep)


#convert children to ordinal factor
bankdata_csv_all$children<- ordered(bankdata_csv_all$children)

#discretize age and income
bankdata_csv_all$age <- cut(bankdata_csv_all$age, breaks = c(0,20,30,40,50,60,100),
                            labels = c("teens", "twenties", "thirties", "forties", "fifties", "sixties"),
                            right = FALSE)

bankdata_csv_all$income <- cut(bankdata_csv_all$income, breaks = c(0,15000,25000,35000,45000,100000),
                               labels = c("0-14999", "15000-24999", "25000-34999", "35000-44999",
                                         "45000 +" ), right = FALSE)

table(bankdata_csv_all$age)

table(bankdata_csv_all$income)

str(bankdata_csv_all)

tid<-as.character(bankdata_csv_all[["id"]])
transactions<- as(bankdata_csv_all, "transactions")
transactionInfo(transactions) [["transactionID"]]
itemFrequencyPlot(transactions, topN=20, type="absolute")

rules_pep <- apriori(transactions, parameter = list(supp = 0.002, conf = 0.5))
rules_pep <- sort(rules_pep, decreasing = TRUE, by="lift")

inspect(rules_pep[1:5])

rules_pep <- apriori(transactions, parameter = list(supp = 0.023, conf = 0.75))
rules_pep <- sort(rules_pep, decreasing = TRUE, by="lift")

inspect(rules_pep[1:5])
agebar<- bankdata_csv_all %>%
  ggplot(aes(x =age)) +
  geom_bar()
