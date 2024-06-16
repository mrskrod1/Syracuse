str(test)
library(readr)
train <- read_csv("Desktop/titanic dataset/train.csv")
View(train)
str(train)
train$PassengerId=factor(train$PassengerId)
str(train)

is.na(train)
is.na(train$PassengerId)
nrow(train[!complete.cases(train),])
length(which(is.na(train$PassengerId)))
length(which(is.na(train$Survived)))
length(which(is.na(train$Pclass)))
length(which(is.na(train$Name)))
length(which(is.na(train$Sex)))
length(which(is.na(train$Age)))
train_new<- train[complete.cases(train),]
nrow(train_new)
train_new2<-na.omit(train)
nrow(train_new2)
train$Fare<- as.numeric(train$Fare)
train_new2$Pclass<- as.numeric(train_new2$Pclass)
PclassFare<- train_new2[train_new2$Pclass==3, "Fare"]
train_new3<-train_new2[train_new2$Pclass ==3,]

boxplot(PclassFare)
hist(PclassFare)
cross_tab<- table(train$Embarked, train$Survived)
print(cross_tab)
summary(train_new3)

aggregate(train$Sex~ train$Fare, train, mean)

aggregate(Fare ~ Sex, data=train, FUN="mean")




