
library(dplyr)
library(gains)
library(irr)
library(caret)
library(ggplot2)
library(e1071)

dataGB=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day6\\Batch\\goodbad.csv")
View(dataGB)

str(dataGB)
colSums(is.na(dataGB))

dataGB$GoodBad=as.factor(dataGB$GoodBad)

dataGB=na.omit(dataGB)

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(dataGB),0.70*nrow(dataGB),replace=F)
train<-dataGB[index,]
test<-dataGB[-index,]

head(train)
head(test)

names(train)
mod<-glm(train$GoodBad~.,data=train[,-21],family="binomial")
summary(mod)

train$CreditHistoryA31<-ifelse(train$CreditHistory=="A31",1,0)
train$CreditHistoryA32<-ifelse(train$CreditHistory=="A32",1,0)
train$CreditHistoryA33<-ifelse(train$CreditHistory=="A33",1,0)


test$CreditHistoryA31<-ifelse(test$CreditHistory=="A31",1,0)
test$CreditHistoryA32<-ifelse(test$CreditHistory=="A32",1,0)
test$CreditHistoryA33<-ifelse(test$CreditHistory=="A33",1,0)

train$SavingsAccA62<-ifelse(train$SavingsAcc=="A62",1,0)
train$SavingsAccA63<-ifelse(train$SavingsAcc=="A63",1,0)

test$SavingsAccA62<-ifelse(test$SavingsAcc=="A62",1,0)
test$SavingsAccA63<-ifelse(test$SavingsAcc=="A63",1,0)


train$StatusA92<-ifelse(train$Status=="A92",1,0)
train$StatusA94<-ifelse(train$Status=="A94",1,0)

test$StatusA92<-ifelse(test$Status=="A92",1,0)
test$StatusA94<-ifelse(test$Status=="A94",1,0)

train$ExCredit<-ifelse(train$SavingsAcc=="A92",1,0)
train$StatusA92<-ifelse(train$SavingsAcc=="A92",1,0)
train$ExCredit=as.factor(train$ExCredit)
test$ExCredit=as.factor(test$ExCredit)

train$CurrResidTenure=as.factor(train$CurrResidTenure)
test$CurrResidTenure=as.factor(test$CurrResidTenure)
train$NumLiab=as.factor(train$NumLiab)
test$NumLiab=as.factor(test$NumLiab)

str(train)

train$JobA172<-ifelse(train$Job=="A172",1,0)
train$JobA173<-ifelse(train$Job=="A173",1,0)
train$JobA174<-ifelse(train$Job=="A174",1,0)

test$JobA172<-ifelse(test$Job=="A172",1,0)
test$JobA173<-ifelse(test$Job=="A173",1,0)
test$JobA174<-ifelse(test$Job=="A174",1,0)


test$StatusA92<-ifelse(test$SavingsAcc=="A92",1,0)
test$StatusA94<-ifelse(test$SavingsAcc=="A94",1,0)


test$NumLiab2<-ifelse(test$NumLiab=="2",1,0)
train$NumLiab2<-ifelse(train$NumLiab=="2",1,0)
test$NumLiab2=as.factor(test$NumLiab2)
train$NumLiab2=as.factor(train$NumLiab2)

train$CreditHistoryA31=as.factor(train$CreditHistoryA31)
test$CreditHistoryA31=as.factor(test$CreditHistoryA31)


train$CreditHistoryA32=as.factor(train$CreditHistoryA32)
test$CreditHistoryA32=as.factor(test$CreditHistoryA32)

train$CreditHistoryA33=as.factor(train$CreditHistoryA33)
test$CreditHistoryA33=as.factor(test$CreditHistoryA33)

train$StatusA92=as.factor(train$StatusA92)
test$StatusA92=as.factor(test$StatusA92)

train$StatusA94=as.factor(train$StatusA94)
test$StatusA94=as.factor(test$StatusA94)

train$JobA172=as.factor(train$JobA172)
test$JobA172=as.factor(test$JobA172)

train$JobA173=as.factor(train$JobA173)
test$JobA173=as.factor(test$JobA173)

train$CreditHistoryA31=as.factor(train$CreditHistoryA31)
test$CreditHistoryA31=as.factor(test$CreditHistoryA31)

train$JobA174=as.factor(train$JobA174)
test$JobA174=as.factor(test$JobA174)

train$SavingsAccA62=as.factor(train$SavingsAccA62)
test$SavingsAccA62=as.factor(test$SavingsAccA62)

train$SavingsAccA63=as.factor(train$SavingsAccA63)
test$SavingsAccA63=as.factor(test$SavingsAccA63)

train$StatusA94=as.factor(train$StatusA94)
test$StatusA94=as.factor(test$StatusA94)

train$StatusA92=as.factor(train$StatusA92)
test$StatusA92=as.factor(test$StatusA92)

test$SavingsAccA62=as.factor(test$SavingsAccA62)
test$SavingsAccA63=as.factor(test$SavingsAccA63)

str(train)
str(test)
View(train)
View(test)

mod2<-glm(formula = GoodBad ~ Check_Account_Status +Duration+Purpose+
           Amount+EmployTenure+Rate+Debtors+CurrResidTenure+
           Propert+Age+Plans+Hous+ExCredit+Tel+Foreign+
           CreditHistoryA31+CreditHistoryA32+
           StatusA92+StatusA94+JobA172+JobA173+JobA174+SavingsAccA62+
           SavingsAccA63+NumLiab2
         , family = "binomial", data = train)


names(train)
names(test)
summary(mod2)

str(train)


str(test)


pred<-predict(mod2,type="response",newdata=test)
View(pred)

test$prob<-predict(mod2,type="response",newdata=test)

quantile(test$prob,prob=c(0.25,0.50,0.75,1))


targeted<-test[test$prob>0.43196405&test$prob<=0.99,]
mean(targeted$Age)

# avg age of top 25% group is 33.45946
