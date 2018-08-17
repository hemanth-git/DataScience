library(gains)
library(dplyr)
install.packages("irr")
library(irr)
library(caret)
install.packages("e1071")
library(e1071)

setwd("C:\\Jigsaw Classes\\Classes\\1. Corporate training\\Logistic Regression in R")
dm<-read.csv("DirectMarketing.csv")
# Direct Marketer who wants to come up with a process to identify good customers, identify customer id's 
#who are considered good according to his definition

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm$Target
names(dm)
dm<- dm[,-10] #removing Amount spent

summary(dm)
table(dm$Target)

#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

summary(dm$History1)
class(dm$Catalogs)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

head(dm)
dm<-dm[,-8] #Removing History
head(dm)

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(dm),0.70*nrow(dm),replace=F)
train<-dm[index,]
test<-dm[-index,]

head(train)
head(test)

dim(train)

#Build the first model using all the variables 

names(train)
mod<-glm(train$Target~.,data=train[,-9],family="binomial")
summary(mod)
summary(train$Age)
?glm

#step(mod,direction="backward") 
#step(mod,direction="both") 
#At each step it tries to see if dropping the variable decreases/increases the AIC

mod1<-glm(formula = Target ~ Age + Location 
          +Salary + Children + Catalogs +  History1, family = "binomial", data = train)
summary(mod1)

#Creating dummies

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$History1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)

mod2<-glm(Target~AgeYoung_d+Location+Salary+Children3_d+
            Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")

summary(mod2)

pred<-predict(mod2,type="response",newdata=test)
#default is log-odds and "response" returns p values
#default is log(odds). "Response" gives predicted probabilities for nrows(test) records
head(pred)
nrow(test)
nrow(train)

table(dm$Target)/nrow(dm) #39.9% are ones
pred<-ifelse(pred>=0.399,1,0)
#pred is converted to 1 and 0 for sake of confusion matrix only
#Without model if I take a random record, the probability of getting a person spending more is 39.9%
#Now, if I take individual probabilities of a model then the 39.9% gets distributed among individuals such that
#some have p than 39.9% and some have p less than 39.9%, thus keeping the overall average to 39.9%

confusionMatrix(pred,test$Target,positive="1") 
?confusionMatrix

gains(test$Target,predict(mod2,type="response",newdata=test),groups = 10)
table(test$Target)
#What % of ones in target will I catch by targeting top 10% of the customers by p value?
#First all the customers are arranged by p value and then this table is obtained

test$prob<-predict(mod2,type="response",newdata=test)
?predict


#How to get top 30% of the probabilities customers?
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
targeted<-test[test$prob>0.732602471&test$prob<=0.999747759,]
head(targeted)
