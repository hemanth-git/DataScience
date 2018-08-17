install.packages("gains")
library(dplyr)
library(gains)
install.packages("irr")
library(irr)
install.packages("caret")
library(caret)
library(ggplot2)
install.packages("e1071")
library(e1071)


dm<-read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day8\\logistic regression\\DirectMarketing.csv")
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm$Target
names(dm)

dm<- dm[,-10]
summary(dm)
table(dm$Target)
names(dm)
View(dm)


dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)


summary(dm$History1)

class(dm$Catalogs)



dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

head(dm)

dm<-dm[,-8] #Removing History
head(dm)
set.seed(200)
?set.seed
index<-sample(nrow(dm),0.70*nrow(dm),replace=F)

View(index)
train<-dm[index,] # 70% of the data
View(train)
test<-dm[-index,] # 30% of the data

?sample

table(train$Target)
table(test$Target) # amount spent more than mean are in high in test sample
table(dm$Target)
118/182
399/601
?glm


#Build the first model using all the variables 

names(train)
mod<-glm(train$Target~.,data=train[,-9],family="binomial")
summary(mod)
?glm
step(mod,direction="backward") 
bothMod=step(mod,direction="both")
View(bothMod)
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

summary(pred)
View(pred)
  

confusionMatrix(pred,test$Target,positive="1")

gains(test$Target,predict(mod2,type="response",newdata=test),groups = 10)


quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

table(dm$Target)/nrow(dm)

?predict

pred<-ifelse(pred>=0.399,1,0)
View(test)
test$prob<-predict(mod2,type="response",newdata=test)

targeted<-test[test$prob>0.732602471&test$prob<=0.999747759,]
head(targeted)
View(targeted)



