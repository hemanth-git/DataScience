setwd("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day11\\R code Class")
goodbad=read.csv("goodbad.csv")
colSums(is.na(goodbad))
# imputing the age
index=which(is.na(goodbad$Age))
goodbad$Age[index]=mean(goodbad$Age,na.rm = TRUE)

str(goodbad)
goodbad$GoodBad=as.factor(goodbad$GoodBad)
summary(goodbad)
nrow(goodbad)

train <- goodbad[1:700,]
test <- goodbad[701:1000,]


library(randomForest)
set.seed(415)
names(train)
frst=randomForest(GoodBad~.,
                  data=train, importance=TRUE, ntree=2000)

var1=data.frame(varImpPlot(frst))
View(var1)
library(dplyr)




prediction <- predict(frst, test)
test$predict=prediction
library(caret)
View(test)
confusionMatrix(prediction,test$GoodBad)

logisticModel1=glm(GoodBad~Check_Account_Status+Duration+CreditHistory+
                     Purpose+Amount+SavingsAcc+EmployTenure+Rate+Status+
                     Debtors+CurrResidTenure+Propert+Age+Plans,
                   data=train,family = "binomial")

summary(logisticModel1)

pred<-predict(logisticModel1,type="response",newdata=test)
test$preditlogist=ifelse(pred>0.5,1,2)
plot(pred)

test$preditlogist=as.factor(test$preditlogist)
table(test$GoodBad)

confusionMatrix(test$GoodBad,test$preditlogist)



