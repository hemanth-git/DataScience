setwd("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day6\\goodbad handson\\")


## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
goodbaddata <- read.csv("goodbad.csv", 
                      stringsAsFactors = TRUE)
str(goodbaddata)
summary(insurance)
summary(insurance$age)

colSums(is.na(goodbaddata))


goodbaddata$Age[is.na(goodbaddata$Age)]=mean(goodbaddata$Age,na.rm = TRUE)
library(dplyr)
library(ggplot2)

#Credit history Vs good or bad customer
#crVsgood
attach(goodbaddata)
p<-ggplot(goodbaddata,aes(CreditHistory))
p+geom_point()

table(CreditHistory,GoodBad)
p<-ggplot(goodbaddata,aes(x=GoodBad,y=CreditHistory))+
  geom_histogram()

aggregate(Amount ~ GoodBad+CreditHistory, data = goodbaddata, FUN = mean)
out=boxplot(Age)

#Age of 20 - 40 are more likely to default
ggplot(goodbaddata,aes(x=Age))+
  geom_histogram(aes(fill=as.factor(GoodBad)),position="dodge")

ggplot(goodbaddata,aes())+
  geom_histogram(aes(fill=as.factor(GoodBad)),position="dodge")
# property Vs credit history Vs Duration
aggregate(Duration ~ Propert+CreditHistory, data = goodbaddata, FUN = mean)
# 4 foreign worker with bad customers
table(Foreign,GoodBad)
goodbaddata%>%filter(Foreign=="A202"& GoodBad=="2")%>%
  select(Foreign,GoodBad,CreditHistory,Amount)
#A32 : existing credits paid back duly till now
#A34 : critical account/ other credits existing (not at this bank)
#A30 : no credits taken/all credits paid back duly
# foreign worker with bad credit history and bad customers. Credit amount likely to default
#$18,000

goodbaddata%>%filter(CreditHistory=="A33"|CreditHistory=="A34" &GoodBad=="2")%>%
  group_by(CreditHistory)%>%summarise(sum(Amount))
# customers having bad credit history and and bad total credit amount avg(57,000)


goodbaddata%>%filter(Propert=="A124"&CreditHistory=="A34" &GoodBad=="2")%>%summarise(sum(Amount))
# $60386 is likely to be defaulted with no property and bad credit history