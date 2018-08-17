dataMarketing=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day7\\datasetsfortodaysession\\DirectMarketing.csv")


View(dataMarketing)
str(dataMarketing)
library(dplyr)
# has relevant functions to perform linear regression.
install.packages("car")
colSums(is.na(dataMarketing))
# need to predict the amount spent with the given following data or variables
plot(dataMarketing$Age)
plot(dataMarketing$Age,dataMarketing$AmountSpent,col="red")

plot(dataMarketing$Salary)

plot(dataMarketing$AmountSpent
     ,dataMarketing$Salary,col="red")

dataMarketing$age1=ifelse(dataMarketing$Age!="Young",
                          "Middle-old",as.character(dataMarketing$Age))
dataMarketing$age1=as.factor(dataMarketing$age1)
View(dataMarketing)


plot(dataMarketing$age1,dataMarketing$AmountSpent,col="red")
#middle-old spend more than young group
plot(dataMarketing$Gender,dataMarketing$AmountSpent,col="red")
#Male spend more than female
plot(dataMarketing$Married,dataMarketing$AmountSpent,col="red")
# Married spend more than single
plot(dataMarketing$Location,dataMarketing$AmountSpent,col="red")
# Far spend more than close

plot(dataMarketing$OwnHome,dataMarketing$AmountSpent,col="red")
# own house spend more than rented

cor(dataMarketing$AmountSpent,dataMarketing$Salary)
# corealtion between two variables

dataMarketing$Children1=ifelse(dataMarketing$Children==3|dataMarketing$Children==2,
                               "3-2",as.character(dataMarketing$Children))
dataMarketing$Children1=as.factor(dataMarketing$Children1)



plot(dataMarketing$History,dataMarketing$AmountSpent,col="red")
# NA are not categorized so we need to categorize

summary(dataMarketing$History)
# using tapply
tapply(dataMarketing$AmountSpent, dataMarketing$History, mean)

# get the index of Na and store in vector
indexHistory=which(is.na(dataMarketing$History))

mean(dataMarketing[indexHistory,"AmountSpent"])

dataMarketing$History1= ifelse(is.na(dataMarketing$History),
                               "Missing",as.character(dataMarketing$History))
dataMarketing$History1=as.factor(dataMarketing$History1)


plot(dataMarketing$History1,dataMarketing$AmountSpent,col="red")
#now the missing are also categorized



data1<-dataMarketing[,-c(1,7,8)] #remove columns from which other derived columns were created
data1<-data[,-c(1,7,8)]

View(data1)
summary(data1)
str(data1)
# linear regression model for Y= Amountspent Vs all the other dependent variables
mod1<-lm(AmountSpent~.,data=data1)
# linear model
summary(mod1)

# reject the H0 - variables are insignificant
# p value which are less than 0.05 are significant

#P- values gives the variable has any effect in Y
# here the Pvalue is 0.12 for Gender so gender is not significant enough 
mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + Children1 + History1, data = data1)
summary(mod2)

# removing the insignificant variables

mod3=lm(AmountSpent~Gender,data = data1)
summary(mod3)

# create dummy variables for insignigicant variables
data1$Male_d=ifelse(data1$Gender=="Male",1,0)
data1$Female_d=ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(dataMarketing$History1=="Missing",1,0)
data1$Low_d<-ifelse(dataMarketing$History1=="Low",1,0)
data1$Med_d<-ifelse(dataMarketing$History1=="Medium",1,0)
data1$High_d<-ifelse(dataMarketing$History1=="High",1,0)



summary(data1$Low_d)
summary(dataMarketing$History1)


mod3<-lm(formula = AmountSpent ~ Male_d + 
           Location + Salary + Catalogs + 
           Children1+Med_d+Low_d , data = data1)
#Now Med_d and Low_d act as separate variables and can be interpreted as an increase in 1 for Med_d wrt to !Med_d
# And as an increase in 1 for Low_d wrt to !Low_d
summary(mod3)



mod4<-lm(formula = AmountSpent ~ Location + Salary + 
           Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod4)
summary(mod1)


#mod1 - Adjusted R-squared:  0.7421
#mod3 - male is included  Adjusted R-squared:  0.7428
#mod4 - male is excluded Adjusted R-squared:  0.7423



#check for Signs of the variable
tapply(dataMarketing$AmountSpent,dataMarketing$History1,mean)
data1%>%filter(History1!="Medium",History1!="Low")%>%
  summarize(Mean=mean(AmountSpent)) #inline

hist(mod4$residuals)
dim(dataMarketing)
qqPlot(mod4$residuals)
# non linear relation ship 
library(ggplot2)

library(car)


vif(mod4)
?vif

plot(mod4$fitted.values,mod4$residuals)
# we observe the funnel shape
# home scadaticity is missing 
#shows there is heteroskedasticity in the data

#Remedy: Apply log transform to y variable


mod5= lm(formula = log(AmountSpent)~Location + Salary + 
           Catalogs + Children1+Med_d+Low_d, data = data1)

qqPlot(mod5$residuals)
qqPlot(mod4$residuals)

plot(mod5$fitted.values,mod5$residuals)
summary(mod1)
plot(dataMarketing$Salary,dataMarketing$AmountSpent)

plot(dataMarketing$Salary,log(dataMarketing$AmountSpent))

plot(dataMarketing$Salary,dataMarketing$AmountSpent)
plot(dataMarketing$Salary,log(dataMarketing$AmountSpent))
plot(log(dataMarketing$Salary),dataMarketing$AmountSpent) 
#the curve looks exponential, therefore there is scope for some log transformation
plot(log(log(dataMarketing$Salary)),dataMarketing$AmountSpent)
plot(log(log(dataMarketing$Salary)),log(dataMarketing$AmountSpent))


mod5a<-lm(formula = log(AmountSpent) ~ Location + 
            log(log(Salary)) + Catalogs + 
            Children1+Med_d+Low_d, data = data1)
summary(mod5a)
#has highest R sqaured 
qqPlot(mod5$residuals)
qqPlot(mod5a$residuals)#qqplot looks okay
plot(mod5a$fitted.values,mod5a$residuals)# no funelling

vif(mod5a)

#Apply square root transform
mod6<-lm(formula = sqrt(AmountSpent) ~ Location + 
           Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod6)
qqPlot(mod6$residuals)
plot(mod6$fitted.values,mod6$residuals)#Seems okay, but funelling is worse 


predicted<-mod6$fitted.values
actual<-sqrt(data1$AmountSpent)

dat<-data.frame(predicted,actual)

p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))

p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="black")


plot(dat$predicted,col="blue",type="l")
lines(dat$actual,col="red",type = "l")

#MAPE error
dat$error=(abs(dat$actual-dat$predicted)/dat$actual)
mean(dat$error)
