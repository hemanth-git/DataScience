setwd("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day6\\insurance")


## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("insurance.csv", 
                      stringsAsFactors = TRUE)
str(insurance)
summary(insurance)
summary(insurance$age)

library(dplyr)
library(ggplot2)
attach(insurance)
table(age)
plot(type="l",table(age))
table(gender,region,smoker)

table(smoker,children)

x=boxplot(age)
y=boxplot(charges)
str(x)
str(y)
listOutliers=x$out
listOutliers


list<-x$out

#gives the positions in the data where outliers are present
index<-which(insurance$age %in% list)
index

insurance$age[index]
mean_sw=mean(insurance$age,na.rm = TRUE)
mean_sw
insurance$age[index]=mean_sw

z=boxplot(bmi)
y=boxplot(charges)

#Check for missing values
colSums(is.na(insurance))
insuranceNew<-na.omit(insurance) #omit observations
dim(insurance)
dim(insuranceNew)

insurance$bmi[is.na(insurance$bmi)]=mean(insurance$bmi,na.rm = TRUE)
dim(insurance)
dim(insuranceNew)

M<-c(1,2,NA,3,4,5)
mean(M)
mean(M,na.rm=FALSE)
mean(insuranceNew$bmi)
mean(insurance$bmi)

install.packages("randomForest")
library(randomForest)

insurance$bmi<-na.roughfix(insurance$bmi)


#Data Aggregations

#group by multiple values 
aggregate(charges ~ gender , data=insurance, FUN=mean) #What inference?
aggregate(charges ~ smoker , data=insurance, FUN=mean) #What inference?
aggregate(charges ~ region , data=insurance, FUN=mean) #What inference?

aggregate(charges ~ region + smoker + gender, data=insurance, FUN=mean)#What inference?


hist(insurance$charges)
library(ggplot2)

ggplot(insurance,aes(x=charges))+
  geom_histogram(aes(fill=as.factor(smoker)))

ggplot(insurance,aes(x=charges))+
  geom_histogram(aes(fill=as.factor(smoker)),position = "dodge")+facet_grid()


#box plots : 
ggplot(insurance,aes(y=charges,x=gender,fill=as.factor(gender)))+ 
  geom_boxplot()+facet_grid(children~gender)

ggplot(insurance,aes(y=charges,x=gender,fill=as.factor(gender)))+ 
  geom_boxplot() +facet_grid(region~smoker)


#bmi vs charges
p<-ggplot(insurance,aes(x=bmi,y=charges,color=as.factor(gender)))
p+geom_point()
p+geom_point()+facet_grid(.~smoker)
p+geom_point()+facet_grid(smoker~region)

#increasing age vs increasing charges, increasing charges vs smoker=yes
p<-ggplot(insurance,aes(x=age,y=charges,color=as.factor(smoker)))
p+geom_point()

p+geom_point()+facet_grid(.~gender)
p+geom_point()+facet_grid(smoker~region)
cor(insurance[c("age", "bmi", "children", "charges")])


pairs(insurance[c("age", "bmi", "children", "charges")])



# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
p<-ggplot(insurance,aes(x=age,y=charges,color=as.factor(smoker)))
p+geom_point()

p+geom_point()+facet_grid(gender~bmi30)

##----------------------------------------Dummy variable creation-----------------------------##
insurance$dummy_r_ne<-ifelse(insurance$region=="northeast",1,0) #Using ifelse

