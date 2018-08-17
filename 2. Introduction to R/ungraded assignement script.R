dataset(lakers)
library('lubridate')
datasets::lakers
?`datasets-package`
data("lakers")
View(lakers)
sample2=data.frame(lakers)
library(dplyr)
sample2%>%mutate(Day=weekdays.Date(as.Date(as.character(sample2[,"date"]),format = '%Y%m%d')))%>%
  filter(player=="Pau Gasol" & opponent=="POR" & Day=="Sunday")%>%summarise(n())

#sample2$player=="Pau Gasol" & sample2$opponent=="POR" 
sample2%>%mutate(Day=as.Date.numeric(sample2$date))->sample3
as.Date.numeric(sample2$date)
weekdays.Date(sample2$date)
weekdays.Date(as.Date(as.character(sample2[,"date"]),format = '%Y%m%d'))
sample2[,"date"]
str(sample2)
sample2%>%mutate(Day=weekdays.Date(as.Date(as.character(sample2[,"date"]),format = '%Y%m%d')))%>%
  filter(game_type=="home" & opponent=="PHX" & Day=="Wednesday")%>%summarise(n())


#dataset AdultUCI from arules package. 
#Load the inbuilt dataset AdultUCI from arules package. 
#How many females are there in t
#he data set whose age is less than 50 and who are black?

library(arules)
data("AdultUCI")
sample4=data.frame(AdultUCI)
str(sample4)
sample4%>%filter(sex=="Female" & age<50 & race=="Black")%>%summarise(n())

#Use the lakers data set in the package lubridate. 
#How many times observations corresponding to the months of 
#October, December, January and April, appear


sample2%>%
  mutate(mon=months.Date(as.Date(as.character(sample2[,"date"]),format = '%Y%m%d')))%>%
  group_by(mon)%>%filter(mon=="October"|mon=="December"|mon=="January"|mon=="April")%>%
  summarise(n())

#Use the lakers data set in the package lubridate. 
#In this data set how many rows correspond to instances 
#where the Player was Pau Gasol and the opposition was POR?  
sample2%>%filter(player=="Pau Gasol" & opponent=="POR" )%>%summarise(n())


#Use the lakers data set in the package lubridate. 
#In this data how many rows correspond to instances 
#where the day was Monday and time 12?
str(sample2)
sample2%>%mutate(Day=weekdays.Date(as.Date(as.character(sample2[,"date"]),format = '%Y%m%d')))%>%
  filter(time=="12:00" & Day=="Monday")%>%summarise(n())



#Using the data set AdultUCI from the package arules() 
#find out the mean age corresponding to all categories 
#in the column "income" for all white females who work less than 25 hours a week. 
#(Consider the missing values in the column "income" also as a separate group)

sample4%>%filter(hours.per.week<25 & race =="White" & sex=="Female")%>%
  group_by(income)%>%
  summarise(mean(age))

#Use the inbuilt mtcars data set, 
#what is the average "mpg" for each category 
#of car defined based on "number of gears".

data(mtcars)
sample5=  data.frame(mtcars)          
sample5%>%group_by(gear)%>%summarise(mean(mpg))

#Use the inbult iris dataset, 
#what is mean Sepal.Length for species setosa?
data(iris)
sample6=data.frame(iris)
str(sample6)
sample6%>%filter(Species=="setosa")%>%summarise(mean(Sepal.Length))
