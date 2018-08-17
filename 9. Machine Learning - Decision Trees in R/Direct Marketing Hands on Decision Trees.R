data=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day10\\DirectMarketing.csv")
View(data)
dm=data
library(dplyr)
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
View(dm)
names(dm)
colSums(is.na(dm))
dm$History1=ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1=as.factor(dm$History1)
dm$Target=as.factor(dm$Target)
names(dm)
str(dm)

library(party)
#create a decision tree with Target(Categorical) as target variable 
dmTree=ctree(Target~Age+Gender+OwnHome+Married+
               Location+Salary+Children+History1, data = dm)
plot(dmTree)
plot(dmTree,type="simple")

#Amount spent is categorized
#1. History is high and location is either far or close. Probablity is 0.95
#2. History is missing and salary>81400 Probablity is .98
#3. business need not target whose salary is <32000
#4. business need not target whose history is low/medium and salary >59000



#create a decision tree with Target(Categorical) as target variable 

dcTree=ctree(AmountSpent~Age+Gender+OwnHome+Married+
               Location+Salary+Children+History1, data = dm)
plot(dcTree,type="simple")

plot(dcTree)
#Amount spent is continous
#1. business need not consider whose location is far and salary < 21600
#2. mean amount spent is 1900 when history is high and salary >40000
#3. mean amount spent is 4000 when location is far and salary >1lakh 
#and history is high/missing
#4. mean amount spent is 2500 when location is close and salary >1.1 lakh 
#and history is high/missing
#5. mean amount spent is 2000 when 1.1lakh< salary >68000
# history is high/missing people who fall in this category are highest.




