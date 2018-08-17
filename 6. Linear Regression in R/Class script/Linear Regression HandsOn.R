mmix=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day7\\Class script\\Linear Regression class files\\MMix.csv")
View(mmix)
View(data1)
str(mmix)
mmix$Facebook=as.factor(mmix$Facebook)
mmix$Twitter=as.factor(mmix$Twitter)
mmix$WebCamp=as.factor(mmix$WebCamp)
mmix$Online=as.factor(mmix$Online)
mmix$Inserts=as.factor(mmix$Inserts)

names(mmix)
data1=mmix[,-c(10,12)]
View(data1)
library(car)
library(dplyr)
mod1= lm(NewVolSales~.,data = data1)
summary(mod1)

data1%>%mutate(Bin_base_price=ntile(data1$Base.Price,5))->data2
View(data2)

names(data2)
data3=data2[,-9]
names(data3)
mod2=lm( formula = NewVolSales~ Radio+TV+InStore+Facebook+Twitter+
          WebCamp+Inserts+Bin_base_price, data=data3)
#R-square is 70
mod3=lm( formula = NewVolSales~ Radio,data=data3)
summary(mod3)

norNewVol=(data3$NewVolSales - mean(data3$NewVolSales))/sd(data3$NewVolSales)
View(norNewVol)
data3=cbind(data3,norNewVol)

mod4=lm( formula = norNewVol~ Radio+TV+InStore+Facebook+Twitter+
           WebCamp+Inserts+Bin_base_price, data=data3)
View(data3)

summary(mod4)

mod5=lm( formula = norNewVol~ Radio+Bin_base_price+TV+InStore, data=data3)
summary(mod5)



qqPlot(mod1$residuals)

library(ggplot2)

library(car)


plot(mod1$fitted.values,mod1$residuals)

plot(log(log(data1$Radio)),log(data1$NewVolSales))

View(data1)
colSums(is.na(data1))


