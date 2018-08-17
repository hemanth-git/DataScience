data("iris")
str(iris)
install.packages("party")
library(party)
View(iris)
iris_ctree=ctree(Species~Sepal.Length+
                   Sepal.Width+Petal.Length+Petal.Width,data = iris)
plot(iris_ctree)
plot(iris_ctree, type="simple")

#using mtcars

data("mtcars")
str(mtcars)
cars=ctree(mpg~cyl+disp+hp+drat+wt+gear+carb,data = mtcars)
plot(cars)
?mtcars
View(mtcars)

install.packages("fanc")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(fanc)
library(rattle)
library(rpart.plot)
library(rpart.plot)

#recursive partitioning
library(rpart)
names(iris)

fit=rpart(Species~Sepal.Length+
            Sepal.Width+Petal.Length+Petal.Width,
          method = "class",
          data = iris, 
          control = rpart.control(cp=0.005,minsplit = 5),
          parms = list(split="gini"))
summary(fit)
plot(fit)
printcp(fit)
?printcp

plot(fit,margin=0.01,main="Classification tree for Iris")
text(fit,use.n = TRUE, all= TRUE,cex=0.8)
