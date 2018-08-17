salaries <- read.csv("C:\\JS\\Data Exploration and Preparation\\Salaries.csv")
class(salaries)
str(salaries)
unique(salaries$lgID)
dim(salaries)
table(salaries$lgID)
table(salaries$lgID)/nrow(salaries)

unique(salaries$teamID)
length(unique(salaries$teamID))
table(salaries$teamID)
sort(table(salaries$teamID))
sort(table(salaries$teamID), decreasing = TRUE)
plot(sort(table(salaries$teamID), decreasing = TRUE))
barchart(sort(table(salaries$teamID)))
barchart(sort(table(salaries$teamID)), main = "Count by Team", xlab = "Count", ylab = "Team Name")
barchart(sort(table(salaries$teamID)), main = "Count by Team", xlab = "Count", ylab = "Team Name", col = "orange3")
colors()

table(salaries$yearID, salaries$lgID)
table(salaries$yearID, salaries$teamID)
table(salaries$lgID, salaries$teamID)

fivenum(salaries$salary)
summary(salaries$salary)

mean(salaries$salary)

salaries_less_than_mean <- subset(salaries, salaries$salary <= mean(salaries$salary))
View(salaries_less_than_mean)
nrow(salaries_less_than_mean)

# Concept of missing values 
data("airquality")
head(airquality)
is.na(airquality$Ozone)
which(is.na(airquality$Ozone))
length(which(is.na(airquality$Ozone)))
length(which(is.na(airquality$Ozone)))/nrow(airquality)

# Count of missing value in each col
colSums(is.na(airquality))
colSums(is.na(airquality))/nrow(airquality)
mean(airquality$Ozone)
mean(airquality$Ozone, na.rm = TRUE)
mean(na.omit(airquality$Ozone))

airquality$Ozone[5] <- mean(airquality$Ozone,na.rm = TRUE)
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone,na.rm = TRUE)
colSums(is.na(airquality))

airquality1 <- na.omit(airquality)
nrow(airquality) - nrow(airquality1)
colSums(is.na(airquality1))

# ?mice
# na.roughfix (randomforest)

# Outliers 
head(iris)
boxplot(iris$Sepal.Length)
summary(iris$Sepal.Length)
iris1 <- edit(iris)
boxplot(iris1$Sepal.Length)

IQR(iris1$Sepal.Length) 
summary(iris1$Sepal.Length)

# Any obs > 3rd quartile value + 1.5*IQR
# Any obs < 1st quartile value - 1.5*IQR
6.4 + 1.5*1.3
5.100 - 1.5*1.3

library(data.table)
salaries

salaries_DT <- as.data.table(salaries)
class(salaries_DT)
salaries_DT
salaries_DT[1,]
salaries_DT[ ,1]
salaries_DT[, yearID]
salaries_DT[, list(yearID, salary)]
salaries_DT[ yearID > 2000, ]
sal_x <- salaries_DT[ lgID == "AL" & yearID == 1990, ]
sal_x
salaries_DT[order(salary), ]

summarize.year <- salaries_DT[ , mean(salary), by = "yearID"]
summarize.year

summarize.year <- salaries_DT[ , list(Avergae = mean(salary)), by = "yearID"]
summarize.year

summarize.year <- salaries_DT[ , list(Avergae = mean(salary), Maximum = max(salary)), by = "yearID"]
summarize.year

summarize.lgID <- salaries_DT[ , list(Avergae = mean(salary), Maximum = max(salary)), by = "lgID"]
summarize.lgID

summarize.lgID.yearID <- salaries_DT[ , list(Avergae = mean(salary), Maximum = max(salary)), by = c("yearID,lgID")]
summarize.lgID.yearID

library(ggplot2)
ggplot(salaries_DT, aes(yearID, salary)) + geom_point()
ggplot(summarize.year, aes(yearID,Avergae )) + geom_point()
ggplot(summarize.year, aes(yearID,Avergae )) + geom_line()
ggplot(summarize.year, aes(yearID,Maximum )) + geom_line()

ggplot(summarize.lgID.yearID, aes(yearID, Avergae, col = lgID)) + geom_line()

# Correlation 
head(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)

plot(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length)

cor(iris[ , 1:4])  # Correlation matrix 

library(corrgram)
corrgram(iris[ , 1:4])
