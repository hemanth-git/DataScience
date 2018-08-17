# Trevor Stephens - 18 Jan 2014
# Titanic: Getting Started With R - Part 5: Random Forests
# Full guide available at http://trevorstephens.com/

# Set working directory and import datafiles
setwd("G:\\Day-9 & 10\\Day-9 & 10\\Random Forest\\")
titanic<-read.csv("titanic.csv")
View(titanic)
# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Convert to a string
titanic$name<-as.character(titanic$name)

str(titanic)

# Engineered variable: Title
titanic$title <- sapply(titanic$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic$title <- sub(' ', '', titanic$title)
# Combine small title groups
titanic$title[titanic$title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic$title[titanic$title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic$title[titanic$title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
titanic$title <- factor(titanic$title)

# Engineered variable: Family size
titanic$familysize <- titanic$sibsp + titanic$parch + 1

# Engineered variable: Family
titanic$surname <- sapply(titanic$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanic$familyID <- paste(as.character(titanic$familySize), titanic$surname, sep="")
titanic$familyID[titanic$familysize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(titanic$familyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
titanic$familyID[titanic$familyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
titanic$familyID <- factor(titanic$familyID)

# Fill in Age NAs
summary(titanic$age)
agefit <- rpart(age ~ pclass + sex + sibsp + parch + fare + embarked + title + familysize, 
                data=titanic[!is.na(titanic$age),], method="anova")
titanic$age[is.na(titanic$age)] <- predict(agefit, titanic[is.na(titanic$age),])
# Check what else might be missing
summary(titanic)
# Fill in Embarked blanks
summary(titanic$embarked)
which(titanic$embarked == '')
titanic$embarked[c(62,830)] = "S"
titanic$embarked <- factor(titanic$embarked)
# Fill in Fare NAs
summary(titanic$fare)
which(is.na(titanic$fare))
titanic$fare[1044] <- median(titanic$fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
titanic$familyID2 <- titanic$familyID
# Convert back to string
titanic$familyID2 <- as.character(titanic$familyID2)
titanic$familyID2[titanic$familysize <= 3] <- 'Small'
# And convert back to factor
titanic$familyID2 <- factor(titanic$familyID2)

# Split back into test and train sets
train <- titanic[1:891,]
test <- titanic[892:1309,]

# Build Random Forest Ensemble
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + 
                      fare + embarked + title + familysize + familyID2,
                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
prediction <- predict(fit, test)
prediction

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + 
                 fare + embarked + title + familysize + familyID2,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file


