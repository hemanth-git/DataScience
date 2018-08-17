setwd("C:\\Users\\hemanth\\Downloads\\Kaggle\\Titanic")
data=read.csv("train.csv")
install.packages('party')

library(party)
library(randomForest)
View(data)
data$Name=as.character(data$Name)
str(data)
summary(data)
colSums(is.na(data))
# Engineered variable: title


funsplit=function(x){
  strsplit(x,'[,.]')[[1]][2]
  }
data$title<-sapply(data$Name,FUN = funsplit)
######
#demo strsplit
var1="Braund, Mr. Owen Harris"
strsplit(var1,"[,.]")[[1]][2]


# combine small little groups
table(data$title)
data$title=sub(' ','',data$title)
View(data)
data$title[data$title %in% c('Mme','Mlle')]<-'Mlle'
data$title[data$title %in% c('Capt','Don','Major','sir')]<-'Sir'
data$title[data$title %in% c('Dona','Lady','the Countess','Jonkheer')]<-'Lady'
colSums(is.na(data))
table(data$title)
data$title=as.factor(data$title)

data$familysize=data$SibSp+data$Parch+1

# engineeried name : Family
data$surname=sapply(data$Name,FUN = function(x){
  strsplit(x,split = '[,.]')[[1]][1]
})

data$familyID=paste(as.character(data$familysize),data$surname,sep = "")

data$familyID[data$familysize<=2]='small'
famIDs=data.frame(table(data$familyID))
famIDs=famIDs[famIDs$Freq<=2,]
data$familyID[data$familyID %in% famIDs$Var1]='small'
data$familyID=factor(data$familyID)

library(rpart)
# imputing the Age 
names(data)
agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + familysize, 
                data=data[!is.na(data$Age),], method="anova")
colSums(is.na(data))
summary(agefit)
data$Age[is.na(data$Age)]= predict(agefit,data[is.na(data$Age),])

summary(data)
# Fill in Embarked blanks
summary(data$Embarked)
which(data$Embarked == '')
data$Embarked[c(62,830)] = "S"
data$Embarked <- factor(data$Embarked)


summary(data$Fare)
which(is.na(data$Fare))
data$fare[c(1226,1310)] <- median(data$Fare, na.rm=TRUE)


# New factor for Random Forests, only allowed <32 levels, so reduce number
data$familyID2 <- data$familyID
data$familyID2 <- as.character(data$familyID2)
data$familyID2[data$familysize <= 3] <- 'Small'
data$familyID2 <- factor(data$familyID2)
View(data)
colSums(is.na(data))

nrow(data)


# Split back into test and train sets
train <- data[1:891,]
test <- data[892:1309,]

library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + 
                      Fare + Embarked + title + familysize ,
                    data=data, importance=TRUE, ntree=2000)
varImpPlot(fit)
prediction <- predict(fit, data1)
View(prediction)


##############################
# data preparation for test data set
data1=read.csv("test.csv")
View(data1)
data1$Name=as.character(data1$Name)
str(data1)
summary(data1)
colSums(is.na(data1))
# Engineered variable: title


funsplit=function(x){
  strsplit(x,'[,.]')[[1]][2]
}
data1$title<-sapply(data1$Name,FUN = funsplit)

# combine small little groups
table(data1$familyID2)
table(data$familyID2)
data1$title=sub(' ','',data1$title)
View(data1)
data1$title[data1$title %in% c('Mme','Mlle')]<-'Mlle'
data1$title[data1$title %in% c('Capt','Don','Major','sir')]<-'Sir'
data1$title[data1$title %in% c('Dona','Lady','the Countess','Jonkheer')]<-'Lady'
colSums(is.na(data1))
table(data1$title)
data1$title=as.factor(data1$title)

data1$familysize=data1$SibSp+data1$Parch+1

# engineeried name : Family
data1$surname=sapply(data1$Name,FUN = function(x){
  strsplit(x,split = '[,.]')[[1]][1]
})

data1$familyID=paste(as.character(data1$familysize),data1$surname,sep = "")

data1$familyID[data1$familysize<=2]='small'
famIDs=data.frame(table(data1$familyID))
famIDs=famIDs[famIDs$Freq<=2,]
data1$familyID[data1$familyID %in% famIDs$Var1]='small'
data1$familyID=factor(data1$familyID)

data1$familyID2 <- data1$familyID
data1$familyID2 <- as.character(data1$familyID2)
data1$familyID2[data1$familysize <= 3] <- 'Small'
data1$familyID2 <- factor(data1$familyID2)

which(is.na(data1$Fare))
data1$Fare[153]=mean(data1$Fare,na.rm = TRUE)
sapply(data, class)
sapply(data1, class)
pre <- predict(fit, data1)
View(pre)
