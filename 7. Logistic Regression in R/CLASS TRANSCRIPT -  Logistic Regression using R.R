#-------Importing the data---------
setwd("E:\\Jaishree\\Data Science with R\\Datasets")
goodbad<-read.csv("GOODBAD.csv")

# Data Exploration

str(goodbad)
dim(goodbad)
#1=good, 0=bad

# How many good customers are there?

# Frequency Distribution of the binary dependent variable 

table(goodbad$Good.Bad)

# 700 good customers (70%) and 3000 bad customers(30%)

# To get unbiased results, the proportion should be above 10% bad customers

#Checking for missing values in all the columns

# sum() function finds total sum of is.na() for the entire dataset
# colSums() finds sum of is.na() for each column

colSums(is.na(goodbad))

# No missing values

# Partition the dataset into training and validation dataset
# From the dataset of 1000 rows, pick up a random sample of 70%
# of the rows and create a training dataset. 
# Sort the resulting vector

?sample()

# sample takes a sample of the specified size from the elements of x 
# x	can either be a vector or a positive integer.The second argument is the number of items to choose. It returns index numbers which can then be sorted

sampling<-sort(sample(nrow(goodbad), nrow(goodbad)*.7))

length(sampling)

#Row subset and create training and validation samples using the index numbers

train<-goodbad[sampling,]
test<-goodbad[-sampling,]
nrow(train)
nrow(test)

# Checking the frequency Distribution of the target variable 

#Table of y for the train dataset. To find the percentages, I am dividing by 700 and 300 respectively

table(train$Good.Bad)
table(train$Good.Bad)/700
table(test$Good.Bad)/300

#Run the Logistic Regression model

colnames(train)

?glm()
# Family of dependent variable is binary or binomial - good or bad
myresult<-glm(data=train,Good.Bad ~ Check_Account_Status +
Duration+CreditHistory+Purpose+Amount+SavingsAcc+
EmployTenure+Rate+Status+Debtors+CurrResidTenure+
Propert+Age+Plans+Hous+ExCredit+Job+NumLiab+Tel+
Foreign,family=binomial)

summary(myresult)

# In Logistic Regression, Null deviance is the variance of the 
# dependent variable explained in the absence of independent variables.
# In other words, Null Deviance gives the model Error when no predictor
# variables (IDV's ) are involved
# Residual deviance is the variance left out after the independent variables
# have explained the variance in the target variable or the variance 
# left out in the presence of independent variables.

# Null Deviance should be greater than the Residual variance for a good model

# AIC - Akaike Information Criteria - should be lower for a good model - . 
# The lesser the AIC, the better. AIC calculates the lack of fit in the model.

# Ho: All beta coefficients are zero, none of the predictors has an effect. 
# Ha: At least one beta coefficient is not zero.

# The equation would be:
# log(p/1-p) = -1.3342 + 0.7137 * Check_Account_StatusA12 + 1.4527 * Check_Account_StatusA13 + ....

# Check_Account_StatusA12 , Check_Account_StatusA13, Check_Account_StatusA14, CreditHistoryA32 and CreditHistoryA34 are significant

# Odds ratios are exp(estimates)

#To choose a good model - The step function gives best fitted model

?step
reduced<-step(myresult,direction="backward")


# Iteration 2: Retain Check_Account_Status + Duration + 
# CreditHistory + Purpose + Amount + SavingsAcc + Rate + 
# Debtors + CurrResidTenure + Plans + Hous + Foreign

myresult<-glm(data=train,Good.Bad ~ Check_Account_Status + Duration + CreditHistory + 
                Purpose + Amount + SavingsAcc + Rate + Status + Debtors + 
                Age + Plans + Foreign,family=binomial)

summary(myresult)

#Residual deviance and AIC have reduced, which is good.

# Dispersion parameter for binomial family taken to be 1 meaning
# that the predicted values are probabilities that the response 
# variable Good_Bad = 1, or the probabilities that the customer will not
# default.
# Confidence Intervals 

?confint
confint(myresult)

# How do the beta coeeficients vary between 2.5% and 97.5% confidence intervals

# For example, Duration beta coefficient  -0.027862 varies between -0.043310522 and 0.01263112

# For a robust model, this range should be as narrow as possible.


#Finding Predicted Values

?glm

myresult$fitted.values


predicted <- myresult$fitted.values
predicted
class(predicted)
str(predicted)
head(predicted)
length(predicted)

# Compare with actual data

head(train$Good.Bad)

# Let us convert the probabilities also into Good/Bad response 
# based on a cut-off probability

#Confusion Matrix
predbkt<-ifelse(predicted>0.5,'G','B')
table(predbkt,train$Good.Bad)


# At 0.5 cut-off probability:-
# 435 good customers were predicted as "good' correctly - "True Positive"
# 118 bad customers were predicted as "bad"correctly - "True Negatives"

#True Positive+ True Negative should be high. 

# Accuracy = (TP+TN)/(P+N)

# Accuracy = (443+88)/(88+123+46+443)
(441+118)/(118+92+49+441)


# For different cutoff probabilities, the confusion matrix will be different

# To find accuracies for different cut-off probabilities

# There are a lot of performance parameters available in ROCR package

install.packages("ROCR")
library(ROCR)


# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(predicted,train$Good.Bad)

?performance

perf <- performance(pred,"acc")
class(perf)
perf
# x values contain the cut-off probabilities

#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

cutoffs <- data.frame(cutoffprob, accuracies )
# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Accuracy is highest at 0.4332426

predbkt<-ifelse(predicted>0.5302152
,'G','B')
table(predbkt,train$Good.Bad)

(125+435)/(125+55+85+435)

# To find true positive rate and false positive rate
# TPR = TP/P
# FPR = FP/N

451/(451+33)
110/(110+106)

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline
# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred,"auc")
auc

# With the testing dataset

myresulttest<-glm(data=test,Good.Bad ~ Check_Account_Status + Duration + CreditHistory + 
                Purpose + Amount + SavingsAcc + Rate + Status + Debtors + 
                Age + Plans + Foreign,family=binomial)
myresulttest

predictedtest <- myresulttest$fitted.values

predbktest<-ifelse(predictedtest>0.5302152,'G','B')
table(predbktest,test$Good.Bad)
(184+51)/(51+184+26+39)





