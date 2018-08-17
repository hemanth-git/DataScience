#Classification trees : 
setwd("D:\\Redo Running case study")

#-------Importing the data---------
HD<-read.csv("heart_disease_data.csv")
dim(HD)

#rpart : classification and regression trees

#----------Classification--------------#
#Some of the parameters you can use are : 

#cp : cost complexity parameter

#observations that must exist in a node in order for a split 
#to be attempted


library(rpart)
#dummy variables
HD$ca2<-ifelse(HD$ca==2,1,0)
HD$cp4<-ifelse(HD$cp==4,1,0)
HD$slope2<-ifelse(HD$slope==2,1,0)

set.seed(100)
sampling<-sort(sample(nrow(HD), nrow(HD)*.7))

length(sampling)
#Select training sample
train<-HD[sampling,]
test<-HD[-sampling,]

#method=annova ;regression models

#new one
fit<-rpart(DV ~ Sex+thalach+trestbps+
             cp4+slope2+ca2,
           method="class", 
           data=HD,
           control=rpart.control(cp=0.01, minsplit=2),
           parms=list(split="gini"))

printcp(fit)
plotcp(fit)

fit<-rpart(as.factor(DV) ~ Sex+thalach+trestbps+
             cp4+slope2+ca2,
           method="class", 
           data=HD, 
           control=rpart.control(cp=0.02, minsplit=2),
           parms=list(split="gini"))

# The complexity parameter (cp) is used to control the size of the decision tree and to select 
# the optimal tree size. If the cost of adding another variable to the decision tree 
# from the current node is above the value of cp, then tree building does not continue.
# We could also say that tree construction does not continue unless it would decrease 
# the overall lack of fit by a factor of cp. 

printcp(fit)

summary(fit)

#Plot tree 
plot(fit, margin=0.1, main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.7)

#extracting rules
asRules(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#Prune the tree using the cp value
#fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
ptree<- prune(fit,cp=0.04 )

fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")

#Predictions
test$p<-predict(fit,newdata=test,type="class")

#Confusion matrix
table(test$p,as.numeric(test$DV))

#Metrics to check 
#VAlidation techniques in R
#Plotting ROC Curve
library(ROCR)
?performance
# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions with actual values

pred<-prediction(as.numeric(test$p),as.numeric(test$DV))
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N

plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
