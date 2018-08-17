data= read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day9\\datasetsfortodaysession\\song_football.csv")
View(data)
colSums(is.na(data))

sample<-data[,2:8]
str(sample)
nrow(sample)
scaled_data<-data.frame(rownum<-1:480)
View(scaled_data)
list=names(sample)

for(i in 1:length(list))
{
  
  x<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}

head(scaled_data)

scaled_data<-scaled_data[,-1]
sample<-cbind(sample,scaled_data)
names(sample)


set.seed(200)
names(sample)
fit.km<-kmeans(sample[,8:14],160)
# clustering 3 player per group can be easy way - 480/3 = 160


data$cluster160=fit.km$cluster
clusterfound=data$cluster160[which(data$First_Name=="Song")]
library(dplyr)
data%>%filter(data$cluster160==clusterfound)->similar3players
####################################################################

fit.km1<-kmeans(sample[,8:14],240)
# clustering 3 player per group can be easy way - 480/2 = 240


data$cluster240=fit.km1$cluster
clusterfound=data$cluster240[which(data$First_Name=="Song")]
View(clusterfound)
library(dplyr)
data%>%filter(data$cluster240==clusterfound)->similar2players

### clustering to 4 groups and then re-run the k-means in the group
data1=cbind(data,scaled_data)
names(data1)
fit.km4<-kmeans(data1[,11:17],4)

data1$cluster4=fit.km4$cluster

clusterfound=data1$cluster4[which(data1$First_Name=="Song")]

data1%>%filter(data1$cluster4==clusterfound)->similar4groups
nrow(similar4groups)
View(similar4groups)
names(similar53groups)
fit.km.similar53<-kmeans(similar4groups[,11:17],53)
View(similar4groups)
similar4groups$cluster53=fit.km.similar53$cluster

clusterfound=similar4groups$cluster53[which(similar4groups$First_Name=="Song")]
similar4groups%>%filter(similar4groups$cluster53==clusterfound)->similar53groups
View(similar53groups)
similar53groups[,c(9,10)]

######################################################################
# using the hierarchial model

View(data)
library(cluster)
samH=data[,2:8]
View(samH)
dmatrix<-daisy(samH,metric="euclidean",stand=TRUE)
summary(dmatrix)


distmatrix<-dist(dmatrix)
str(distmatrix)
d<-as.matrix(distmatrix)

disclust<-hclust(distmatrix,method="average")
str(disclust)

plot(as.dendrogram(disclust))
rect.hclust(disclust, 5)

#To get flat clustering : 
k<-cutree(disclust,k = 160)
head(k)
#Once you have k which is the cluster no, attach it to your dataset
data$cluster<-k
View(data)
