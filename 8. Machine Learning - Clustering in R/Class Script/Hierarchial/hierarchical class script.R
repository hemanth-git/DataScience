##--------------------------------------Step1 : Read the data---------------------------------------------
europe<-read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day9\\Class Script\\europe.csv")
names(europe)
dim(europe)

##--------------------------------------Step2 : Scaling data + Finding Distance Measures---------------------------------------------

#Distance calculation : Euclidean
#Method : Average

#Daisy function in R scales the data and computes the distance measures
library(cluster)
?daisy
dmatrix<-daisy(europe[-1],metric="euclidean",stand=TRUE)
summary(dmatrix)
class(dmatrix)


distmatrix<-dist(dmatrix)
str(distmatrix)
