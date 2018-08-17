library(dplyr)
install.packages("ggplot2")
library(ggplot2)
dat1=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day6\\Batch\\audit.csv")
##Grammar elements##
#Aesthetic maps
#Geoms
#Statistical Transformation
#Scales
p<-ggplot(dat1,aes(x=Age,y=Income))
p+geom_point(aes(color=Gender))
p+geom_point(aes(color="blue"))
p+geom_point(color="blue")#Mapping Vs Fixing a value

p<-ggplot(dat1,aes(x=Gender,y=Income))
p+geom_bar()#Why?
p+geom_bar(stat="identity")

p<-ggplot(dat1,aes(x=Age,y=Income))
p+geom_point(aes(color=Gender))
p+geom_point(aes(color=Gender))+scale_x_continuous(breaks=seq(0,80,10))


p+geom_point(aes(size=Gender,color=Income))#Adjust the size of points

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range = c(1,2))
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range = c(1,3))

#Adjust the gradient of the color
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")

#What if I don't want to see the legend corresponding to size?
p+geom_point(aes(size=Gender,color=Income))+
  scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")+guides(size=F)

#Can we improve this further?

p+geom_point(aes(size=Gender,color=Income))+
  scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")+guides(size=F)+
  facet_grid(Gender~.)#Anything else we can do?

p+geom_point(aes(size=Gender,color=Income,))+
  scale_color_continuous(low = "black",high = "white")+
  scale_size_discrete(range = c(1,2))

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")+guides(size=F)+
  geom_smooth()+facet_grid(Gender~.)

dat1%>%mutate(incomeBin=ntile(Income,2))->dat2
q<-ggplot(dat2,aes(x = Age,y = Income))
q+geom_point(aes(size=Gender,color=Income))+
  scale_size_discrete(range = c(1,2))+
  scale_color_continuous(low="black",high = "red")+
  +scale_y_continuous(breaks=seq(1000))+
  facet_grid(incomeBin~.)
