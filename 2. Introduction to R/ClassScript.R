#basics

#arthematic operations
sqrt(25)
x=sqrt(25)
x
class(x)
y="hemanth"
y
class(y)
z='z'
class(z)
z='zebra'
class(z)

# will load the objects in the current
ls()
# removing objects
rm(z)
ls()

#vectors
sample.num=c(1,2,3,4,5,6)
sample.char=c("hemanth","virat","dhoni","DK","KW")
sample.bool=c(TRUE,FALSE,TRUE,FALSE,TRUE)
sample.dataframe=data.frame(sample.bool,sample.char)
sample.dataframe$sample.bool=TRUE
sample.dataframe
class(sample.bool)
for (variable in sample.char) {
  #print(variable)
  if(sample.bool[match(variable,sample.char)]){
   print(variable)
  }
  else
  print(cat(variable, "Out of squad"))
}

#creating sequence of numbers

seq1=seq(1:10)
seq1

mons=c("March","April","January","November","January",
"September","October","September","November","August",
"January","November","November","February","May","August",
"July","December","August","August","September","November",
"February","April");
class(mons)
mons=factor(mons)
class(mons)
table(mons)
mons
#data frame

sample2.num=c(1,2,3,4,5,6)
sample2.char=c("hemanth","virat","dhoni","DK","KW")
sample2.bool=c(TRUE,FALSE,TRUE,FALSE,TRUE)
sample2.dataframe=data.frame(sample.bool,sample.char)
sample2.dataframe
class(sample2.dataframe)
View(sample2.dataframe)



exampledata=data.frame(names=c("Amit","Anne","Jay"),
                       income=c(80000,75000,60000),
                       Gender=c("M","F","M"))
exampledata[1,1]
exampledata[,2]
# to extract or filter the data with Gender=="M" and store in another dataframe
dt=exampledata[exampledata$Gender=="M" ,]
dt
# filtering data frame with logical operator
dt=exampledata[exampledata$Gender=="M" & exampledata$income>70000 ,]
dt
View(exampledata[exampledata$Gender=="M" & exampledata$income>70000 ,])


sample3.md.vector =c("anni","ar","mani","dsp")
sample3.rating.vector =c(4,6,7,5)
sample3.dataframe= data.frame(sample3.md.vector,sample3.rating.vector,col.names = FALSE)
ave(sample3.dataframe[2])
mean(sample3.dataframe[2])
sample3.dataframe
