#day3 customers.csv data manipulation

getwd()
setwd("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day3 R lang\\filesforday3")
good1=read.table("customers.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)
View(good1)
str(good1)
good2=read.table("customers.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
View(good2)
str(good2)
# converting a col in to vector
zip=c(good2$zip)

LAPeople=good1[good1$state=="LA",]
LAPeople
read.delim("customers.csv",header = TRUE,sep=",")
class(good2)
#using as.is to convert the with factors of the required column
good3=read.table("customers.csv",header=TRUE,sep=",",stringsAsFactors = TRUE,as.is = "first_name")
str(good3)

#using na.strings to blank feilds to fill with "NA"
good4=read.table("customers3.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)
View(good4)

good5=read.table("customers3.csv",header=TRUE,sep=",",stringsAsFactors = TRUE,na.strings = c("","NA"))
View(good5)

# saving the multiple objects to a file
save(good1,good2,good5,file="good1-5.rda")
getwd()

# installing the packages and sample data sets 
install.packages("datasets")
library(datasets)
data()
data("iris")
library(XLConnect)
install.packages("XLConnect")


# pulling the data from a url

dataurl=url("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
webtextutl=readLines(dataurl,n=50)
View(webtextutl)
class(dataurl)

dataurl=url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
dataframeurl=read.table(dataurl)
View(dataframeurl)

#downloading file from url links
download.file("http://www.gutenberg.org/files/2701/2701-0.txt","sampleurl.txt")

#JSON file
library(jsonlite)
data.customersjson=fromJSON("customersJSON.txt")
data.customersjson
class(data.customersjson)
View(data.customersjson)
str(data.customersjson)
#data object is loaded with character from JSON file to convert to Factor
data.convertToFactor=type.convert(data.customersjson)
str(data.convertToFactor)
