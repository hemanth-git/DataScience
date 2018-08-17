#install the sqldf
install.packages("sqldf")
library(sqldf)

sqldat1=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day4\\datafiles\\oj.csv")

sqldf("Select * from sqldat1 where brand='tropicana'")

sqldat1%>%mutate(Decile=ntile(desc(income),3))%>%
  sqldf("select brand,avg(income),min(income),max(income),count(income) 
        from sqldat1 group by brand")
sqldat1%>%mutate(Decile=ntile(INCOME,3))%>%sqldf("select brand,avg(income),min(income),max(income),count(income) from sqldat1 group by Decile")


id1=c(1,2,3,4,5)
names1=c("a","b","c","d","e")
values1=c(200,250,300,600,100)
table1=data.frame(id1,names,values)

id1=c(1,1,3,5)
packed=c("xyz","vnm","cdm","dma")
quant=c(200,250,300,600)
table2=data.frame(id1,packed,quant)


data1=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day4\\datafiles\\left.csv")
data2=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day4\\datafiles\\right.csv")

innerJoin=sqldf("select * from data1 join data2 using(CustomerId)")

leftJoin= sqldf("select * from data1 left join data2 using(CustomerId)")
rightJoin= sqldf("select * from data1 right join data2 using(CustomerId)")

# this to support left outer join
merge(data1,data2,all.x = TRUE)

#this to support right outer join
merge(data1,data2,all.y = TRUE)

#this is to support inner join eliminating not matched rows from right

Result=data1[data2,nomatch=0]
