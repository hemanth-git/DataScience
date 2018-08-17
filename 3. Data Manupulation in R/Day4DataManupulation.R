# installing dplyr
install.packages("dplyr")
library(dplyr)
setwd("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day4")
dat1=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day4\\datafiles\\audit.csv")
str(dat1)
head(dat1)
View(dat1)

#filter used to filter the rows having private and male
dat2=filter(dat1,Employment=="Private",Gender=="Male")
View(dat2)

dat3=select(filter(dat1,Employment=="Private" & Gender=="Male"),Age,Marital)
View(dat3)
str(dat3)
View(group_by(dat3,Marital))

# to get the frequency of all the occurences
# group_by will group the factors n() is the count
cat4=summarise(group_by(dat1,Marital),n())

# pipes used in the %

dat1%>%filter(Occupation=="Machinist" & Income>7000 )%>%select(Age,Marital)%>%group_by(Marital)%>%summarise(n())

dat1%>%filter(Occupation=="Machinist" & Income>7000 )%>%
select(Age,Marital)

dat1%>%group_by(Gender,Marital)%>%summarise(mean(Income))->dat5
dat1%>%group_by(Occupation,Gender)%>%summarise(mean(Income))->dat6
dat1%>%group_by(Gender)%>%summarise(mean(Hours))->dat7
dat1%>%group_by(Education,Employment,Occupation)%>%summarise(Count=n())%>%
  arrange(desc(Count))->dat8
dat1%>%group_by(Education,Employment)%>%summarise(Count=n())%>%
  arrange(desc(Count))->dat9
#to get the mean income of employment with education and count
dat1%>%group_by(Education,Employment)%>%summarise(mean(Income),Count=n())%>%
  arrange(desc(Count))->dat10
# to get the mean of hours worked and mean income with education and employment
dat1%>%group_by(Education,Employment)%>%summarise(mean(Income),mean(Hours),Count=n())%>%
  arrange(desc(Count))->dat11
# to get the least two incomes of Employment
dat1%>%select(Gender,Income)%>%group_by(Gender)%>%
  filter(min_rank(desc(Income))<=2)%>%arrange(desc(Income))
# to get the cumulative distribution of sorted by income with 1% of the desc
dat1%>%select(Gender,Income)%>%group_by(Gender)%>%
  filter(cume_dist(desc(Income))<=0.01)%>%arrange(desc(Income))

# to mutate to create or add the new column to the data frame.
# ntile is to bin/categorize the col into given equal parts for the specified col.

dat1%>%mutate(Bin1=ntile(Hours,5))->dat12

dat12%>%mutate(Bin2=ntile(desc(Income),10))->dat13

dat13%>%group_by(Bin2)%>%summarise(n())
x=1:10
x
#returns a cumulative vector sum values in the cumulation
cumsum(1:10)
#returns a cumulative vector max values in the cumulation
cummax(1:10)
#returns a cumulative vector min values in the cumulation
cummin(1:10)
#returns a cumulative vectorcumulatively any values greater than 9
cumany(x>=9)
# cumulatively all the values greater than 9
cumall(x>=9)

# decile min, max, mean, sum, count.
dat1%>%mutate(IncomeDecile=ntile(desc(Income),10))%>%
  group_by(IncomeDecile)%>%summarise(Min=min(Income),Max=max(Income),
                                     Mean=mean(Income),
                                     Count=n())%>%
  mutate(Cumsum=cumsum(Count))%>%mutate(Cu=cumany(Cumsum>800))->dat14

