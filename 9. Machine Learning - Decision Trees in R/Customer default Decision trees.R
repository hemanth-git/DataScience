cd=read.csv("C:\\Users\\hemanth\\Downloads\\JigSaw\\Day10\\goodbad.csv")
View(cd)
str(cd)
names(cd)
colSums(is.na(cd))
mean(cd$Age)
meanAge=mean(cd$Age,na.rm=TRUE)
cd$Age[which(is.na(cd$Age))]=meanAge
cd$GoodBad=as.factor(cd$GoodBad)
gbTree=ctree(GoodBad~Check_Account_Status+Duration+CreditHistory+
               Purpose+Amount+SavingsAcc+EmployTenure+Rate+
               Status+Debtors+CurrResidTenure+Propert+
               Age+Plans+Hous+ExCredit+Job+NumLiab+Tel+Foreign,data = cd)
plot(gbTree)
plot(gbTree,type="simple")
#Insights
#1. if checking amount is <$200 and
  #Duration of bank acount is <21
  #credit history is paid back duly.
  #  probabilty that customer default is 0.75
#2. if checking amount is <$200 and
  #Duration of bank acount is <21 and
  # and Savings Acc is < $1000 
  # probability that customer will default is 0.61

