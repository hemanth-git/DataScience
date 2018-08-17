##############################################################################################################################################################
# t distribution
#P(t<=x)
pt(1.65,29)

# We take a sample of 28 items the sample mean is 30, sample sd = 5, the sample comes from a population whose mean is 35, what are the chances we'll observe a sample mean of atmost 30?

#P(X<=30) 

# t value corresponding to 30 = (30-35)/(5/sqrt(28))
# t value = -5.291503

#P(X<=30) => P(t<=-5.291503)
#options(scipen=999)
pt(-5.291503,27) #excel, t.dist(-5.291503,27,true)

## t tests ##
# Single sample t tests
# Create a vector of data
set.seed(100)
x<-rnorm(16,2,1) #Data series of 16 points with mean 2 and sd 1
mean(x)
# H0: Mean = 2
# Ha: Mean < 2
t.test(x,alternative = "less",mu = 2)

# H0: Mean = 2
# Ha: Mean Not equal to 2
t.test(x,alternative="two.sided" ,mu = 2)

#Two sample t-tests#
#Independent Sample Test
#Create two random samples
set.seed(100)
x1<-rnorm(20,2,1)
x2<-rnorm(20,3,1.5)
mean(x1)
mean(x2)
#Ho: Mean1 = Mean2 (Mean difference is zero)
#Ha: Mean1 and Mean2 are different
t.test(x1,x2,alternative = "two.sided",mu = 0)

#Paired sample
#Ho: Mean1 = Mean2 (Mean difference is zero)
#Ha: Mean1 and Mean2 are different
t.test(x1,x2,alternative = "two.sided",mu = 0,paired = TRUE)

############################################################################################################################################################
