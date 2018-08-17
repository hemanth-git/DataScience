##Probability distributions##
#Two types of fuctions : Density and Cummulative
# Density functions are like excel functions where we provide FALSE as the input to cummulative
# Cumulative functions in R, akin to excel's functions when cummulative parameter is TRUE

## Binomial distribution ##
# Density function #
# dbinom(number of success,number of trials, prob of success)

# P(X=3), X-> # heads in a toss of 10 coins
dbinom(3,10,0.5) #binom.dist(3,10,0.5,false)

# P(X=1,2,3,4,5,6,7,8,9,10), X-> # heads in a toss of 10 coins
dbinom(1:10,10,0.5) # supply number of success as a vector

# Cummulative probability function
#pbinom(number of successes, number of trails,prob success)

#P(X<=3),X-> # heads in a toss of 10 coins
pbinom(3,10,0.5) #excel binom.dist(3,10,0.5,true)

#P(X<=1,2,3,4,5,6,7,8,9,10), X-> # heads in a toss of 10 coins
pbinom(1:10,10,0.5) # supply number of success as a vector


## Negative Binomial Distribution ##

#dnbinom(num_f,num_s,prob_s)
#P(x=2), X-> Number of heads, P(2nd head occurs in 5th trial)
dnbinom(3,2,0.5) #excel negbinom.dist(3,2,0.5,false)

#P(Upto 2 heads occur in 5th trial)
#pnbinom(num_f,num_s,prob_s)
pnbinom(3,2,0.5)  #excel negbinom.dist(3,2,0.5,true)

## Hypergeometric Distribution
#dhyper(sample_s,pop_s,pop_f,sample_size)

#P(2 red when 5 cards are drawn at random from a deck without replacement)
dhyper(2,26,26,5) #excel hypgeom.dist(2,5,26,52,false)

#P(upto 2 red when 5 cards are drawn at random from a deck without replacement)
#phyper(sample_s,pop_s,pop_f,sample_size)
phyper(2,26,26,5) #excel hypgeom.dist(2,5,26,52,true)

##Poisson Distribution##
#dpois(x,mean)
#P(X=4|Mean=6), X->observed rate
dpois(4,6) #excel poisson.dist(4,6,false)

#ppois(x,mean)
#P(X<=4|Mean=6),  X->observed rate
ppois(4,6) #excel poisson.dist(4,6,true)


##Normal Distribution##
#P(X<=1.65)
pnorm(1.65,0,1) #excel, norm.dist(1.65,0,1,true)
