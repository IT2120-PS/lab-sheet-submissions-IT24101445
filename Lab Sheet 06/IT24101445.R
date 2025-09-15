#Exercise Lab 06

setwd("C:\\Users\\ashan\\Desktop\\IT24101445")

#01
#Part i
#Binomial Distribution
#X = number of students who passed the test
#random variable X has binomial distribution with n=50 and p=0.85

#Part ii  ( P(X>=47) ) = (P(x>46))
pbinom(46,50,0.85,lower.tail=FALSE)


#02
#Part i
#random variable X = number of calls received per hour.

#Part ii
#Poisson Distribution
#random variable X has poisson distribution with lamda=12 

#Part iii
dpois(15,12)

