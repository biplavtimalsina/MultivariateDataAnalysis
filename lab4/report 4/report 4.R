
library(MASS)     # ... for Multivariate Normal Distribution
library(car)      # ... for ellipse plots



##########################################################################
#Q1 Find the correlation matrix for the data, study it, and comment.
########################################################################### 
dt<-read.table("salespeople.txt", header=FALSE)
dt

cor(dt)


##########################################################################
#Q2. Check data normality (univariate and multivariate)
########################################################################### 
#extract the columns of data into 7 seperate variables
 x1=dt$V1
 x2=dt$V2
 x3=dt$V3
 x4=dt$V4
 x5=dt$V5
 x6=dt$V6
 x7=dt$V7
x=cbind(x1,x2,x3,x4,x5,x6,x7)
 
#checking normality of the variables
qqnorm(x1)			# Normal probability plot for original variable
qqline(x1,col="red")
grid()

qqnorm(x2)			# Normal probability plot for original variable
qqline(x2,col="red")
grid()

qqnorm(x3)			# Normal probability plot for original variable
qqline(x3,col="red")
grid()

qqnorm(x4)			# Normal probability plot for original variable
qqline(x4,col="red")
grid()

qqnorm(x5)			# Normal probability plot for original variable
qqline(x5,col="red")
grid()

qqnorm(x6)			# Normal probability plot for original variable
qqline(x6,col="red")
grid()

qqnorm(x7)			# Normal probability plot for original variable
qqline(x7,col="red")
grid()

#checking multivariate normality of the seven variables

X=cbind(x1,x2,x3,x4,x5,x6,x7)
S1<-solve(cov(X))     # inverse of estimated covariance
d<-rep(0,50)
for (i in 1:len)
	d[i]<-t(X[i,])%*%S1%*%X[i,]  # distance from i-th point

qqplot(qchisq(seq(1,len)/len,7),d) # qqplot with chi-sq quantiles
segments(0,0,10,10,col='red',lwd=2)
grid()

ks.test(d,"pchisq",7) # formal KS test
#ks.test(d,"pchisq",2) what is the difference in these two????


####################################################################
#Q3. Perform FA with different number of common factors (try 1 to 5) and different rotations,
#analyze loadings and scores, find main groups of variables, find single-factor variables (if
#any), check scores’ normality. Find the optimal number of factors (consider the easiness
#of factor interpretation, number of factors, the proportion of the total variance explained,
#the residual matrix, etc.) Discuss and justify your choice. [This assignment will be
#evaluated on the basis of your discussion and justification. Support each of your
#statements by figures or numerical summaries of analysis.]
########################################################################


#doing factor analysis on our dataset
fa<-factanal(x,factors=1,rotation='none',scores='Bart')
fa










