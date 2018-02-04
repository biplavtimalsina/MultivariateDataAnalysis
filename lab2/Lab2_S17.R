#=================================================#
#                   STAT 755                      #
#             Generalized variance                #
#         Multivariate Normal Distribution        #
#=================================================#

#=================================================
# Install libraries ...
#=================================================
library(Matrix)   # ... for matrix operations
library(car)      # ... for ellipse plots
library(stats)    # ... for statistical operations
library(MASS)     # ... for Multivariate Normal Distribution
library(graphics) # ... for arrows


#============================================================
# Multivariate Normal Sample ...
# ... as a linear combination of iid standard normal rvs
#============================================================

len<-5
N<-matrix(rnorm(len*2),len,2) # 5x2 iid N(0,1) rvs
A<-matrix(c(1,1,1,-1),2,2)   # 2x2 matrix of coefficients
X<-N%*%A                      # 5x2 linear combination

#============================================================
# Multivariate Normal Sample ...
# ... using an R operator
#============================================================

Sigma <- matrix(c(10,4,4,2),2,2)
mvrnorm(n=1,c(0,0),Sigma) # sample 1x2 with mean [0,0]
mvrnorm(n=5,c(0,0),Sigma) # sample 5x2 with mean [0,0]
mvrnorm(n=5,c(-100,100),Sigma) # sample 5x2 with mean [-100,100]

var(mvrnorm(n=1000, rep(0, 2), Sigma)) # Sigma is the population variance
var(mvrnorm(n=1000, rep(0, 2), Sigma, empirical = TRUE)) # Sigma is the sample variance

# Correlation and covariance matrices
#-------------------------------------- 
cor(N) # correlation matrix
cor(X) # correlation matrix
cov(N) # variance-covariance matrix
cov(X) # variance-covariance matrix
var(N) # the same as cov(N)
var(X) # the same as cov(X)

#=========================================================
# Generalized variance I: Volume occupied by data
#---------------------------------------------------------
# This example illustrates that generalized variance is
# related to the volume occupied by data scatter
#=========================================================
len<-1000
N<-matrix(rnorm(len*2),len,2)    # 1000x2 iid N(0,1) rvs
A<-matrix(c(2,1,1,2),2,2)        # 2x2 matrix of coefficients
X<-N%*%A                         # 1000x2 linear combination
X[,1]=X[,1]+5                    # shift first column
N[,2]=N[,2]+5
det(cov(N))                      # gen. var for N
det(cov(X))                      # gen. var for X
e1<-SA(X)                        # ellipses for X
e2<-SA(N,add=T)                  # ellipses for N

#==========================================================
# Generalized variance II: Linearly dependent observations
#----------------------------------------------------------
# This example shows how to find linearly dependent vectors
# in a data matrix with zero generalized variance
#==========================================================
len<-1000
N<-matrix(rnorm(len*2),len,2)    # 100x2 iid N(0,1) rvs
A<-matrix(c(1,1,1,-1,2,3),2,3)   # 2x3 matrix of coefficients
X<-N%*%A                         # 100x3 linear combination
det(cov(N))                      # gen. var for N
det(cov(X))                      # gen. var for X

Sigma<-cov(X)                    # covariance matrix
e<-eigen(Sigma)                  # eigenvalues, eigenvectors 
e
plot(X%*%e$vectors[,1],col='blue')  # lin. comb. for max. eigenvalue
points(X%*%e$vectors[,3],col='red') # lin. comb. for 0-eigenvalue

e$vectors[,3]/e$vectors[2,3] # "good" form of linear dependence

#==================================================================
# Multivariate Normal (MVN) Distribution
#------------------------------------------------------------------
# This example shows how to 
# a) create Normal rvs with given variance matrix from iid N(0,1)
# b) create iid N(0,1) from Normal rvs with given covariance matrix 
#==================================================================
Sigma <- matrix(c(10,4,4,2),2,2)   # variance matrix
I<-diag(c(1,1))                    # identity matrix
N<-mvrnorm(n=10000,c(0,0),I)        # MVN with variance I
X<-mvrnorm(n=10000,c(0,0),Sigma)    # MVN with variance Sigma

e<-eigen(Sigma)                    # spectral decomposition
P<-e$vectors                       # eigenvectors
L<-e$values                        # eigenvalues

Sm05<-P%*%sqrt(diag(1/L))%*%t(P)   # inverse square-root matrix
Sp05<-P%*%sqrt(diag(L))%*%t(P)     # square-root matrix

Z<-t(Sm05%*%t(X))                  # vector of iid N(0,1) rvs
X1<-t(Sp05%*%t(N))                 # MVN rv with variance Sigma
var(Z)
var(X1)
Sigma

#====================================================
# Chi-square distribution of statistical distances
#----------------------------------------------------
# This example shows how to test for multi-normality
# using the chi-square distribution
#====================================================
Sigma <- matrix(c(10,4,4,2),2,2)   # variance matrix

# (A) True Multivariate Normal
len=1000               
X<-mvrnorm(n=len,c(0,0),Sigma)     # 1000x2 MVN rv
S1<-solve(cov(X))     # inverse of estimated covariance
d<-rep(0,len)
for (i in 1:len)
	d[i]<-t(X[i,])%*%S1%*%X[i,]  # distance from i-th point

qqplot(qchisq(seq(1,len)/len,2),d) # qqplot with chi-sq quantiles
segments(0,0,10,10,col='red',lwd=2)
grid()

ks.test(d,"pchisq",2) # formal KS test

# (B) Not Multivariate Normal
len=1000               
X<-mvrnorm(n=len,c(0,0),Sigma)     # 1000x2 MVN rv
X<-X^2

S1<-solve(cov(X))     # inverse of estimated covariance
d<-rep(0,len)
for (i in 1:len)
	d[i]<-t(X[i,])%*%S1%*%X[i,]  # distance from i-th point

qqplot(qchisq(seq(1,len)/len,2),d) # qqplot with chi-sq quantiles
segments(0,0,10,10,col='red',lwd=2)
grid()

ks.test(d,"pchisq",2) # formal KS test


#===================================================
# Function that illustrates spectral decomposition
# and statistical distance ellipses
#===================================================

SA <- function(X,add=FALSE,data.plot=TRUE)
{
# Vector of means
#==============================
n<-dim(X)[1]
ones<-matrix(rep(1,n),ncol=1)
mu<-as.vector(t(X) %*% ones / n)

# Variance
#===========================================================
Sigma<-var(X)

e<-eigen(Sigma)
par(bg='yellow')
ellipse(mu,Sigma,3,add=add,xlim=range(X),ylim=range(X))
ellipse(mu,Sigma,2,add=TRUE)
ellipse(mu,Sigma,1,add=TRUE)
if (data.plot)
points(X[,1],X[,2],pch=20,col=4)
arrows(mu[1],mu[2],mu[1]+e$vectors[1,1]*sqrt(e$values[1]),
mu[2]+e$vectors[2,1]*sqrt(e$values[1]),length=.1,col='green',lwd=2)
arrows(mu[1],mu[2],mu[1]+e$vectors[1,2]*sqrt(e$values[2]),
mu[2]+e$vectors[2,2]*sqrt(e$values[2]),length=.1,col='green',lwd=2)
e
}

ellipse<-function(mu,Sigma,R,col='red',add=FALSE,xlim=NULL,ylim=NULL,N=1000)
{	
	# Find coordinates of a circle
	t<-seq(0,2*pi,length.out=N)
	x<-R*cos(t)
	y<-R*sin(t)
	
	# Spectral decomposition of Sigma
	e<-eigen(Sigma)                    # spectral decomposition
	P<-e$vectors                       # eigenvectors
	L<-e$values 

    # Square root matrix
    S05<-P%*%sqrt(diag(L))%*%t(P)
    
    # Ellipse cordinates 
	vec<-cbind(x,y)
	vec<-t(vec%*%S05)
	x<-vec[1,]+mu[1]
	y<-vec[2,]+mu[2]
	
	if (add)
		{
			points(x,y,type='l',col=col)
		}	
    else
     	plot(x,y,type='l',col=col,xlim=xlim,ylim=ylim)
		
}
