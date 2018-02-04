require(ggplot2)
require(GGally)
require(CCA)

library(MASS)     # ... for Multivariate Normal Distribution
library(car)      # ... for ellipse plots


table<-read.table('Lab3_close.csv',sep=',', header=TRUE)


# Remove long-term trend
#=========================
P<-table[,seq(2,11)]
cor(P)				# remove the dates
P<-log10(P)

PD<-P*0
for (i in seq(1,10))
{
t<-ts(P[,i],frequency=365)
s<-stl(t,s.window=250)
PD[,i]=t-s$time.series[,2]
}

ggpairs(PD)

#initial grouping into BA,NASDAQ,DELL,GE,GM,IBM,MSFT  and MD,PEPSI,DELL

X<-PD[,c(1,2,4,7,9)]
Y<-PD[,c(3,8,10,5,6)]

cor(X,Y)
cancor(X,Y)

#.995 
#.73

X<-PD[,c(2,4,5,8)]
Y<-PD[,c(1,3,6,7,9,10)]

cor(X,Y)
cancor(X,Y)

#.995 
#.801
................................
X<-PD[,c(1,2,4,5,6,7,9,8)]
Y<-PD[,c(3,10)]
cor(X,Y)

cancor(X,Y) 
#.9891
#.58
......................................
X<-PD[,c(1,2,4,5,6,7,9,8)]
Y<-PD[,c(3,10)]
cor(X,Y)
cancor(X,Y) 
#.991
#.554

X<-PD[,c(1,4,5,6,7,9)]
Y<-PD[,c(3,8,10,2)]
cor(X,Y)
cancor(X,Y)
#.994
#.777

............................
X<-PD[,c(4,7,9)]
Y<-PD[,c(1,2,3,5,6,8,10)]
cor(X,Y)
cancor(X,Y)
#.992
#.62

..............................
X<-PD[,c(2,4,7,9)]
Y<-PD[,c(1,3,5,6,8,10)]
cor(X,Y)
cancor(X,Y)
#.994
#.668

...............................
X<-PD[,c(2,4,5,7,9)]
Y<-PD[,c(1,3,6,8,10)]
cor(X,Y)
cancor(X,Y)
#.985
#.684
...................................
X<-PD[,c(2,4,5,7,9)]
Y<-PD[,c(1,3,6,8,10)]
cor(X,Y)
cancor(X,Y)
#.684

........................................
X<-PD[,c(8,1,2,3,4,7)]
Y<-PD[,c(10,9,5,6)]
cor(X,Y)
cancor(X,Y) 
#.9964
#.828
ggpairs(X)
...........................................
X<-PD[,c(8,1,2,4,7)]
Y<-PD[,c(10,9,5,6,3)]
cor(X,Y)
cancor(X,Y) 
#.9972
#.82

............................
 X<-PD[,c(8,1,2,4)]
Y<-PD[,c(10,9,5,6,3,7)]
cor(X,Y)
cancor(X,Y) 
#.9974
#.81

................................
 X<-PD[,c(1,9,4)]
Y<-PD[,c(10,2,5,6,3,7,8)]
cor(X,Y)
cancor(X,Y) 
#.9969
$.79
...............................
 X<-PD[,c(2,4,3)]
Y<-PD[,c(9,5,10,1,6,7,8)]
cor(X,Y)
cancor(X,Y) 
#.9969
#.771
................................
 X<-PD[,c(8,1,4)]
Y<-PD[,c(10,9,2,5,6,3,7)]
cor(X,Y)
cancor(X,Y) 
#.9976
#.69
................................
 X<-PD[,c(8,1,4,5)]
Y<-PD[,c(10,9,2,3,6,7)]
cancor(X,Y) 
ggpairs(X)
#.99814
#.79
..........................
 X<-PD[,c(9,8,10,5,4,6)]
Y<-PD[,c(1,3,7,2)]
cancor(X,Y) 
#
#.79
ggpairs(P)

 X<-PD[,c(2,4,5,9,8)]
Y<-PD[,c(1,3,6,7,10)]
cancor(X,Y) 
#
#.73

 X<-PD[,c(8,4,5,2)]
Y<-PD[,c(1,3,6,7,9,10)]
cancor(X,Y) 
#
#.80

