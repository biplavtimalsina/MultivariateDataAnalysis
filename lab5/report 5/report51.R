library(MASS)

X<-iris
plot(X)
X<-iris[,-5]


d<-dist(iris[,3:4],method='euclidean') 
h<-hclust(d,method='sin')

d<-dist(X,method='euclidean')
h<-hclust(d,method='com')
plot(h,hang=-.01,xlab='',sub='',main='')

c<-cutree(h,k=3)
I1<-c==1
I2<-c==2
plot(X[,2],X[,4],pch=19,xlab="Sepal Width", ylab="Petal Width")# versicolor
points(X[I1,2],X[I1,4],col='red',pch=19)# setosa
points(X[I2,2],X[I2,4],col='blue',pch=19) # Virginica
.......................................................................
X<-cbind(iris[,3],iris[,4])
#question 3
###########################################
#euclidean single
d<-dist(X,method='euclidean') 
h<-hclust(d,method='sin')
windows()

plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
c<-cutree(h,k=3)
I1<-c==1
I2<-c==2
windows()
plot(X[,1],X[,2],pch=19)
points(X[I1,1],X[I1,2],col='red',pch=19)
points(X[I2,1],X[I2,2],col='blue',pch=19)
table(c,iris$Species)
########################################

#euclidean max

d<-dist(X,method='euclidean') 
h<-hclust(d,method='com')
windows()

plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
c<-cutree(h,k=3)
I1<-c==1
I2<-c==2
windows()
plot(X[,1],X[,2],pch=19)
points(X[I1,1],X[I1,2],col='red',pch=19)
points(X[I2,1],X[I2,2],col='blue',pch=19)
table(c,iris$Species)
##################################################


#euclidean average
d<-dist(X,method='euclidean') 
h<-hclust(d,method='ave')
windows()

plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
c<-cutree(h,k=3)
I1<-c==1
I2<-c==2
windows()
plot(X[,1],X[,2],pch=19)
points(X[I1,1],X[I1,2],col='red',pch=19)
points(X[I2,1],X[I2,2],col='blue',pch=19)
table(c,iris$Species)
..........................................................
#euclidean median
 d<-dist(X,method='euclidean') 
 h<-hclust(d,method='med')
windows()
 plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
 c<-cutree(h,k=3)
 I1<-c==1
 I2<-c==2
 windows()
 plot(X[,1],X[,2],pch=19)
 points(X[I1,1],X[I1,2],col='red',pch=19)
 points(X[I2,1],X[I2,2],col='blue',pch=19)
table (c,iris$Species)


##############################################################
#manhattan distance
d<-dist(X,method='manhattan') 
h<-hclust(d,method='ave')
windows()

plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
c<-cutree(h,k=3)
I1<-c==1
I2<-c==2
windows()
plot(X[,1],X[,2],pch=19)
points(X[I1,1],X[I1,2],col='red',pch=19)
points(X[I2,1],X[I2,2],col='blue',pch=19)
table(c,iris$Species)
##################################################################

d<-dist(X,method='minkowski',p=2) 
 h<-hclust(d,method='med')
windows()
 plot(h,hang=-.01,xlab='',sub='',main='',label=iris$Species)
 
 c<-cutree(h,k=3)
 I1<-c==1
 I2<-c==2
windows()
 plot(X[,1],X[,2],pch=19)
 points(X[I1,1],X[I1,2],col='red',pch=19)
 points(X[I2,1],X[I2,2],col='blue',pch=19)
table(c,iris$Species)


