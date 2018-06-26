sigmoid<-function(z){
  s<-(1/(1+exp(-z)))
  return (s)
}

cost<-function(x,y,theta){
  s<-dim(x)
  j<-0
  h<-sigmoid(x%*%theta)
  j<- -sum(y*log(h) +(1-y)*log(1-h))/size[1]
  return (j)
}

gradient<-function(x,y,theta){
  s<-dim(x)
  j_hist<-rep(0,2000)
  for(i in 1:2000){
    
    h<-sigmoid(x%*%theta)
    theta<- theta - (0.1*(t(x)%*%(h-y)))/s[1]
 
    
    j_hist[i]<-cost(x,y,theta)
  }
  
  print(theta)
  print(cost(x,y,theta))
  return (theta)
}

data<-read.csv("titan_train.csv")
x<-data$Age
y<-data$Survived
size<-dim(x)

x[is.na(x)==T]<-mean(x,na.rm = T)
x<-data.matrix(x)
size<-dim(x)
ones<-rep(1,size[1])
z<-matrix(c(ones,x),nrow=size[1],ncol=2)
z[,2]<-z[,2]-min(z[,2])
z[,2]<-z[,2]/(max(z[,2])-min(z[,2]))
theta<-c(0,0)
theta<-data.matrix(theta)
y<-data.matrix(y)
print(cost(z,y,theta))
thetaf<-gradient(z,y,theta)

plot(j,type='l')

test<-read.csv("test.csv")
test$Age[is.na(test$Age)==T]<-mean(test$Age,na.rm = T)
xtest<-test$Age


xtest<-data.matrix(xtest)
sizetest<-dim(xtest)

onestest<-rep(1,sizetest[1])
ztest<-matrix(c(onestest,xtest),nrow=sizetest[1],ncol=2)
ztest[,2]<-ztest[,2]-min(ztest[,2])
ztest[,2]<-ztest[,2]/(max(ztest[,2])-min(ztest[,2]))
htest<- sigmoid(ztest%*%thetaf)
