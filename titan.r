sigmoid<-function(z){
  s<-1/(1+exp(-z))
  return (s)
}

cost<-function(theta,x,y){
  s<-dim(x)
  j<-0
  h<-sigmoid(x%*%theta)
  j<- sum(-y*log(h)-(1-y)*log(1-h))/s[1]
  return (j)
  
}
grad<-function(theta,x,y,a){
  s<-dim(x)
  j_hist<-rep(0,50000);
  for(i in (1:50000))
  {
    h<-sigmoid(x%*%theta)
    theta<-theta-(a*(t(x)%*%(h-y)))/s[1]
    c<-cost(theta,x,y)
    j_hist[i]<-c
  }
  return(theta)
  
  
}

data<-read.csv("titan_train.csv")
y<-data[,2]
x<-data[,-c(1,2,4,9,12,11)]
x$Age[is.na(x$Age)]<-mean(x$Age,na.rm = T)
x$Age<-((x$Age-min(x$Age))/(max(x$Age)-min(x$Age)))
x$Pclass<-((x$Pclass-min(x$Pclass))/(max(x$Pclass)-min(x$Pclass)))
levels(x$Sex)<-0:1
x$Fare<-((x$Fare-min(x$Fare))/(max(x$Fare)-min(x$Fare)))
theta = c(1,1,1,1,1,1)
x$SibSp<-((x$SibSp-min(x$SibSp))/(max(x$SibSp)-min(x$SibSp)))
x$Parch<-((x$Parch-min(x$Parch))/(max(x$Parch)-min(x$Parch)))
x<-data.matrix(x)
y<-data.matrix(y)
theta<-data.matrix(theta)
print(cost(theta,x,y))
theta<-grad(theta,x,y,0.1)

print(cost(theta,x,y))
print(theta)

test<-read.csv("test.csv")
x_test<-test[,-c(1,3,8,10,11)]
x_test$Age[is.na(x_test$Age)]<-mean(x_test$Age,na.rm = T)
x_test$Fare[is.na(x_test$Fare)]<-mean(x_test$Fare,na.rm = T)
x_test$Age<-((x_test$Age-min(x_test$Age))/(max(x_test$Age)-min(x_test$Age)))
x_test$Pclass<-((x_test$Pclass-min(x_test$Pclass))/(max(x_test$Pclass)-min(x_test$Pclass)))
levels(x_test$Sex)<-0:1
x_test$Fare<-((x_test$Fare-min(x_test$Fare))/(max(x_test$Fare)-min(x_test$Fare)))
x_test$SibSp<-((x_test$SibSp-min(x_test$SibSp))/(max(x_test$SibSp)-min(x_test$SibSp)))
x_test$Parch<-((x_test$Parch-min(x_test$Parch))/(max(x_test$Parch)-min(x_test$Parch)))
x_test<-data.matrix(x_test)
h_test<-sigmoid(x_test%*%theta)


h_test<-round(h_test)
print(sum(h_test))


