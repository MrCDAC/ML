mat_x<-0
mat_y<-0
mat_theta<-0
cost<- function(mat_theta,mat_x,mat_y){
  j<-0
  s<-dim(mat_x)
  h <- mat_x%*%mat_theta
  t <- (h-mat_y)^2
  j<- sum(t)
  j<- j/2
  j<-j/(s[1])
  return (j);
}

grad<-function(mat_theta,mat_x,mat_y,alpha){
  s<-dim(mat_x)
  J_hist<-rep(0,50)
  s<-dim(mat_x)
 for(i in 1:50){
    h <- mat_x%*%mat_theta
  mat_theta <- (mat_theta - alpha*(t(mat_x)%*%(h-mat_y))/s[1])
 
 c<-cost(mat_theta,mat_x,mat_y)

  J_hist[i]<-c
 }
 print(mat_theta)
 print(c)
 
 return (J_hist)
  
}

data<-read.csv("hous.csv")
x<-data[,c(1,2,3,4,5)]
y<-data[,c(6)]
theta = c(0,0,0,0,0)
mat_x<-data.matrix(x)
mat_y<-data.matrix(y)
for(i in 1:5){
mat_x[,i]<-((mat_x[,i]-min(mat_x[,i]))/(max(mat_x[,i])-min(mat_x[,i])))
}
mat_y<-((mat_y-min(mat_y))/(max(mat_y)-min(mat_y)))
mat_theta<-data.matrix(theta)
c<-cost(mat_theta,mat_x,mat_y)
print(c)

J<-grad(mat_theta,mat_x,mat_y,0.1)

plot(J,type='l',xlab="No. of Iterations")
