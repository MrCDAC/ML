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
  J_hist<-rep(0,500)
  s<-dim(mat_x)
  for(i in 1:500){
    h <- mat_x%*%mat_theta
    mat_theta <- (mat_theta - alpha*(t(mat_x)%*%(h-mat_y))/s[1])
    
    c<-cost(mat_theta,mat_x,mat_y)
  print(c)    
    J_hist[i]<-c

  }
  return (mat_theta)
}

print(cost(mat_theta,mat_x,mat_y))
theta_f<-grad(mat_theta,mat_x,mat_y,0.1)
print(theta_f)
