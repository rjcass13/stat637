library(faraway) #Load package for the data
data(orings)
y <- orings$damage #define a generic response vector "y"
y[which(orings$damage>0)] <- 1 #1 if any of the o-rings failed
temp <- orings$temp #easier to write "temp" #single covariate
X <- cbind(rep(1, length(temp)), temp) #X matrix
#Starting values for IWLS
b0 <- 0
b1 <- 0
b <- c(b0, b1)
epsilon <- .00001
diff <- 1
iter <- 0

#IWLS Loop
while((diff > epsilon) && FALSE){
  # #W and Z in terms of beta only (not g'(mu), etc.)
  # #w <- exp(b0 + temp*b1)/((1+exp(b0 + temp*b1))^2)
  # w <- exp(X%*%b)/((1+exp(X%*%b))^2) #using vector notation
  # W <- diag(c(w)) #create the diagonal matrix
  # #z <- b0 + temp*b1 + (y - exp(b0 + temp*b1)/(1+exp(b0 + temp*b1)))*((1+exp(b
  # z <- X%*%b + (y-exp(X%*%b)/(1+exp(X%*%b)))*(1+exp(X%*%b))^2/exp(X%*%b) #agai
  #Z and W as general as possible
  mui <- exp(X%*%b)/(1+exp(X%*%b))
  varyi <- mui*(1-mui)
  gprimemui <- 1/(mui*(1-mui))
  z <- X%*%b + (y-mui)*gprimemui
  W <- diag(c(1/(varyi*gprimemui^2)))
  b <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
  diff <- (b[1]-b0)^2 + (b[2]-b1)^2
  b0 <- b[1]
  b1 <- b[2]
  iter <- iter + 1
  print(paste("Iteration: ", iter))
  print(paste("Estimate: ", b))
}





# Poisson Version
while(diff > epsilon) {
  # Poisson Exponential Link
  mui <- exp(X%*%b)
  # Poisson variance = mean
  varyi <- mui
  # Derivative of the link function of mui 
  gprimemui <- 1/mui

  z <- X%*%b + (y-mui)*gprimemui
  W <- diag(c(1/(varyi*gprimemui^2)))
  b <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
  diff <- (b[1]-b0)^2 + (b[2]-b1)^2
  b0 <- b[1]
  b1 <- b[2]
  iter <- iter + 1
  print(paste("Iteration: ", iter))
  print(paste("Estimate: ", b))
}

# Get the covariance using the final value of b
mui <- exp(X%*%b)
varyi <- mui
gprimemui <- 1/mui
W <- diag(c(1/(varyi*gprimemui^2)))
J <- t(X)%*%W%*%X
cov <- solve(J)
cov

summary(glm(y ~ temp, family = poisson(link='log')))$cov.scaled

# Confidence interval for Beta1
b1 + c(-1, 1)*(1.96*sqrt(cov[2,2]))

