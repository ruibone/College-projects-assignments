newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0 # x0: the initial value
  fx <- ftn(x)
  iter <- 0
  while ((abs(fx[1]) > tol) & (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else { # abs(fx[1]) <= tol
    cat("Algorithm converged\n")
    return(x)
  }
}

###possion###

logLik <- function(X, lambda){
  likelihood <- 0
  for (i in 1:length(X)){
      likelihood <- likelihood+log((exp(-lambda)*lambda^X[i])/factorial(X[i]))
  }
  return(likelihood)
}
X <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)   
lambda <- seq(0, 10, 0.01)
fp2 <- logLik(X,lambda)
plot(lambda, fp2, type = "l", main = "log-likelihood")
abline(v = sum(X)/length(X), col = 3)

ftn <- function(lambda) {
  f <- -length(X)+sum(X)/lambda
  df <- -(lambda^(-2))*sum(X)
  return(c(f, df))
}

lambda <- seq(0, 10, 0.01)
f <- -length(X)+sum(X)/lambda
plot(lambda, f, type = "l", main = "decide initial value")

newtonraphson(ftn, 1, 1e-6)

###normal###

BMI <- read.csv("C:/Users/Darui Yen/OneDrive/орн▒/BMIrepeated.csv", header = T, sep = ",")
for (i in 1:nrow(BMI)) {
  if (BMI$SEX[i] == "M") {
    BMI$SEX[i] <- 0
  }else BMI$SEX[i] <- 1
}
BMI$SEX <- as.numeric(BMI$SEX)


x <- as.matrix(cbind(rep(1, nrow(BMI)), BMI$SEX, BMI$AGE))
beta <- solve(t(x)%*%x)%*%t(x)%*%matrix(BMI$BMI3, nrow(BMI))
re <- BMI$BMI3 - x%*%beta
sigma2 <- sum(re^2)/nrow(BMI)
loglike <- -(nrow(BMI)/2)*(log(2*pi)+log(sigma2)+1)
