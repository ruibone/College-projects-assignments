newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0         # x0: the initial value
  fx <- ftn(x)
  iter <- 0
  while ((max(abs(fx[[1]])) > tol) & (iter < max.iter)) {
    x <- x - solve(fx[[2]]) %*% fx[[1]]
    fx <- ftn(x)
    iter <- iter + 1
  }
  if (max(abs(fx[[1]])) > tol) {
    cat('Algorithm failed to converge\n')
    return(NULL)
  } else {      # max(abs(fx[[1]])) <= tol
    cat("Algorithm converged\n")
    return(x)
  }
}


resp <- read.csv("C:/Users/Darui Yen/OneDrive/орн▒/resp.csv", header = T, sep = ",")

X <- cbind(rep(1, nrow(resp)), ifelse(resp$treatment == "A", 0, 1), resp$age, resp$baseline)
Y <- resp$outcome
ftn <- function(betacoef) {
  pi1 <- exp(X%*%betacoef) / (1 + exp(X%*%betacoef))
  gradient <- t(X)%*%(Y - pi1)
  heissan <- -t(X)%*%diag(c(pi1*(1-pi1)))%*%X
  loglikelihood <- sum(Y*log(pi1/(1-pi1)) + log(1 - pi1))
  return(list(gradient, heissan, loglikelihood))
}

beta <- newtonraphson(ftn, rep(0, ncol(X)))

solve(-ftn(beta)[[2]])


ftn(beta)[[3]]
