################################
##   likelihood calculation   ##
################################

#####binomial#####
#plot
n <- 20
p <- seq(0,1,0.001)
pfunction <- (p^15)*((1-p)^5)
plot(p, pfunction, ylab = "Likelihood")
abline(v = 15/n, col = "blue", lwd = 3)

#MLE
loglik <- function(p) {-(15*log(p)+5*log(1-p))} 
nlm(loglik, p = 0.5)$estimate

#####Poisson#####
#plot
n <- 12
x <- c(1,4,3,3,0,2,3,2,1,1,2,0)
lambda <- seq(0, 4, 0.001)
logfunction <- -n*lambda+log(lambda)*sum(x)-sum(log(factorial(x)))
plot(lambda, logfunction, ylab = "log-likelihood")
abline(v = sum(x)/n, col = "blue", lwd = 3)

#MLE
loglik <- function(lambda) {-(-n*lambda+log(lambda)*sum(x)-sum(log(factorial(x))))}
nlm(loglik, p = 0.5)$estimate

library(MASS)
fitdistr(x, densfun = "Poisson")


#####normal#####
###mean
#plot
n <- 15
x <- c(62,54,63,63,64,66,69,63,69,62,68,68,67,65,70)
mu <- seq(min(x), max(x), 0.001)
logfunction <- (-n/2)*(log(2*pi)+log(var(x)))
for (i in 1:length(x)) {
  logfunction <- logfunction + -(1/2)*(1/var(x))*((x[i]-mu)^2)
}

plot(mu, logfunction, ylab = "log-likelihood")
abline(v = sum(x)/n, col = "blue", lwd = 3)

#MLE
loglik <- function(mu) {
  logfunction <- (-n/2)*(log(2*pi)+log(var(x)))
  for (i in 1:length(x)) {
    logfunction <- logfunction + -(1/2)*(1/var(x))*((x[i]-mu)^2)
  }
  return((-1)*logfunction)
}
nlm(loglik, p = 0.5)$estimate

fitdistr(x, densfun = "normal")
