setwd("C:/Users/Darui Yen/OneDrive/орн▒")
library("rjags")
library("R2OpenBUGS")

data <- list(r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10,   8, 10,   8, 23, 0,  3, 22, 15, 32, 3),
             n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
             x1 = c(0,   0,  0,   0,   0, 0,   0,   0,  0,   0,   0,  1,   1,   1,   1, 1,   1,  1,   1,   1, 1),
             x2 = c(0,   0,  0,   0,   0, 1,   1,   1,  1,   1,   1,  0,   0,   0,   0, 0,   1,  1,   1,   1, 1),
             N = 21)

cat("model{
  for (i in 1:N){
    b[i] ~ dnorm(0, tau)
    x12[i] <- x1[i]*x2[i]
    logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] + alpha12*x12[i] + b[i]
    r[i] ~ dbin(p[i], n[i])
  }
  alpha0 ~ dnorm(0, 1.0E-5)
  alpha1 ~ dnorm(0, 1.0E-5)
  alpha2 ~ dnorm(0, 1.0E-5)
  alpha12 ~ dnorm(0, 1.0E-5)
  tau ~ dgamma(1.0E-3, 1.0E-3)
  sigma <- 1/sqrt(tau)
  }",
  file = "model.txt"
)

rmodel <- function(){
  for (i in 1:N){
    b[i] ~ dnorm(0, tau)
    x12[i] <- x1[i]*x2[i]
    logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] + alpha12*x12[i] + b[i]
    r[i] ~ dbin(p[i], n[i])
  }
  alpha0 ~ dnorm(0, 1.0E-5)
  alpha1 ~ dnorm(0, 1.0E-5)
  alpha2 ~ dnorm(0, 1.0E-5)
  alpha12 ~ dnorm(0, 1.0E-5)
  tau ~ dgamma(1.0E-3, 1.0E-3)
  sigma <- 1/sqrt(tau)
}

initial <- function(){
  list(alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha12 = 0, tau = 1, b = rep(0.1,21))
}

parameters <- c("alpha0", "alpha1", "alpha2", "alpha12", "sigma")

#OpenBUGS
res <- bugs(data, initial, model.file = rmodel, parameters, n.chain = 1, n.iter = 10000, n.burnin = 2000, n.thin = 10,
            debug = T)
#jags
jag <- jags.model("model.txt", data, initial, n.chains = 1, quiet=FALSE)
result <- coda.samples(jag, parameters, n.iter = 10000, thin = 10)


#####mixed normal distribution
load.module("mix")

score <- read.delim("score.txt", header = F)
data <- list(x = as.vector(t(score)), N = nrow(score))
initial <- list(mu1 = 40, mu2 = 60, pi1 = rep(0.5, nrow(score)))


cat("model{
  for (i in 1:N) {
    pi1[i] ~ dbeta(1.5, 1.5)
    pi2[i] <- 1 - pi1[i]
    x[i] ~ dnormmix(c(mu1, mu2), c(1/100, 1/225), c(pi1[i], pi2[i]))
  }
  mu1 ~ dnorm(30, 1/100)
  mu2 ~ dnorm(70, 1/100)
  
}",
    file = "mixed.txt")

run <- jags.model("mixed.txt", data = data, inits = initial, n.chains = 2)
#update(run, n.iter = 10000)

samples <- coda.samples(run, variable.names=c("pi1", "mu1", "mu2"), n.iter=10000)
summary(samples)
plot(samples)
