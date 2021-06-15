modelz="
model{
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
  }
  
  mu = X[,1]*beta[1]*z1 + X[,2]*beta[2]*z2 + X[,3]*beta[3]*z3 + X[,4]*beta[4]*z4
  
  beta[1:M] ~ dmnorm(mu_beta[1:M], tau_beta[1:M, 1:M])
  z1 ~ dbin(0.5,1)
  z2 ~ dbin(0.5,1)
  z3 ~ dbin(0.5,1)
  z4 ~ dbin(0.5,1)
  sigma ~ dgamma(1/2, Var_Y/2)
  tau = 1/sigma
}"

writeLines(modelz, con="regressionz.bug") 

modelstring="
model{
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
  }
  
  mu = X%*%beta
  
  beta[1:M] ~ dmnorm(mu_beta[1:M], tau_beta[1:M, 1:M])
  
  sigma ~ dgamma(1/2, Var_Y/2)
  tau = 1/sigma
}"

writeLines(modelstring, con="regression.bug") 

#data
age = c(23,22,22,25,27,20,31,23,27,28,22,24)
grp = c(0,0,0,0,0,0,1,1,1,1,1,1)
oxygen = c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.4,11.05,0.26,6.51)

X = matrix(nrow = 12, ncol = 4)
X[,1] = 1
X[,2] = grp
X[,3] = age
X[,4] = age*grp
Y = oxygen

# g-prior for beta
k = 12
mu_beta = c(0,0,0,0)
Var_Y = var(Y)
Sigma_beta = k*Var_Y*solve(t(X)%*%X)

seed_germ <- list(
  Y=Y, X=X, N=12, M=4,
  mu_beta=mu_beta, tau_beta=solve(Sigma_beta),
  Var_Y = Var_Y
)
inits <- list(beta=c(0,0,0,0))

library(rjags)
jags <- jags.model(file="regression.bug", data=seed_germ, inits=inits, n.chains=3)
update(jags, n.iter=1000,progress.bar="text")
out=coda.samples(jags, variable.names=c("beta", "sigma"), n.iter=10000,
                 thin=5)


initz <- list(beta=c(0,0,0,0), z1=0, z2=0, z3=0, z4=0)

jags <- jags.model(file="regressionz.bug", data=seed_germ, inits=initz, n.chains=3)
update(jags, n.iter=1000,progress.bar="text")
out=coda.samples(jags, variable.names=c("beta", "sigma", "z1", "z2", "z3", "z4"), n.iter=10000,
                 thin=5)

traceplot(out)
densplot(out)
autocorr.plot(out)
summary(out)

###marginal propability###

zmargin1 <- as.matrix(out[1][,6:9])
zmargin2 <- as.matrix(out[2][,6:9])
zmargin3 <- as.matrix(out[3][,6:9])
zmargin <- rbind(zmargin1, zmargin2, zmargin3)
zvector <- unique(zmargin, MARGIN = 1)
zprob <- rep(0, 3)

for(i in 1:3) {
  zprob[i] <- sum(apply(zmargin, 1, identical, y=zvector[i,]))
}
zprob <- zprob/sum(zprob)

marginal <- cbind(zvector, zprob)[rev(order(zprob)),]


###figure9.4###

be <- as.matrix(out[,1:4])
matr <- matrix(NA, 6000, 12)

for (i in 1:12) {
  matr[,i] <- be[,2] + be[,4]*(i+19)
}

boxplot(matr, outline = F, ylim = c(-5,15), names = 20:31, xlab = "age", ylab = "beta_2 + beta_4*age", main = "boxplot")