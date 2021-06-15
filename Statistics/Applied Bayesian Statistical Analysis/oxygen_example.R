y <- c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)
# y = change in maximal oxygen uptake (positive number = improvement)
x1 <- c(0,0,0,0,0,0,1,1,1,1,1,1)              # 0 = running group, 1 = aerobics group
x2 <- c(23,22,22,25,27,20,31,23,27,28,22,24)  # ages of subjects
x3 <- x1*x2                                   # cross-product term (interaction b/w group and age)

X <- as.matrix( cbind(rep(1, length(x1)), x1, x2, x3) )


# Gill's function to produce posterior medians
# and quantile based credible intervals for the
# regression parameters (based on multivariate-t posterior)

t.ci.table <- function(coefs, cov.mat, level=0.95, degrees=Inf, quantiles=c(0.025, 0.50, 0.975) ) {
  quantile.mat <- cbind(coefs, sqrt(diag(cov.mat)), t(qt(quantiles, degrees) %o% sqrt(diag(cov.mat))) + 
                          matrix(rep(coefs,length(quantiles)), ncol=length(quantiles)) )
  quantile.names <- c("mean", "Std. Error") 
  for (i in 1:length(quantiles)) {
    quantile.names <- c(quantile.names, paste(quantiles[i], "Quantile"))
  }
  dimnames(quantile.mat)[2] <- list(quantile.names)
  return(round(quantile.mat,4))
}


### Noninformative Prior Analysis:

bhat <- solve(t(X) %*% X) %*% t(X) %*% y
sig2hat <- t(y - X %*% bhat) %*% (y - X %*% bhat) / (nrow(X) - ncol(X))
my.cov.mat <- solve(t(X) %*% X) * ((nrow(X)-ncol(X))*sig2hat / (nrow(X) - ncol(X) - 2))[1,1]

# Getting posterior information about the betas:
table.beta.info <- t.ci.table(bhat, my.cov.mat, degrees = nrow(X) - ncol(X))
table.beta.info

# Getting posterior information about sigma^2:

library(TeachingDemos) # loading TeachingDemos package, to use hpd function
library(pscl)  # loading pscl package, to use inverse gamma distribution

my.alpha <- (nrow(X) - ncol(X) - 1)/2
my.beta <- 0.5 * sig2hat * (nrow(X) - ncol(X))

# Point estimate (posterior median here) for sigma^2:

sig.sq.poster.med <- qigamma(0.50, alpha=my.alpha, beta=my.beta)
round(sig.sq.poster.med, 3)

hpd.sig.sq <- hpd(qigamma, alpha=my.alpha, beta=my.beta)
round(hpd.sig.sq, 3)


# Function (from Hoff 2009) to calculate a marginal probability for y:

lpy.X <- function(y,X,g=length(y),nu0=1,s20=try(summary(lm(y~-1+X))$sigma^2,silent=TRUE)) {
  n<-dim(X)[1]; p<-dim(X)[2]
  if(p==0) {Hg<-0; s20<-mean(y^2)}
  if(p>0) {Hg<-(g/(g+1))*X%*%solve(t(X)%*%X)%*%t(X)}
  SSRg<- t(y)%*%( diag(1,nrow=n) - Hg)%*%y
  
  -.5*(n*log(pi)+p*log(1+g)+(nu0+n)*log(nu0*s20+SSRg)-nu0*log(nu0*s20))+lgamma((nu0+n)/2)-lgamma(nu0/2)
}

## The oxygen uptake data:

y <- c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)
# y = change in maximal oxygen uptake (positive number = improvement)
x1 <- c(0,0,0,0,0,0,1,1,1,1,1,1)              # 0 = running group, 1 = aerobics group
x2 <- c(23,22,22,25,27,20,31,23,27,28,22,24)  # ages of subjects
x3 <- x1*x2                                   # cross-product term (interaction b/w group and age)
X <- as.matrix( cbind(rep(1, length(x1)), x1, x2, x3) )

### Starting values for Gibbs Sampler:
z<-rep(1,dim(X)[2])  # starting with z = all 1's (all terms in model)
lpy.c<-lpy.X(y,X[,z==1,drop=FALSE])
S <- 10000  # number of Monte Carlo iterations
Z<-matrix(NA,S,dim(X)[2])

### The Gibbs Sampler:
for(s in 1:S)
{
  for(j in sample(1:dim(X)[2]))
  {
    zp<-z; zp[j] <- 1-zp[j]
    lpy.p<-lpy.X(y,X[,zp==1,drop=FALSE])
    r<- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
    z[j]<-rbinom(1,1,1/(1+exp(-r)))
    if(z[j]==zp[j]) {lpy.c<-lpy.p}
  }
  Z[s,]<-z
}
#########

# Considering all possible subsets:

poss.z.vectors <-  unique(Z,MARGIN=1)
z.probs <- rep(0, times= nrow(poss.z.vectors))

for(i in 1:nrow(poss.z.vectors)) {
  z.probs[i] <- sum(apply(Z,1,identical, y=poss.z.vectors[i,]))
}
z.probs <- z.probs/sum(z.probs)

cbind(poss.z.vectors, z.probs)[rev(order(z.probs)),]

# Considering only certain z vectors (interaction only appearing when both first-order terms appear):
#
poss.z.vectors <- matrix( c(
  1,0,0,0,
  1,1,0,0,
  1,0,1,0,
  1,1,1,0,
  1,1,1,1
), ncol=4, byrow=T)

z.probs <- rep(0, times= nrow(poss.z.vectors))

for(i in 1:nrow(poss.z.vectors)) {
  z.probs[i] <- sum(apply(Z,1,identical, y=poss.z.vectors[i,]))
}
z.probs <- z.probs/sum(z.probs)

cbind(poss.z.vectors, z.probs)[rev(order(z.probs)),]
#


# Rerun analysis, considering all possible subsets ...
