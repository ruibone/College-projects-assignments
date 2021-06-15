#############################################################
#                                                           #
#        Random Variables by ¦ÜÞm                           #
#############################################################
rm(list=ls(all=T))

## You may combine with set.seed() as before
#############################################
########## Discrete R.V.s ###################



###############################
########## Bernoulli & Binomial
size_1 = 1
size_2 = 100
n      = 10
prob   = 0.5 

dbinom(1  , size_1, prob)
pbinom(0.3, size_1, prob)
qbinom(0.5, size_1, prob)
rbinom(n  , size_1, prob)

####
par(mfrow = c(1,3))

plot(0:1, dbinom(0:1, size_1, prob), 
     xlim=c(-0.08,1.08),
     xlab='N', ylab='p.m.f.',
     col='blue')
for(i in 0:1){
  segments(i, 0, i, dbinom(i, size_1, prob), col='blue')  
}

plot(seq(-0.1,1.1,by=0.005), pbinom(seq(-0.1,1.1,by=0.005), size_1, prob),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qbinom(seq(0,1,by=0.005), size_1, prob),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Bernoulli", outer = TRUE, cex = 25, line=-2)
dev.off()

####
par(mfrow = c(1,3))

plot(0:100, dbinom(0:100, size_2, prob), 
     xlim=c(-0.08,100.1),
     xlab='N', ylab='p.m.f.',
     col='blue')
for(i in 0:100){
  segments(i, 0, i, dbinom(i, size_2, prob), col='blue')  
}

plot(seq(-0.1,100.1,by=0.005), pbinom(seq(-0.1,100.1,by=0.005), size_2, prob),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qbinom(seq(0,1,by=0.005), size_2, prob),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Binomial", outer = TRUE, cex = 25, line=-2)
dev.off()


###############################
########## Geometric
prob = 0.05
n    = 64

dgeom(10 , prob)
pgeom(4  , prob)
qgeom(0.5, prob)
rgeom(100, prob)

####
par(mfrow = c(1,3))

plot(0:100, dgeom(0:100, prob), 
     xlim=c(-0.08,100.5),
     xlab='N', ylab='p.m.f.',
     col='blue')
for(i in 0:100){
  segments(i, 0, i, dgeom(i, prob), col='blue')  
}

plot(seq(-0.1,100.5,by=0.005), pgeom(seq(-0.1,100.5,by=0.005), prob),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.00005),qgeom(seq(0,1,by=0.00005), prob),
     xlab='Percent', ylab='Quantile',
     ylim=c(0,200),
     col='blue', type='l')

title("Geometric", outer = TRUE, cex = 25, line=-2)
dev.off()


###############################
########## Negative Binomial
size = 4
prob = 0.086
n    = 77

dnbinom(8  , size, prob)
pnbinom(15 , size, prob)
qnbinom(0.7, size, prob)
rnbinom(n  , size, prob)

####
par(mfrow = c(1,3))

plot(0:120, dnbinom(0:120, size, prob), 
     xlim=c(-0.08,120.5),
     xlab='N', ylab='p.m.f.',
     col='blue')
for(i in 0:120){
  segments(i, 0, i, dnbinom(i, size, prob), col='blue')  
}

plot(seq(-0.1,120.5,by=0.005), pnbinom(seq(-0.1,120.5,by=0.005), size, prob),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qnbinom(seq(0,1,by=0.005), size, prob),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Geometric", outer = TRUE, cex = 25, line=-2)
dev.off()


###############################
########## Poission
lamda = 5
n     = 200

dpois(4 , lamda)
ppois(7 , lamda)
qpois(0.8, lamda)	
rpois(n  , lamda)	

####
par(mfrow = c(1,3))

plot(0:25, dpois(0:25, lamda), 
     xlim=c(-0.08,25.5),
     xlab='N', ylab='p.m.f.',
     col='blue')
for(i in 0:25){
  segments(i, 0, i, dpois(i, lamda), col='blue')  
}

plot(seq(-0.1,25.5,by=0.005), ppois(seq(-0.1,25.5,by=0.005), lamda),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qpois(seq(0,1,by=0.005), lamda),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Poission", outer = TRUE, cex = 25, line=-2)
dev.off()


#############################################
########## continuous R.V.s #################



########## Uniform
max = 20
min = 10
n   = 50

dunif(23.35, min, max)
punif(13   , min, max)
qunif(0.8   , min, max)
runif(n    , min, max)  

####
par(mfrow = c(1,3))
x_grid = seq(min,max,by=0.005)

plot(x_grid, dunif(x_grid, min, max), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, punif(x_grid, min, max),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qunif(seq(0,1,by=0.005), min, max),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Uniform", outer = TRUE, cex = 25, line=-2)
dev.off()


########## Normal / Guassin
mean = 60
sd   = 10
n    = 123

dnorm(90 , mean, sd)
pnorm(95 , mean, sd)
qnorm(0.8, mean, sd)
rnorm(n  , mean, sd)

####
par(mfrow = c(1,3))
x_grid = seq(-10,130,by=0.005)

plot(x_grid, dnorm(x_grid, mean, sd), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, pnorm(x_grid, mean, sd),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qnorm(seq(0,1,by=0.005), mean, sd),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Normal / Guassin", outer = TRUE, cex = 25, line=-2)
dev.off()


########## Lognormal
meanlog = 0
sdlog   = 0.6
n       = 100

dlnorm(2  , meanlog, sdlog)
plnorm(1.1, meanlog, sdlog)
qlnorm(0.8, meanlog, sdlog)
rlnorm(n  , meanlog, sdlog)

####
par(mfrow = c(1,3))
x_grid = seq(-0.05,7,by=0.005)

plot(x_grid, dlnorm(x_grid, meanlog, sdlog), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, plnorm(x_grid, meanlog, sdlog),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qlnorm(seq(0,1,by=0.005), meanlog, sdlog),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Lognormal", outer = TRUE, cex = 25, line=-2)
dev.off()


########## Exponential
lamda = 5
n     = 200

dexp(1  , lamda)
pexp(0.6, lamda)
qexp(0.2, lamda)
rexp(n  , lamda)

####
par(mfrow = c(1,3))
x_grid = seq(-0.05,2,by=0.005)

plot(x_grid, dexp(x_grid, lamda), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, pexp(x_grid, lamda),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qexp(seq(0,1,by=0.005), lamda),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Exponential", outer = TRUE, cex = 25, line=-2)
dev.off()

########## T
df = 4
n  = 80

dt(0  , df)
pt(0  , df)
qt(0.6, df)
rt(n  , df)

####
par(mfrow = c(1,3))
x_grid = seq(-5,5,by=0.005)

plot(x_grid, dt(x_grid, df), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, pt(x_grid, df),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qt(seq(0,1,by=0.005), df),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("T", outer = TRUE, cex = 25, line=-2)
dev.off()

########## Cauchy
mode  = 0
scale = 1.5
n     = 55

dcauchy(0.5     , mode, scale)
pcauchy(0       , mode, scale)
qcauchy(0.999995, mode, scale)
rcauchy(n       , mode, scale)

####
par(mfrow = c(1,3))
x_grid = seq(-10,10,by=0.005)

plot(x_grid, dcauchy(x_grid, mode, scale), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, pcauchy(x_grid, mode, scale),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qcauchy(seq(0,1,by=0.005), mode, scale),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Cauchy", outer = TRUE, cex = 25, line=-2)
dev.off()


