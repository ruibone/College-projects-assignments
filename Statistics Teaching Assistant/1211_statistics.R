###example_23###

size <- 5
p <- 0.4

dbinom(4, size, p)
pbinom(4, size, p) - pbinom(3, size, p)
1 - pbinom(3, size, p)
pbinom(3, size, p, lower.tail = F)


#plot
par(mfrow = c(1,2))
barplot(dbinom(0:5, size, p), names.arg = 0:5, xlab="N", ylab="p.m.f.", main = "Binomial")
plot(0:5, pbinom(0:5, size, p), type = "l", xlab="N", ylab="c.d.f.", main = "Binomial")


###example_24###

k <- 6
N <- 4 + k
n <- 4

dhyper(1, k, N-k, n)
phyper(1, k, N-k, n) - phyper(0, k, N-k, n)
1 - phyper(3, k, N-k, n)
phyper(3, k, N-k, n, lower.tail = F)

#plot
barplot(dhyper(0:n, k, N-k, n), names.arg = 0:n, xlab="x", ylab="p.m.f.", main = "Hypergeometric")
plot(0:n, phyper(0:n, k, N-k, n), type = "l", xlab="x", ylab="c.d.f.", main = "Hypergeometric")


###example_26###

p <- 0.5

dgeom(2, p)
pgeom(2, p) - pgeom(1, p)
pgeom(3, p)
1 - pgeom(3, p)
pgeom(3, p, lower.tail = F)

#plot
barplot(dgeom(0:10, p), names.arg = 0:10, xlab="x", ylab="p.m.f.", main = "Geometric")
plot(0:10, pgeom(0:10, p), type = "l", xlab="x", ylab="c.d.f.", main = "Geometric")


###example_27###

r <- 2 #success
p <- 0.5

dnbinom(3-r, r, p)
pnbinom(3-r, r, p) - pnbinom(2-r, 2, p)
1 - pnbinom(2-r, r, p)
pnbinom(2-r, r, p, lower.tail = F)

#plot
barplot(dnbinom(0:10, r, p), names.arg = 0:10, xlab="x", ylab="p.m.f.", main = "Negative Binomial")
plot(0:10, pnbinom(0:10, r, p), type = "l", xlab="x", ylab="c.d.f.", main = "Negative Binomial")


###example_28###

lambda <- 2

dpois(3, lambda)
ppois(3, lambda) - ppois(2, lambda)
dpois(0, lambda)
ppois(0, lambda)
1 - ppois(1, lambda*30)
ppois(1, lambda*30, lower.tail = F)

#plot
barplot(dpois(0:10, lambda), names.arg = 0:10, xlab="x", ylab="p.m.f.", main = "Poisson")
plot(0:10, ppois(0:10, lambda), type = "l", xlab="x", ylab="c.d.f.", main = "Poisson")


###example_2###

min = 0
max = 100

dunif(5, min, max)
1 - punif(10, min, max)
punif(10, min, max, lower.tail = F)

#plot
plot(seq(0, 100, 0.01), dunif(seq(0, 100, 0.01), min = 0, max = 100), type = "l", xlab="x", ylab="p.d.f.", main = "Uniform")
plot(seq(0, 100, 0.01), punif(seq(0, 100, 0.01), min = 0, max = 100), type = "l", xlab="x", ylab="c.d.f.", main = "Uniform")


#example_30###

rate <- 1/24

dexp(18, rate)
1 - pexp(18, rate)
pexp(18, rate, lower.tail = F)

#plot
plot(seq(0, 100, 0.01), dexp(seq(0, 100, 0.01), rate), type = "l", xlab="x", ylab="p.d.f.", main = "Exponential")
plot(seq(0, 100, 0.01), pexp(seq(0, 100, 0.01), rate), type = "l", xlab="x", ylab="c.d.f.", main = "Exponential")


###example_28###

rate <- 1/2
shape <- 3 #等待3人上門

dgamma(4, shape, rate)
1 - pgamma(4, shape, rate)
pgamma(4, shape, rate, lower.tail = F)

#plot
plot(seq(0, 30, 0.01), dgamma(seq(0, 30, 0.01), shape, rate), type = "l", xlab="x", ylab="p.d.f.", main = "Gamma")
plot(seq(0, 30, 0.01), pgamma(seq(0, 30, 0.01), shape, rate), type = "l", xlab="x", ylab="c.d.f.", main = "Gamma")


###example(31)

mu <- 66
sigma <- 3

1 - pnorm(63, mu, sigma)
pnorm(63, mu, sigma, lower = F)
qnorm(1-0.84, mu, sigma)
qnorm(0.84, mu, sigma, lower.tail = F)

#plot
plot(seq(51, 81, 0.01), dnorm(seq(51, 81, 0.01), mu, sigma), type = "l", xlab="x", ylab="p.d.f.", main = "Normal")
plot(seq(51, 81, 0.01), pnorm(seq(51, 81, 0.01), mu, sigma), type = "l", xlab="x", ylab="c.d.f.", main = "Normal")


###chi-square distribution###

df = 4
n  = 80

dchisq(0  , df)
pchisq(0  , df)
qchisq(0.6, df)
rchisq(n  , df)

####
par(mfrow = c(1,3))
x_grid = seq(0,20,by=0.005)

plot(x_grid, dchisq(x_grid, df), 
     xlab='x', ylab='p.d.f.',
     col='blue', type='l')

plot(x_grid, pchisq(x_grid, df),
     xlab='N', ylab='c.d.f.',
     col='blue', type='l')

plot(seq(0,1,by=0.005),qchisq(seq(0,1,by=0.005), df),
     xlab='Percent', ylab='Quantile',
     col='blue', type='l')

title("Chi-square", outer = TRUE, cex = 25, line=-2)