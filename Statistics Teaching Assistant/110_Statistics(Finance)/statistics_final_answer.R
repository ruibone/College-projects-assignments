##### 5.1.b. #####
n <- 1
prob <- 1/2
x <- 0:n
y <- dbinom(0:n, n, prob)
plot(x, y, xlab = 'x', ylab = 'fx', xlim = c(-0.5, 1.5), ylim = c(0, 1), 
     main = 'pdf of W0 (~Ber(p = 0.5))', pch = 16)
for(i in 0:1){
  segments(i, 0, i, dbinom(i, n, prob), lwd = 8)  
}

##### 5.2.b. #####
n <- 2
prob <- 2/3
x <- seq(-0.5, 2.5, 0.001)
y <- pbinom(x, n, prob)
plot(x, y, type = 'l', xlab = 'N', ylab = 'probability', main = 'cdf')

##### 5.2.c.2. #####
N <- 3
n <- 2
k <- 2
dhyper(1, n, N-n, k)

##### 5.3.a. #####
prob = 2/3
fail <- 3
1 - pgeom(fail-1, prob) 

##### 5.4. #####
n <- 5
success <- 2 
prob <- 2/3 
dnbinom(n-success, success, prob) 

##### 5.5.b.1. #####
n <- 100
q <- 0.01
dbinom(99, n, 1-q)

##### 5.5.b.2. #####
mu <- n*(1-q)
sigma <-  sqrt(n*(1-q)*q)
pnorm(99+0.5, mu, sigma) - pnorm(99-0.5, mu, sigma)