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

###lower bound###
p <- seq(0, 1, 0.001)
fpl <- -0.975
for (k in 0:19) {
  fpl <- fpl + choose(100, k)*(p^k)*((1-p)^(100-k))
}
plot(p, fpl, type = "l")

ftnl <- function(p) {
  fp <- -0.975
  dfp <- 0
  for (k in 0:19) {
    fp <- fp + choose(100, k)*(p^k)*((1-p)^(100-k))
    dfp <- dfp + choose(100, k)*(k*(p^(k-1))*((1-p)^(100-k)) - (p^k)*(100-k)*((1-p)^(99-k)))
  }
  return(c(fp, dfp))
}

newtonraphson(ftnl, 0.2, 1e-9)

###upper bound###
p <- seq(0, 1, 0.001)
fpu <- -0.025
for (k in 0:20) {
  fpu <- fpu + choose(100, k)*(p^k)*((1-p)^(100-k))
}
plot(p, fpu, type = "l")

ftnu <- function(p) {
  fp <- -0.025
  dfp <- 0
  for (k in 0:20) {
    fp <- fp + choose(100, k)*(p^k)*((1-p)^(100-k))
    dfp <- dfp + choose(100, k)*(k*(p^(k-1))*((1-p)^(100-k)) - (p^k)*(100-k)*((1-p)^(99-k)))
  }
  return(c(fp, dfp))
}

interval <- c(newtonraphson(ftnl, 0.2, 1e-9), newtonraphson(ftnu, 0.2, 1e-9))

###asymptotic interval###

phat <- 20/100
low <- phat - qnorm(0.975)*sqrt((phat*(1-phat))/100)
up <- phat + qnorm(0.975)*sqrt((phat*(1-phat))/100)
asymptotic <- c(low, up)

###package###

binom.confint(20, 100, conf.level = 0.95)
