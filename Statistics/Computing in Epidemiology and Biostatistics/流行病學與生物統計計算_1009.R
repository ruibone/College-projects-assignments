done <- FALSE
prime <- c()
i <- 2

while (!done) {
  index <- sum((i%%(1:(i-1))) == 0)
  if (index == 1) prime <- c(prime ,i)
  i <- i + 1
  if (i > 100) done <-TRUE
}

prime
