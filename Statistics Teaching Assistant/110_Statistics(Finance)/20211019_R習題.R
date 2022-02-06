# 1.
draw = sample(c(0,1), size = 1000, replace = T)
draw > 0.5
plot(draw, type = 'l')

#2.
normal = rnorm(20, 2, 2.5)
mean(normal)
sd(normal)
hist(normal)

#3.
binom = rbinom(200, 10, 0.1)
hist(binom)