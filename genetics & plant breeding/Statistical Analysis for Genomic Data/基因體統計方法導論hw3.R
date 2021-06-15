y = scan()
30 32 28 29 29 22 20 21 20 21

trt = scan()
1 1 1 1 0 1 0 0 0 0 

x = matrix(scan(),10,2)
1 1 1 1 1 1 1 1 1 1 
1 1 1 1 0 1 0 0 0 0

b = solve(t(x) %*% x) %*% (t(x) %*% y)
sigma2 = t(y - x %*% b) %*% (y - x %*% b )/10
sigma2
b

out = lm(y~trt)
out
summary(out)
