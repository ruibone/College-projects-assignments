###problem2
t <- c(-1,0,1,2)
plot(t, t^2+(1/5)*t+(9/10), type = "l", main = "Problem 2")

###problem3
curve((1/4)+(5/pi)*cos(x)-(5/pi)*sin(x)-(5/pi)*sin(2*x), 0, 2*pi, xaxt = "n", main = "Problem 3")
axis(1, at = c(0, pi/2, pi , (3*pi)/2, 2*pi))
lines(c(0, (3/2)*pi), c(-1,-1))
lines(c((3/2)*pi,2*pi), c(4,4))

###problem4
