chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  n <- nrow(x)
  p <- ncol(x)
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n-0.5)/n
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
}


setwd("C:/Users/Darui Yen/OneDrive/орн▒")
data <- read.table("hematology.dat", header = F, sep = "")
datcolnames(data) <- c("y1","y2","y3","y4","y5","y6")


###D2###
D2 <- mahalanobis(data, colMeans(data), cov(data))
par(mfrow=c(1,1))
chisplot(as.matrix(data))
abline(1,1)

###normality###
par(mfrow = c(2,3))
qqnorm(data$y1, ylab = "Ordered Observations y1")
qqline(data$y1)
qqnorm(data$y2, ylab = "Ordered Observations y2")
qqline(data$y2)
qqnorm(data$y3, ylab = "Ordered Observations y3")
qqline(data$y3)
qqnorm(data$y4, ylab = "Ordered Observations y4")
qqline(data$y4)
qqnorm(data$y5, ylab = "Ordered Observations y5")
qqline(data$y5)
qqnorm(data$y6, ylab = "Ordered Observations y6")
qqline(data$y6)

###wilk statistics###
n <- nrow(data)
p <- ncol(data)
w <- 1 - (n*(max(D2)))/(n-1)**2 
F <- ((n-p-1)/p)*((1/w)-1)
Fcrit <- qf(0.05, p ,(n-p-1))
if (F >= Fcrit) print("significnat!") else print("not significant!")

#####problem2#####################################################
dat <- read.csv("pottery.csv", header = T, sep = ",")

pairs(~Al2O3+Fe2O3+MgO+CaO+Na2O+K2O+TiO2+MnO+BaO, data = dat, 
      col = c("red","yellow","blue","black","green")[dat$kiln])

###density plot###
library(ggplot2)
library(GGally)
ggplot(dat, aes(Al2O3, Fe2O3)) +  coord_equal() + 
  stat_density2d(aes(fill = ..level..), alpha = .5, data = dat) 

ggpairs(dat[,-10],
        title = "Chemical Comparison", 
        lower = list(continuous = wrap("density", alpha = 0.5), discrete = "blank", combo="blank"), 
        diag = list(discrete="blank", continuous = wrap("densityDiag", alpha = 0.5)), 
        upper = list(combo = "blank", continuous = wrap("points", size = 1, colour = factor(dat$kiln))))
                                                