library(glmnet)
library(car)
library(bestNormalize)

us <- read.csv("C:/Users/Darui Yen/OneDrive/орн▒/us_poll_data.csv", header = T, sep = ",")

us <- cbind(State = us$State, as.data.frame(apply(us[,-1], 2, scale)))
#us[is.na(us) == 1] <- 0


shapiro.test(us$Hispanic..of.any.race..ratio)#p-value = 6.903e-07
shapiro.test(us$Non.Hispanic.White.ratio)#p-value = 0.0715
shapiro.test(us$Non.Hispanic.Black.ratio)#p-value = 1.084e-05
shapiro.test(us$Non.Hispanic.Asian.ratio)#p-value = 9.668e-12
shapiro.test(us$Non.Hispanic.American.Indian.ratio)#p-value = 1.115e-11
shapiro.test(us$Prevalence)#p-value = 0.04372
shapiro.test(us$Deaths.Ratio)#p-value = 0.002793
shapiro.test(us$Test.Ratio)#p-value = 4.214e-05
shapiro.test(us$PercentBachelorsOrHigher)#p-value = 0.7644
shapiro.test(us$PercentBelowHighSchool)#p-value = 0.0237
shapiro.test(us$PercentHighSchool)#p-value = 0.01619
shapiro.test(us$Percent)#p-value = 7.5e-05
shapiro.test(us$vote)#p-value = 3.141e-08

par(mfrow = c(3,4))
qqPlot(us$Hispanic..of.any.race..ratio, envelope = F)
qqPlot(us$Non.Hispanic.White.ratio, envelope = F)
qqPlot(us$Non.Hispanic.Black.ratio, envelope = F)
qqPlot(us$Non.Hispanic.Asian.ratio, envelope = F)
qqPlot(us$Non.Hispanic.American.Indian.ratio, envelope = F)
qqPlot(us$Prevalence, envelope = F)
qqPlot(us$Deaths.Ratio, envelope = F)
qqPlot(us$Test.Ratio, envelope = F)
qqPlot(us$PercentBachelorsOrHigher, envelope = F)
qqPlot(us$PercentBelowHighSchool, envelope = F)
qqPlot(us$PercentHighSchool, envelope = F)
qqPlot(us$Percent, envelope = F)
qqPlot(us$vote, envelope = F)

qqPlot(bestNormalize(us$Hispanic..of.any.race..ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Non.Hispanic.White.ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Non.Hispanic.Black.ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Non.Hispanic.Asian.ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Non.Hispanic.American.Indian.ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Prevalence)$x.t, envelope = F)
qqPlot(bestNormalize(us$Deaths.Ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$Test.Ratio)$x.t, envelope = F)
qqPlot(bestNormalize(us$PercentBachelorsOrHigher)$x.t, envelope = F)
qqPlot(bestNormalize(us$PercentBelowHighSchool)$x.t, envelope = F)
qqPlot(bestNormalize(us$PercentHighSchool)$x.t, envelope = F)
qqPlot(bestNormalize(us$Percent)$x.t, envelope = F)
qqPlot(bestNormalize(us$vote)$x.t, envelope = F)

par(mfrow = c(1,1))

pairs(us[,-c(1,2,11,12,15:18)])
model1 <- lm(vote~., data = us[,-c(1,2,3,8,9,11,12,15:18)])
vif(model1)

ridge <- glmnet(x = as.matrix(us[,-c(1,2,11,15:18)]), y = us[, 14], alpha = 0, family = "gaussian")
plot(ridge, xvar = "lambda", main = "ridge")
lasso <- glmnet(x = as.matrix(us[,-c(1,2,11,15:18)]), y = us[, 14], alpha = 1, family = "gaussian")
plot(lasso, xvar = "lambda", main = "lasso")

