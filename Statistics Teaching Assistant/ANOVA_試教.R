######## ex1 data
a=c(7.541616,7.834254,7.719094,7.557151,6.293025,6.993867,8.532398,6.232903,5.472263)

b=c(10.891238,5.693458,8.405147,8.617932,3.351708,6.472797,9.663088,6.548482,2.907314,3.003679)

######## ex2: one way ANOVA
x=gl(3,4,12,labels = c("A","B","C"))
y=c(7,3,10,4,4,10,6,8,20,24,19,21)
dat <- data.frame(x,y)

fit=lm(y~x)
ano <- aov(fit)


# install.packages("agricolae")
library("agricolae")

lsd <- LSD.test(y=fit,trt="x",alpha = 0.05,console = T)
plot(lsd)

######## ex3: two way ANOVA
x2=gl(3,4,12,labels = c("A","B","C"))
block=gl(3,1,12,labels = c("A","B","C"))
y2=c(7,3,10,4,4,10,6,8,20,24,19,21)

fit2=lm(y2~x2+block)
anova(fit2)

LSD.test(y=fit2,trt="x2",alpha = 0.05,console = T)\

########################
######################## little practice
boxplot(y3~x3)
y3=c(7.66,6.98,7.8,5.26,5.44,5.8,7.41,7.33,7.04,3.51,2.91,3.66)
x3=gl(4,3,12,labels = c("noraml","eva","mixed","CO2"))
fit3=lm(y3~x3)
anova(fit3)
LSD.test(fit3,"x",alpha = 0.05,console = T)



