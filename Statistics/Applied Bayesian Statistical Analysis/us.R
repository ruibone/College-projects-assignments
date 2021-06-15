modelz="
model{
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
  }
  
  mu = X[,1]*beta[1] + X[,2]*beta[2]*z2 + X[,3]*beta[3]*z3 + X[,4]*beta[4]*z4 + X[,5]*beta[5]*z5 + X[,6]*beta[6]*z6 + X[,7]*beta[7]*z7 + X[,8]*beta[8]*z8 + X[,9]*beta[9]*z9 + X[,10]*beta[10]*z10 + X[,11]*beta[11]*z11 + X[,12]*beta[12]*z12
  
  beta[1:M] ~ dmnorm(mu_beta[1:M], tau_beta[1:M, 1:M])
  sigma ~ dgamma(1/2, Var_Y/2)
  tau = 1/sigma
  z2 ~ dbin(0.5,1)
  z3 ~ dbin(0.5,1)
  z4 ~ dbin(0.5,1)
  z5 ~ dbin(0.5,1)
  z6 ~ dbin(0.5,1)
  z7 ~ dbin(0.5,1)
  z8 ~ dbin(0.5,1)
  z9 ~ dbin(0.5,1)
  z10 ~ dbin(0.5,1)
  z11 ~ dbin(0.5,1)
  z12 ~ dbin(0.5,1)
}"

model="
model{
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
  }
  
  mu = X%*%beta
  
  beta[1:M] ~ dmnorm(mu_beta[1:M], tau_beta[1:M, 1:M])
  sigma ~ dgamma(1/2, Var_Y/2)
  tau = 1/sigma
}"


writeLines(model, con="regression.bug") 
writeLines(modelz, con="regressionz.bug") 


us <- read.csv("C:/Users/Darui Yen/OneDrive/桌面/us_poll_data.csv", header = T, sep = ",")
us <- cbind(State = us$State,  region = us$region, n = rep(0, nrow(us)), m = rep(0, nrow(us)), s = rep(0, nrow(us)),
            as.data.frame(apply(us[,-c(1,14)], 2, scale))) #標準化
for (i in 1:nrow(us)) {
  if (us$region[i] == "n") {
    us$n[i] = 1
  } else if (us$region[i] == "m") {
    us$m[i] = 1
  } else if (us$region[i] == "s") {
    us$s[i] = 1
  }
} #地區轉成dummy variable
us <- us[-9,-c(2,6,10,15:16,20:23)] #篩選變數

us11 <- us[c(which(us$voteT.B > 0.75)),]
us12 <- us[c(which(us$voteT.B < -0.75)),]#選取特別

X1 <- as.matrix(cbind(rep(1, nrow(us)), us[,-c(1,13,14)])) #包含地區
X2 <- as.matrix(cbind(rep(1, nrow(us)), us[,-c(1:4,13,14)])) #不包含地區
Y1 <- us$voteT 
Y2 <- us$voteT.B
 

# g-prior for beta
k = 100
mu_beta = rep(0, ncol(X1))
Var_Y = var(Y2)
Sigma_beta = k*Var_Y*solve(t(X1)%*%X1)

uslist <- list(
  Y=Y2, X=X1, N=nrow(us), M=ncol(X1),
  mu_beta=mu_beta, tau_beta=solve(Sigma_beta),
  Var_Y = Var_Y
)


library(rjags)
jags <- jags.model(file = "regressionz.bug", data = uslist, n.chains = 3)
update(jags, n.iter = 1000, progress.bar = "text")
out=coda.samples(jags, variable.names = c("beta","sigma","z2","z3","z4","z5","z6","z7","z8","z9","z10","z11","z12"), 
                 n.iter=10000,thin=5)


traceplot(out)
densplot(out)
autocorr.plot(out)
summary(out)



###marginal propability###
zmargin1 <- as.matrix(out[1][,14:24])
zmargin2 <- as.matrix(out[2][,14:24])
zmargin3 <- as.matrix(out[3][,14:24])
zmargin <- rbind(zmargin1, zmargin2, zmargin3)
zvector <- unique(zmargin, MARGIN = 1)
zprob <- rep(0, 11)

for(i in 1:11) {
  zprob[i] <- sum(apply(zmargin, 1, identical, y=zvector[i,]))
}
zprob <- zprob/sum(zprob)

marginal <- cbind(zvector, zprob)[rev(order(zprob)),]
marginal <- as.data.frame(marginal)
colnames(marginal) <- c("test%","bachelor","unemployment","north","midwest","south","white","black","asian","prevalence",
                        "death%","marginal")

###表格化
library(formattable)
formattable(as.data.frame(marginal[1:5,])) 

###分別找出亞裔、學歷、失業率最好和最差的五個州進行比較
asian <- cbind(usa[order(usa$Non.Hispanic.Asian.ratio, decreasing = T)[c(1:5,46:50)],c(1,5,16)], 
      us[order(us$Non.Hispanic.Asian.ratio, decreasing = T)[c(1:5,46:50)],14])
colnames(asian)[4] <- c("scaled_vote")
formattable(asian)

bachelor <- cbind(usa[order(usa$PercentBachelorsOrHigher, decreasing = T)[c(1:5,46:50)],c(1,10,16:18)]) 
formattable(bachelor)

unemployment <- cbind(usa[order(usa$Percent, decreasing = T)[c(1:5,46:50)],c(1,13,16)], 
               us[order(us$Percent, decreasing = T)[c(1:5,46:50)],14])
colnames(unemployment)[c(2,4)] <- c("unemployment_difference","scaled_vote")
formattable(unemployment)
