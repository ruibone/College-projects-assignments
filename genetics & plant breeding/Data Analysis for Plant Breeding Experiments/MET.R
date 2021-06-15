setwd("C:/Users/Darui Yen/OneDrive/桌面")
library(agridat)
library(gt)
library(rpivotTable)
library(dplyr)
library(car)
load("rice.RData")

out0 = aov(yield~gen+env+gen:env, data=rice.data)
outmatr0 <- as.matrix(anova(out0))
outmatr0 %>%
  as.data.frame() %>%
  mutate(name = rownames(outmatr0)) %>%
  gt(rowname_col = "name") %>%
  tab_header(title = "ANOVA table ", subtitle = "rice.data")

gen.env.avg = model.tables(out0, "means")$tables$`gen:env`
gen.avg = apply(gen.env.avg,1,mean)
env.avg = apply(gen.env.avg,2,mean)
d1 = data.frame(y = c(gen.env.avg), 
                gen = gl(nrow(gen.env.avg),1,length(gen.env.avg),labels=rownames(gen.env.avg)),
                env = gl(ncol(gen.env.avg),nrow(gen.env.avg),labels=colnames(gen.env.avg)))

out1 = aov(y~gen+env+gen:env, data=d1)
outmatr1 <- as.matrix(anova(out1))
outmatr1 %>%
  as.data.frame() %>%
  mutate(name = rownames(outmatr1)) %>%
  gt(rowname_col = "name") %>%
  tab_header(title = "ANOVA table ", subtitle = "rice.data") %>%
  tab_source_note("註： 4重複合併取平均")
#-----------------------------------------------------------------------------------------------------------

matr2 <- data.frame(geno = rownames(gen.env.avg), beta = rep(NA,nrow(gen.env.avg)), 
                    "pvalue by hypothesis test" = rep(NA,nrow(gen.env.avg)))
index <- 1
par(mfrow = c(3,4))
for (geno in rownames(gen.env.avg)){
  Finlay.data = data.frame(y = gen.env.avg[geno,], x = env.avg)
  plot(y~x, data = Finlay.data, main = as.character(geno))
  linear <- lm(y ~ x, data = Finlay.data)
  abline(linear)
  mtext(paste("R^2 =",round(as.numeric(summary(linear)[8]),2)), side = 3)
  hypo <- linearHypothesis(linear, c("x = 1"))
  matr2[index,2] <- linear$coefficients[2]
  matr2[index,3] <- hypo$`Pr(>F)`[2]
  index <- index + 1
}
par(mfrow = c(1,1))

gt(matr2[1:10,], rowname_col = "geno")
gt(matr2[11:19,], rowname_col = "geno")

#-----------------------------------------------------------------------------------------------------------

out = c()
for (i in 1:nrow(gen.env.avg)){
  M = apply(gen.env.avg, 2, max)
  g = gl(2,length(M))
  e = gl(length(M),1,2*length(M))
  aovi = anova(lm(c(M,gen.env.avg[i,])~g+e+g:e))
  out = rbind(out, aovi[c('g','g:e'),'Sum Sq'])
}
P = apply(out,1,sum)/length(env.avg)
PGE = out[,2]/(length(env.avg)-1)

Pframe <- data.frame(geno = rownames(gen.env.avg), Pi = P, PGEi = PGE)
Pframe %>%
  gt(rowname_col = "geno") %>%
  tab_header(title = "Superiority Analysis")

alldata = data.frame(d1, x = env.avg[match(d1$env,names(env.avg))])
mr = lm(y~gen + env + gen:x, data=alldata)
anova(mr)

qf(0.95,16,252)*618809
qf(0.95,15,252)*618809

G11.data = data.frame(y = gen.env.avg['G11',],x = env.avg)
G7.data = data.frame(z = gen.env.avg['G7',],w = env.avg)
plot(y~x, data=G11.data, main = "G11 vs. G7", xlab = "enviroment mean", ylab = "yeild", pch = 16)
points(z~w,data=G7.data,col = 2, add = T, pch = 15)
abline(lm(y ~ x, data=G11.data))
abline(lm(z ~ w, data=G7.data), col =2)
legend("topleft", legend = c("G11","G7"), pch = c(15,16), col = c(1,2))
#------------------------------------------------------------------------------------------------------------

aov.out = anova(out0)

g = nrow(gen.env.avg)
s = ncol(gen.env.avg)
Wi2mat = (gen.env.avg-matrix(gen.avg, g, s)-matrix(env.avg, g, s, byrow=TRUE)+matrix(mean(gen.avg), g, s))^2
Wi2 = apply(Wi2mat,1,sum)
SSGE = sum(Wi2mat)
p1 = (g*Wi2+SSGE)/(2*(g-1)*(s-1))
p2 = (-g*Wi2+(g-1)*SSGE)/((g-2)*(g-1)*(s-1))
sigmai2 = (g*(g-1)*Wi2+SSGE)/((g-1)*(g-2)*(s-1))
Fstar = sigmai2/aov.out$'Mean Sq'[4]
shuframe <- data.frame("sigma^2" = sigmai2, "F*" = Fstar, geno = rownames(gen.env.avg))
shuframe %>%
  gt(rowname_col = "geno") %>%
  tab_header(title = "Shukla stability variance")

#------------------------------------------------------------------------------------------------------------

N <- gen.env.avg - matrix(env.avg, g, s, byrow=TRUE)
gge.svd <- svd(N)
SSgge <- c()
for (i in 1:15) {
  gge.pcmat = gge.svd$u[,i] %*% t(gge.svd$v[,i]) * gge.svd$d[i]
  SSgge[i] <- round(4*sum((gge.pcmat)^2))
}

biplot(prcomp(N), scale=0, main="biplot   (scale = 0)")
abline(h=0, col="grey")
abline(v=0, col="grey")

#------------------------------------------------------------------------------------------------------------

M <- gen.env.avg-matrix(gen.avg, g, s)-matrix(env.avg, g, s, byrow=TRUE)+matrix(mean(gen.avg), g, s)
svd.out = svd(M)


pcmatr <- data.frame(Df = seq(32,4,-2),"Sum Sq" = rep(NA,15) ,
                    "Mean Sq" = rep(NA,15), "F value" = rep(NA,15), "Pr(>F)" = rep(NA,15))
for (i in 1:15){
  pcimat = svd.out$u[,i] %*% t(svd.out$v[,i]) * svd.out$d[i] 
  pcmatr[i,2] <- round(4*sum((pcimat)^2))
  pcmatr[i,3] <- pcmatr[i,2]/pcmatr[i,1]
}

anotable <- as.data.frame(aov.out)
colnames(pcmatr) <- colnames(anotable)
for (i in 1:15) {
  rownames(pcmatr)[i] <-  paste0("PC",i)
  pcmatr[i,4] <- pcmatr[i,3]/anotable[4,3]
  pcmatr[i,5] <- pf(pcmatr[i,4], df1=pcmatr[i,1], df2=anotable[4,1], lower.tail=FALSE)
}
pcmatr <- pcmatr[,1:5]
anovatable <- rbind(as.data.frame(aov.out[1:3,]), pcmatr, as.data.frame(aov.out[4,]))
anovatable %>%
  mutate(name = rownames(anovatable)) %>%
  gt(rowname_col = "name") %>%
  tab_header(title = "ANOVA table")

precent <- pcmatr[,3]/anotable[4,3]


svd2 = svd(IntMat/1000)

x = c(svd2$u[,1]*svd2$d[1], svd2$v[,1]*svd2$d[1])
y = c(gen.avg,env.avg)/1000

plot(x,y,
     ylab="Scaled Mean Yield", xlab="Scaled PC 1 (38.6%)",
     xaxt="n", ylim = c(2,12),
     pch=16)
for(i in 1:g){
  abline(a=y[i],b=x[i],lwd=ifelse((i %in% c(16,1,5,7,8,6)),2,1))
}
axis(1,at=x[-c(1:g)],labels=colnames(IntMat),las=2,cex.axis=0.5)
abline(v=1.62,lty=2)
abline(v=0.2,lty=2)
abline(v=0.44,lty=2)
abline(v=-0.36,lty=2)
abline(v=-1.03,lty = 2)
                  