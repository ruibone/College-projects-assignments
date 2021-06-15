wine <- read.csv("C:/Users/Darui Yen/OneDrive/桌面/探索式多變量分析期末報告/winequalityN.csv", header = T, sep = ",")

###preprocessing###
#NA row
narow <- which(is.na(rowMeans(wine[,2:12])) == 1)
wine <- wine[-c(narow),]

#multivariate outlier

D2 <- mahalanobis(wine[,2:12], colMeans(wine[,2:12]), cov(wine[,2:12]))
multiout <- as.numeric(names(D2[D2 > 3*11]))

wine <- wine[-multiout,]


###distribution plot###
library(ggplot2)

for (i in 2:12) {
  p <- ggplot(wine, aes(x = wine[,i], fill = type)) + geom_density(alpha = 0.3) + ggtitle(colnames(wine)[i])
  print(p)
}


ggplot(wine, aes(x = wine[,13], fill = type)) + geom_histogram(position = "dodge", binwidth = 1, alpha = 0.5) + 
  ggtitle(colnames(wine)[13]) + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


###univariate outlier###

standwine <- matrix(0, nrow(wine), 11)
outlier <- list(NA)

for (i in 2:12) {
  standwine[,(i-1)] <- abs(scale(wine[,i]))
  outlier[[i-1]] <- which(standwine[,(i-1)] > 4)
}
names(outlier) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11")
outlier


###correlation plot###
library(corrplot)

corrplot(cor(linewine), method = "color", type = "upper", order = "hclust", addCoef.col = "black")

###transformation###
library(car)
library(MASS)
library(bestNormalize)

transwine <- wine[,-1]
for (i in 2:12) {
  temp <- bestNormalize(wine[,i], allow_orderNorm = F)$x.t
  transwine[,i-1] <- temp 
}

par(mfrow = c(3,4))
qqPlot(transwine[,1], lwd = 1, id = F, envelope = F, main = colnames(transwine)[1])
qqPlot(transwine[,2], lwd = 1, id = F, envelope = F, main = colnames(transwine)[2])
qqPlot(transwine[,3], lwd = 1, id = F, envelope = F, main = colnames(transwine)[3])
qqPlot(transwine[,4], lwd = 1, id = F, envelope = F, main = colnames(transwine)[4])
qqPlot(transwine[,5], lwd = 1, id = F, envelope = F, main = colnames(transwine)[5])
qqPlot(transwine[,6], lwd = 1, id = F, envelope = F, main = colnames(transwine)[6])
qqPlot(transwine[,7], lwd = 1, id = F, envelope = F, main = colnames(transwine)[7])
qqPlot(transwine[,8], lwd = 1, id = F, envelope = F, main = colnames(transwine)[8])
qqPlot(transwine[,9], lwd = 1, id = F, envelope = F, main = colnames(transwine)[9])
qqPlot(transwine[,10], lwd = 1, id = F, envelope = F, main = colnames(transwine)[10])
qqPlot(transwine[,11], lwd = 1, id = F, envelope = F, main = colnames(transwine)[11])
qqPlot(transwine[,12], lwd = 1, id = F, envelope = F, main = colnames(transwine)[12])
par(mfrow = c(1,1))
shapiro.test(transwine$chlorides[sample(1:nrow(wine), 3000)])


###pca###
library(rgl)

pca <- princomp(wine[,-c(1,13)], scores = T, cor = T)
frame <- as.data.frame(cbind(pca$scores[,1:2], as.factor(wine$type)))
colnames(frame) <- c("PC1", "PC2", "type")
frame$type <- as.factor(frame$type)
ggplot(frame, aes(x = PC1, y = PC2, col = type)) + geom_point() + ggtitle("PCA") + xlab("PC1 (27.4%)") + 
  ylab("PC2 (22.7%)")

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

rgl_init()
rgl.spheres(pca$scores[,1], pca$scores[,2], pca$scores[,3], r = 0.15, color = rep(c("yellow", "red")
                                                                                 , c(nrow(wwine), nrow(rwine)))) 
axis3d('x', pos=c( NA, 0, 0 ), col = "darkgrey")
axis3d('y', pos=c( 0, NA, 0 ), col = "darkgrey")
axis3d('z', pos=c( 0, 0, NA ), col = "darkgrey")
movie3d(spin3d(axis = c(1, 0, 0)), duration = 10, dir = "C:/Users/Darui Yen/OneDrive/桌面")



###clustering###
library(factoextra)
library(cluster)
library(gridExtra)

kmean <- kmeans(transwine, centers = 2)
group <- ifelse(kmean$cluster == 1, "red", "white")
cate <- as.matrix(table(wine$type, group))
fviz_cluster(kmean, data = transwine, geom = "point", ellipse.type = "norm", main = "K-means Clustering")+
  annotation_custom(tableGrob(cate), xmin = 2, xmax = 4.5, ymin = 5, ymax = 7.5)

kmedoid <- pam(transwine, k = 2)
group2 <- ifelse(kmedoid$cluster == 2, "red", "white")
cate2 <- as.matrix(table(wine$type, group2))
fviz_cluster(kmedoid, data = transwine, geom = "point", ellipse.type = "norm", main = "K-medoid Clustering")+
  annotation_custom(tableGrob(cate2), xmin = 2, xmax = 4.5, ymin = 5, ymax = 7.5)


###linear model###

linewine <- as.data.frame(cbind(wine[,1], transwine))
colnames(linewine)[1] <- "type"
for (i in 1:nrow(linewine)) {
  if (linewine$type[i] == "white") {
    linewine$type[i] <- as.numeric(0)
  }else linewine$type[i] <- as.numeric(1)
}
linewine[,1] <- as.numeric(linewine[,1])

rwine <- subset(linewine, type == 0)[,-1]
wwine <- subset(linewine, type == 1)[,-1]

summary(lm(quality~., data = linewine))
summary(lm(quality~., data = rwine))
summary(lm(quality~., data = wwine))

#forward selection
null <- lm(quality~1, data = linewine)
full <- lm(quality~., data = linewine)
forward <-  step(null, scope=list(lower=null, upper=full), direction="forward", k = log(nrow(linewine)))
backward <- step(full, scope = list(upper=full), direction="backward") 
step <- step(null, scope = list(upper=full), direction="both")
summary(forward)

#lasso
library(glmnet)
lasso <- glmnet(x = as.matrix(linewine[,-13]), y = linewine[, 13], alpha = 1,family = "gaussian")
plot(lasso, xvar = "lambda", main = "LASSO", ylim = c(-2,1))

#cv
index <- sample(1:nrow(wine), 0.8*nrow(wine))
as.matrix(train <- linewine[index,])
test <- linewine[-index,]

train[,1] <- as.numeric(train[,1])
test[,1] <- as.numeric(test[,1])

cvlasso <- cv.glmnet(x = as.matrix(train[,1:12]), y = train[, 13], alpha = 1, family = "gaussian")
lassolam <- cvlasso$lambda.min
plot(lasso, xvar = "lambda", main = "LASSO", ylim = c(-2,1))
abline(v=log(lassolam), col="blue", lty=5.5 )
abline(v = log(cvlasso$lambda.1se), col = "red", lty = 5.5)

#ridge
ridge <- glmnet(x = as.matrix(linewine[,-13]), y = linewine[, 13], alpha = 0, family = "gaussian")
cvridge <- cv.glmnet(x = as.matrix(train[,1:12]), y = train[, 13], alpha = 0, family = "gaussian")
ridgelam <- cvridge$lambda.min
plot(ridge, xvar = "lambda", main = "Ridge", ylim = c(-2,1))
abline(v=log(ridgelam), col="blue", lty=5.5 )
abline(v = log(cvridge$lambda.1se), col = "red", lty = 5.5)
plot(cvridge)

#selection
coef(cvridge, s = "lambda.1se")
finalmodel <- lm(quality~volatile.acidity+density+chlorides+sulphates+pH+alcohol, data = linewine)
summary(finalmodel)

#prediction
lassotest = predict(lasso, s = lassolam, newx = as.matrix(test[, -13]))

#Kaiser-Meyer Olkin (KMO) sampling adequacy statistic
library(psych)
KMO(linewine[,-1])
KMO(linewine[,-c(1,2,4,5,7,8,13)])


###canonical analysis###

