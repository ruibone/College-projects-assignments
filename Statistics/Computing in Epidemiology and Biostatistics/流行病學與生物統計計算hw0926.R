mid <- c( 11,16,23,31,36,47,50)
fin <- c( 3,9,16,20,27,31,36,49,50)
both <- mid[mid %in% fin] 
both[both%%2 == 1] #(1)
both[both%%2 == 0] #(2)
leftside <- mid[!(mid %in% fin)]
rightside <- fin[!(fin %in% mid)]
leftside[leftside%%2 == 1]
rightside[rightside%%2 == 0]

######################################

BetaEstimate <- function(response, predictor){
  y <- response
  x <- matrix(c(rep(1,length(predictor)), predictor), length(predictor), 2)
  beta <- solve(t(x)%*%x)%*%t(x)%*%y
  beta0hat <- beta[1,1]
  beta1hat <- beta[2,1]
  sprintf("\u03B2\u03020 = %f , \u03B2\u03021 = %f", beta0hat, beta1hat)
}

seisure <- read.csv("C:/Users/Darui Yen/OneDrive/®à­±/seizure.csv", header = T)

BetaEstimate(seisure$y,seisure$ltime)
