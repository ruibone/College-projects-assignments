n <- 10000 
win <- sample(1:3,n,replace = T)
choose <- sample(1:3,n,replace = T)
sum(win == choose)/n

open <- rep(NA,100)
space <- 1:3

for (i in 1:n){
  if (choose[i] == win[i]){
    open[i] <- sample(space[-choose[i]], 1)
  }else{
    open[i] <- space[-c(choose[i], win[i])]
  }
  choose[i] <- space[-c(choose[i], open[i])]
}
sum(win == choose)/n
