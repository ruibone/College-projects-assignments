sim=1000; N=1200; m=1; n=N/m; Vc=9; Vs=16; Ve=100; alpha=0.2
Isel <- function(sim, m=1, n=1200, Vc=9, Vs=16, Ve=100, alpha=0.2){
  gc <- rep(rnorm(m, mean=0, sd=sqrt(Vc)), each=n)
  gs <- rnorm(m*n, sd=sqrt(Vs))
  g <- gc+gs
  e <- rnorm(m*n, sd=sqrt(Ve))
  p <- g+e
  pi <- rep(tapply(p, gl(m, n), mean), each=n)
  bs <- coef(lm(g ~ pi + p))[c(2,3)] #output
  opt.b <- n*Vc*Ve/(Vs*(n*Vc+Vs+Ve))
  b <- c(seq(0, 15), opt.b)
  s.gain <- vector("numeric", length(b))
  s.diff <- vector("numeric", length(b))
  Ig.cor <- vector("numeric", length(b))
  nsel <- round(m*n*alpha)
  for (j in 1:length(b)) {
    I <- b[j]*pi+p
    Ig.cor[j] <- cor(I, g)
    s.idx <- order(I, decreasing = TRUE)[1:nsel]
    s.gain[j] <- mean(g[s.idx])
    s.diff[j] <- mean(p[s.idx])
  }
  Vpop <- c(var(p), var(g))
  Mpop <- c(mean(p), mean(g))
  return(list(Vpop=Vpop, Mpop=Mpop, bs=bs, Ig.cor=Ig.cor, s.gain=s.gain, s.diff=s.diff))
}




sim.out <- lapply(1:1000, Isel, m=m, n=n)
exp.V <- rowMeans(simplify2array(lapply(sim.out, function(x) x$Vpop)))
exp.M <- rowMeans(simplify2array(lapply(sim.out, function(x) x$Mpop)))
exp.bs <- rowMeans(simplify2array(lapply(sim.out, function(x) x$bs)))
exp.gain <- rowMeans(simplify2array(lapply(sim.out, function(x) x$s.gain)))[17]
exp.diff <- rowMeans(simplify2array(lapply(sim.out, function(x) x$s.diff)))[17]
exp.cor <- rowMeans(simplify2array(lapply(sim.out, function(x) x$Ig.cor)))
exp.b <- exp.bs[1]/exp.bs[2]

exp.M
exp.V

rowMeans(simplify2array(lapply(sim.out, function(x) x$s.gain)))[17]
rowMeans(simplify2array(lapply(sim.out, function(x) x$s.diff)))[17]

sqrt(rowVars(simplify2array(lapply(sim.out, function(x) x$s.gain))))[17]
sqrt(rowVars(simplify2array(lapply(sim.out, function(x) x$s.diff))))[17]

exp.b

