#############################################################
#                                                           #
#        L.L.N. by 至煒                                     #
#############################################################
rm(list=ls(all=T))

#############################################
#####simulation-1

par(mfcol=c(3,2))
ns=c(5, 500, 50000)
for (v in 2:1){
  for(n in ns){
    xbah=numeric(1000)
    for(i in 1:1000){
      x=rt(n,v)
      xbah[i]=mean(x)
    }
    hist(xbah,main=paste("從t(",v,")分配抽樣n=", n, "的樣本平均數分配"))
  }
}

#############################################
#####simulation-2

n              = 10000
set.seed(1013)
chi_outcome    = rchisq (n, df=10)
set.seed(1013)
cauchy_outcome = rcauchy(n,location=10, scale=100)

index          = 1:n
chi_m          = NULL
cauchy_m       = NULL
for(i in index){
  chi_m    = c(chi_m   , mean(   chi_outcome[1:i]))
  cauchy_m = c(cauchy_m, mean(cauchy_outcome[1:i]))
}

par(mfcol=c(1,2))
plot(chi_m   , type='l', col='blue', main='chi-square(10)', ylab='mean', ylim=c(0,20))
abline(h=10, col='gray')
plot(cauchy_m, type='l', col='blue', main='cauchy(10,100)', ylab='mean', ylim=c(-400,400))
abline(h=10, col='gray')



#############################################################
#                                                           #
#        C.I.   by 至煒                                     #
#############################################################
rm(list=ls(all=T))

#############################################
####例一

mu = 21.2
sigma = 8
confidence = 0.95; alpha = 1 - confidence
n = 100

mu - qnorm(1-alpha/2)*(sigma/sqrt(n)) #L
mu + qnorm(1-alpha/2)*(sigma/sqrt(n)) #U

#############################################
####例二

mu = 46
sigma = 5.7
confidence = 0.86; alpha = 1 - confidence
n = 16

mu - qt(1-alpha/2, n-1)*(sigma/sqrt(n)) #L
mu + qt(1-alpha/2, n-1)*(sigma/sqrt(n)) #U

#############################################
####例三

sigma = 5
confidence = 0.95; alpha = 1 - confidence
n = 16

((n-1)*sigma^2)/qchisq(1-alpha/2, n-1) #L
((n-1)*sigma^2)/qchisq(alpha/2, n-1)   #U

sqrt(((n-1)*sigma^2)/qchisq(1-alpha/2, n-1)) #L
sqrt(((n-1)*sigma^2)/qchisq(alpha/2, n-1))   #U

#############################################
####例四

s  = c(202,201,196,194)
mu = 200 
var = mean((s - mu)^2)
confidence = 0.95; alpha = 1 - confidence
n = length(s)

(n*var/qchisq(1-alpha/2, n)) #L
(n*var/qchisq(alpha/2, n))   #U



























