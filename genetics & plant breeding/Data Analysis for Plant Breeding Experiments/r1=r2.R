ratio=matrix(seq(0.1,0.5,by=0.1),nrow=1)
Col=c(8,4,6,3,2)
curve((1-x)^2,from = 0,to = 0.5,type = "l",lty=1,col=1,ylim=c(0,1),
      xlab = "r1 (r in the case of M-Q)",ylab = "probability for selecting correct plants")

curve(expr=(1-x)^2,from = 0,to = 0.5,type = "l",lty=1,col=1,ylim=c(0,1),
      xlab = "r1 (r in the case of M-Q)",ylab = "probability for selecting correct plants")
for(i in 1:5){
  curve(expr=((1-x*2*ratio[i])*(1-x*2*(1-ratio[i]))/
                ((1-x*2*ratio[i])*(1-x*2*(1-ratio[i]))+x*2*ratio[i]*x*2*(1-ratio[i])))^2,
        from = 0,to = 0.5,type = "l",add=T,lty=7-i,col=Col[i])}
text(0.38,0.82,"r1/r2=1/9");text(0.39,0.7,"r1/r2=2/8");text(0.4,0.63,"r1/r2=3/7")
text(0.41,0.57,"r1/r2=4/6");text(0.35,0.49,"r1/r2=5/5");text(0.335,0.4,"M-Q")


#---------------------------------------------------------------------------------------------------------

curve((((1-1*x)*(1-1*x))/((1-1*x)*(1-1*x)+x*1*x*1))**2, 0, 0.5, ylim = c(0,1), col = 2, lwd = 3,
      xlab = "r1", ylab = "Probability for selecting correct plants")
curve((((1-(1/2)*x)*(1-(3/2)*x))/((1-(1/2)*x)*(1-(3/2)*x)+x*(1/2)*x*(3/2)))**2, 0, 0.5,
      ylim = c(0,1), col = 3, lwd = 3, add = T)
curve((((1-(1/3)*x)*(1-(5/3)*x))/((1-(1/3)*x)*(1-(5/3)*x)+x*(1/3)*x*(5/3)))**2, 0, 0.5,
      ylim = c(0,1), col = 4, lwd = 3, add = T)
curve((((1-(1/4)*x)*(1-(7/4)*x))/((1-(1/4)*x)*(1-(7/4)*x)+x*(1/4)*x*(7/4)))**2, 0, 0.5,
      ylim = c(0,1), col = 5, lwd = 3, add = T)
curve((((1-(1/5)*x)*(1-(9/5)*x))/((1-(1/5)*x)*(1-(9/5)*x)+x*(1/5)*x*(9/5)))**2, 0, 0.5,
      ylim = c(0,1), col = 6, lwd = 3, add = T)
legend("topright", legend = c("r1=r2","r1=3*r2","r1=5*r2","r1=7*r2","r1=9*r2"), col = c(2,3,4,5,6),
       lty = 1, lwd = 3, text.col = c(2,3,4,5,6))
