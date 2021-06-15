setwd("C:/Users/Darui Yen/OneDrive/桌面")
library(readxl)

d20 <- read_excel("321_20200720.xlsx")
d20 <- as.data.frame(d20)
d21 <- read_excel("321_20200721.xlsx")
d21 <- as.data.frame(d21)
d22 <- read_excel("321_20200722.xlsx")
d22 <- as.data.frame(d22)
d23 <- read_excel("321_20200723.xlsx")
d23 <- as.data.frame(d23)
d24 <- read_excel("321_20200724.xlsx")
d24 <- as.data.frame(d24)
d25 <- read_excel("321_20200725.xlsx")
d25 <- as.data.frame(d25)
d26 <- read_excel("321_20200726.xlsx")
d26 <- as.data.frame(d26)
d27 <- read_excel("321_20200727.xlsx")
d27 <- as.data.frame(d27)
d28 <- read_excel("321_20200728.xlsx")
d28 <- as.data.frame(d28)
d29 <- read_excel("321_20200729.xlsx")
d29 <- as.data.frame(d29)
d30 <- read_excel("321_20200730.xlsx")
d30 <- as.data.frame(d30)
d31 <- read_excel("321_20200731.xlsx")
d31 <- as.data.frame(d31)



#####空壓機#####

colMeans(d20[d20$空壓機_單 == d20$空壓機_雙,7:8])
colMeans(d20[d20$空壓機_單 != d20$空壓機_雙,7:8])

colMeans(d21[d21$空壓機_單 == d21$空壓機_雙,7:8])
colMeans(d21[d21$空壓機_單 != d21$空壓機_雙,7:8])


equmean <- c(colMeans(d20[d20$空壓機_單 == d20$空壓機_雙,7:8]), colMeans(d21[d21$空壓機_單 == d21$空壓機_雙,7:8]), 
             colMeans(d22[d22$空壓機_單 == d22$空壓機_雙,7:8]), colMeans(d23[d23$空壓機_單 == d23$空壓機_雙,7:8]), 
             colMeans(d24[d24$空壓機_單 == d24$空壓機_雙,7:8]), colMeans(d25[d25$空壓機_單 == d25$空壓機_雙,7:8]), 
             colMeans(d26[d26$空壓機_單 == d26$空壓機_雙,7:8]), colMeans(d27[d27$空壓機_單 == d27$空壓機_雙,7:8]), 
             colMeans(d28[d28$空壓機_單 == d28$空壓機_雙,7:8]), colMeans(d29[d29$空壓機_單 == d29$空壓機_雙,7:8]), 
             colMeans(d30[d30$空壓機_單 == d30$空壓機_雙,7:8]), colMeans(d31[d31$空壓機_單 == d31$空壓機_雙,7:8]))

nonmean <- c(colMeans(d20[d20$空壓機_單 != d20$空壓機_雙,7:8]), colMeans(d21[d21$空壓機_單 != d21$空壓機_雙,7:8]), 
             colMeans(d22[d22$空壓機_單 != d22$空壓機_雙,7:8]), colMeans(d23[d23$空壓機_單 != d23$空壓機_雙,7:8]), 
             colMeans(d24[d24$空壓機_單 != d24$空壓機_雙,7:8]), colMeans(d25[d25$空壓機_單 != d25$空壓機_雙,7:8]), 
             colMeans(d26[d26$空壓機_單 != d26$空壓機_雙,7:8]), colMeans(d27[d27$空壓機_單 != d27$空壓機_雙,7:8]), 
             colMeans(d28[d28$空壓機_單 != d28$空壓機_雙,7:8]), colMeans(d29[d29$空壓機_單 != d29$空壓機_雙,7:8]), 
             colMeans(d30[d30$空壓機_單 != d30$空壓機_雙,7:8]), colMeans(d31[d31$空壓機_單 != d31$空壓機_雙,7:8]))


plot(as.character(20:31), equmean[seq(1,23,2)], type = "l", ylim = c(0,0.4), col = 1,
     main = "壓縮機單", ylab = "壓縮機啟動比例", xlab = "日期")
lines(as.character(20:31), nonmean[seq(1,23,2)], col = 2)
legend("topright", legend = c("空壓機相等", "空壓機不等"), col = 1:2, lty = 1)

plot(as.character(20:31),equmean[seq(2,24,2)], type = "l", ylim = c(0,0.4), col = 3,
     main = "壓縮機雙", ylab = "壓縮機啟動比例", xlab = "日期")
lines(as.character(20:31),nonmean[seq(2,24,2)], col = 4)
legend("topright", legend = c("空壓機相等", "空壓機不等"), col = 3:4, lty = 1)


pair1 <- rbind(as.vector(table(d20[d20$空壓機_單 == d20$空壓機_雙,7:8])),
              as.vector(table(d21[d21$空壓機_單 == d21$空壓機_雙,7:8])),
              as.vector(table(d22[d22$空壓機_單 == d22$空壓機_雙,7:8])), 
              as.vector(table(d23[d23$空壓機_單 == d23$空壓機_雙,7:8])),
              as.vector(table(d24[d24$空壓機_單 == d24$空壓機_雙,7:8])), 
              as.vector(table(d25[d25$空壓機_單 == d25$空壓機_雙,7:8])),
              as.vector(table(d26[d26$空壓機_單 == d26$空壓機_雙,7:8])), 
              as.vector(table(d27[d27$空壓機_單 == d27$空壓機_雙,7:8])),
              as.vector(table(d28[d28$空壓機_單 == d28$空壓機_雙,7:8])), 
              as.vector(table(d29[d29$空壓機_單 == d29$空壓機_雙,7:8])),
              as.vector(table(d30[d30$空壓機_單 == d30$空壓機_雙,7:8])), 
              as.vector(table(d31[d31$空壓機_單 == d31$空壓機_雙,7:8])))
colnames(pair1) <- c("00", "01", "10", "11")
rownames(pair1) <- 20:31
sumpair1 <- cbind(pair1, "total" = rowSums(pair1))
propair1 <- round(prop.table(pair1, margin = 1), 2)

pair2 <- rbind(as.vector(table(d20[d20$空壓機_單 != d20$空壓機_雙,7:8])),
               as.vector(table(d21[d21$空壓機_單 != d21$空壓機_雙,7:8])),
               as.vector(table(d22[d22$空壓機_單 != d22$空壓機_雙,7:8])), 
               as.vector(table(d23[d23$空壓機_單 != d23$空壓機_雙,7:8])),
               as.vector(table(d24[d24$空壓機_單 != d24$空壓機_雙,7:8])), 
               as.vector(table(d25[d25$空壓機_單 != d25$空壓機_雙,7:8])),
               as.vector(table(d26[d26$空壓機_單 != d26$空壓機_雙,7:8])), 
               as.vector(table(d27[d27$空壓機_單 != d27$空壓機_雙,7:8])),
               as.vector(table(d28[d28$空壓機_單 != d28$空壓機_雙,7:8])), 
               as.vector(table(d29[d29$空壓機_單 != d29$空壓機_雙,7:8])),
               as.vector(table(d30[d30$空壓機_單 != d30$空壓機_雙,7:8])), 
               as.vector(table(d31[d31$空壓機_單 != d31$空壓機_雙,7:8])))
colnames(pair2) <- c("00", "01", "10", "11")
rownames(pair2) <- 20:31
sumpair2 <- cbind(pair2, "total" = rowSums(pair2))
propair2 <- round(prop.table(pair2, margin = 1),2)


#####車速#####

quantile(d21[d21$空壓機_單 != d21$空壓機_雙,]$面板車速)

#####time plot#####
plot(d21[d21$空壓機_單 == d21$空壓機_雙,]$面板車速[1:10000], type = "l", lwd = 0.5)
plot(d21[d21$空壓機_單 != d21$空壓機_雙,]$面板車速[1:10000], type = "l", lwd = 0.5)


#####151-152#####

change24 <- d24[d24$EMUID1 == 151,]
change25 <- d25[d25$EMUID1 == 151,]
change26 <- d26[d26$EMUID1 == 151,]
change28 <- d28[d28$EMUID1 == 151,]
change29 <- d29[d29$EMUID1 == 151,]
change30 <- d30[d30$EMUID1 == 151,]

mat151 <- rbind(colMeans(change24[,5:8]), colMeans(change25[,5:8]), colMeans(change26[,5:8]),
                colMeans(change28[,5:8]), colMeans(change29[,5:8]), colMeans(change30[,5:8]))
rownames(mat151) <- c(24:26,28:30)

matplot(mat151, col = 1:4, lwd = 2, type = "l", main = "before & after", ylab = "value", xlab = "day")
legend(5.2, -3, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)


#####149-150#####

change020 <- d20[d20$EMUID1 == 149,]
change021 <- d21[d21$EMUID1 == 149,]
change022 <- d22[d22$EMUID1 == 149,]
change023 <- d23[d23$EMUID1 == 149,]
change024 <- d24[d24$EMUID1 == 149,]
change025 <- d25[d25$EMUID1 == 149,]
change026 <- d26[d26$EMUID1 == 149,]
change027 <- d27[d27$EMUID1 == 149,]
change028 <- d28[d28$EMUID1 == 149,]
change029 <- d29[d29$EMUID1 == 149,]
change030 <- d30[d30$EMUID1 == 149,]
change031 <- d31[d31$EMUID1 == 149,]

mat149 <- rbind(colMeans(change020[,5:8]), colMeans(change021[,5:8]), colMeans(change022[,5:8]), 
                colMeans(change023[,5:8]), colMeans(change024[,5:8]), colMeans(change025[,5:8]), 
                colMeans(change026[,5:8]), colMeans(change027[,5:8]), colMeans(change028[,5:8]),
                colMeans(change029[,5:8]), colMeans(change030[,5:8]), colMeans(change031[,5:8]))
rownames(mat149) <- 20:31

matplot(mat149, col = 1:4, lwd = 2, type = "l", main = "剛換空壓機", ylab = "value", xlab = "day")
legend(10, -3, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)


#####157-158#####

change20 <- d20[d20$EMUID1 == 157,]
change21 <- d21[d21$EMUID1 == 157,]
change22 <- d22[d22$EMUID1 == 157,]
change23 <- d23[d23$EMUID1 == 157,]
change24 <- d24[d24$EMUID1 == 157,]
change25 <- d25[d25$EMUID1 == 157,]
change26 <- d26[d26$EMUID1 == 157,]
change27 <- d27[d27$EMUID1 == 157,]
change28 <- d28[d28$EMUID1 == 157,]
change29 <- d29[d29$EMUID1 == 157,]
change30 <- d30[d30$EMUID1 == 157,]
change31 <- d31[d31$EMUID1 == 157,]

mat157 <- rbind(colMeans(change20[,5:8]), colMeans(change21[,5:8]), colMeans(change22[,5:8]), 
                colMeans(change23[,5:8]), colMeans(change24[,5:8]), colMeans(change25[,5:8]), 
                colMeans(change26[,5:8]), colMeans(change27[,5:8]), colMeans(change28[,5:8]),
                colMeans(change29[,5:8]), colMeans(change30[,5:8]), colMeans(change31[,5:8]))
rownames(mat157) <- 20:31

matplot(mat157, col = 1:4, lwd = 2, type = "l", main = "三年未換空壓機", ylab = "value", xlab = "day")
matplot(mat157[,3:4], col = 3:4, lwd = 2, type = "l", main = "空壓機", ylab = "value", xlab = "day")
legend(10, -3, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)


#####119-120#####

change0020 <- d20[d20$EMUID1 == 119,]
change0021 <- d21[d21$EMUID1 == 119,]
change0022 <- d22[d22$EMUID1 == 119,]
change0023 <- d23[d23$EMUID1 == 119,]
change0024 <- d24[d24$EMUID1 == 119,]
change0025 <- d25[d25$EMUID1 == 119,]
change0026 <- d26[d26$EMUID1 == 119,]
change0027 <- d27[d27$EMUID1 == 119,]
change0028 <- d28[d28$EMUID1 == 119,]
change0029 <- d29[d29$EMUID1 == 119,]
change0030 <- d30[d30$EMUID1 == 119,]
change0031 <- d31[d31$EMUID1 == 119,]

mat119 <- rbind(colMeans(change0020[,5:8]), colMeans(change0021[,5:8]), colMeans(change0022[,5:8]), 
                colMeans(change0023[,5:8]), colMeans(change0024[,5:8]), colMeans(change0025[,5:8]), 
                colMeans(change0026[,5:8]), colMeans(change0027[,5:8]), colMeans(change0028[,5:8]),
                colMeans(change0029[,5:8]), colMeans(change0030[,5:8]), colMeans(change0031[,5:8]))
rownames(mat119) <- 20:31

matplot(mat119, col = 1:4, lwd = 2, type = "l", main = "三年未換空壓機", ylab = "value", xlab = "day")
matplot(mat119[,3:4], col = 3:4, lwd = 2, type = "l", main = "空壓機", ylab = "value", xlab = "day")
legend(10, -3, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)


#####121-122#####

change00020 <- d20[d20$EMUID1 == 121,]
change00021 <- d21[d21$EMUID1 == 121,]
change00022 <- d22[d22$EMUID1 == 121,]
change00023 <- d23[d23$EMUID1 == 121,]
change00024 <- d24[d24$EMUID1 == 121,]
change00025 <- d25[d25$EMUID1 == 121,]
change00026 <- d26[d26$EMUID1 == 121,]
change00027 <- d27[d27$EMUID1 == 121,]
change00028 <- d28[d28$EMUID1 == 121,]
change00029 <- d29[d29$EMUID1 == 121,]
change00030 <- d30[d30$EMUID1 == 121,]
change00031 <- d31[d31$EMUID1 == 121,]

mat121 <- rbind(colMeans(change00020[,5:8]), colMeans(change00021[,5:8]), colMeans(change00022[,5:8]), 
                colMeans(change00023[,5:8]), colMeans(change00024[,5:8]), colMeans(change00025[,5:8]), 
                colMeans(change00026[,5:8]), colMeans(change00027[,5:8]), colMeans(change00028[,5:8]),
                colMeans(change00029[,5:8]), colMeans(change00030[,5:8]), colMeans(change00031[,5:8]))
rownames(mat121) <- 20:31

matplot(mat121, col = 1:4, lwd = 2, type = "l", main = "三年未換空壓機", ylab = "value", xlab = "day")
matplot(mat121[,3:4], col = 3:4, lwd = 2, type = "l", main = "空壓機", ylab = "value", xlab = "day")
legend(10, -3, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)


#####153-154#####

chan20 <- d20[d20$EMUID1 == 153,]
chan21 <- d21[d21$EMUID1 == 153,]
chan22 <- d22[d22$EMUID1 == 153,]
chan23 <- d23[d23$EMUID1 == 153,]
chan24 <- d24[d24$EMUID1 == 153,]
chan25 <- d25[d25$EMUID1 == 153,]
chan26 <- d26[d26$EMUID1 == 153,]
chan27 <- d27[d27$EMUID1 == 153,]
chan28 <- d28[d28$EMUID1 == 153,]
chan29 <- d29[d29$EMUID1 == 153,]
chan30 <- d30[d30$EMUID1 == 153,]
chan31 <- d31[d31$EMUID1 == 153,]

mat153 <- rbind(colMeans(chan20[,5:8]), colMeans(chan21[,5:8]), colMeans(chan22[,5:8]), 
                colMeans(chan23[,5:8]), colMeans(chan24[,5:8]), colMeans(chan25[,5:8]), 
                colMeans(chan26[,5:8]), colMeans(chan27[,5:8]), colMeans(chan28[,5:8]),
                colMeans(chan29[,5:8]), colMeans(chan30[,5:8]), colMeans(chan31[,5:8]))
rownames(mat153) <- 20:31

matplot(mat153, col = 1:4, lwd = 2, type = "l", main = "兩年未換空壓機", ylab = "value", xlab = "day")
matplot(mat153[,3:4], col = 3:4, lwd = 2, type = "l", main = "空壓機", ylab = "value", xlab = "day")
legend(2, -0.5, legend = c("單", "雙", "單啟動", "雙啟動"), col = 1:4, lty = 1:4, lwd = 2)
