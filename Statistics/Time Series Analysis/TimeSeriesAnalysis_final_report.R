library(TSA)
library(dplyr)
library(astsa)
origin = read.csv('C:/Users/user/Desktop/data.csv', sep = ',', header = T)
wind = origin

########## date preprocessing ##########
time_split = strsplit(wind$dt, ' ')
year = month = day = rep(0, nrow(wind))
for (i in 1:nrow(wind)){
  temp_split = strsplit(time_split[[i]][1], '-')
  year[i] = temp_split[[1]][1]
  month[i] = temp_split[[1]][2]  
  day[i] = temp_split[[1]][3] 
}
wind$year = as.integer(year)
wind$month = as.integer(month)
wind$day = as.integer(day)
wind$dt = NULL
wind$MW = as.numeric(wind$MW)

########## total electricity generated ###########
year_sum = wind %>% 
  group_by(year) %>%
  summarise(sum_mw = sum(MW))
y_name = pull(year_sum[1])
year_mw = pull(year_sum)
names(year_mw) = y_name

month_sum = wind %>%
  group_by(year, month) %>%
  summarise(sum_mw = sum(MW))
ym_name = paste(pull(month_sum[1]), pull(month_sum[2]), sep = '-')
month_mw = pull(month_sum)
names(month_mw) = ym_name

barplot(month_mw, col = 'blue', xlab = 'month', ylab = 'volume', main = 'Wind Energy generated between 2011-2021')

########## identify time series model (p, d, q) ##########
target = month_mw
names(target) = NULL
train = target[1:120]
test = target[12:132]
plot(month_mw, type = 'l', main = 'aa')

diff_wind = diff(target)
plot(diff_wind, type = 'l', xlab = 'month', ylab = 'volume', main = 'wind energy generated (diff = 1)')
acf(diff_wind, main = 'ACF of wind energy generated (diff = 1)') #ma1?
pacf(diff_wind, main = 'PACF of wind energy generated (diff = 1)') #ar1?
eacf(diff_wind) # ar1 or ma1 or arma1

sarima(target, p = 1, d = 1, q = 0, P = 0, D = 0, Q = 0, S = -1) #aic=32.22, bic=32.29, best
sarima(target, p = 0, d = 1, q = 1, P = 0, D = 0, Q = 0, S = -1) #aic=32.23, bic=32.30
sarima(target, p = 1, d = 1, q = 1, P = 0, D = 0, Q = 0, S = -1) #aic=32.24, bic=32.330, theta1 not significant

########## seasonality & residual ##########
sdiff_wind = diff(diff_wind, lag = 12)
plot(sdiff_wind, type = 'l', xlab = 'month', ylab = 'volume', main = 'wind energy generated (diff = 1 & 12)')
acf(sdiff_wind, main = 'ACF of wind electricity (diff = 12)') #ma1?
pacf(sdiff_wind, main = 'PACF of wind electricity (diff = 12)') #ar1?
eacf(sdiff_wind) # ma1

sarima(target, p = 1, d = 1, q = 0, P = 0, D = 0, Q = 0, S = 12) #aic=32.22, bic=32.29, bad in residual & ljung-box
sarima(target, p = 0, d = 1, q = 1, P = 0, D = 0, Q = 0, S = 12) #aic=32.23, bic=32.30, slightly better in ljung-box
sarima(target, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 0, S = 12) #aic=32.69, bic=32.74
sarima(target, p = 1, d = 1, q = 0, P = 0, D = 0, Q = 1, S = 12) #aic=32.20, bic=32.29
sarima(target, p = 1, d = 1, q = 0, P = 1, D = 0, Q = 1, S = 12) #aic=32.14, bic=32.25
