#############################################################
#                                                           #
#        Sample & Plot by ���m                              #
#############################################################
rm(list=ls(all=T))

data = quakes 
head(data, 7)
data$depth
#The data set give the locations of 1000 seismic events of MB > 4.0.
#The events occurred in a cube near Fiji since 1964.

######################################################
##### sample() �q�V�q���H���������

#���ᤣ��^(���|����P�@��)
sample(data$depth, 100, replace = FALSE)
#�����^(���\����P�@��)
sample(data$depth, 100, replace = TRUE)

##### �qdataframe�H�����o�㵧���
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

######################################################
##### set.seed() �T�w�H��������覡i.e.���A�H��

#seed_66
set.seed(66)
sample(data$depth, 100, replace = FALSE)
sample(data$depth, 100, replace = TRUE)
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

#seed_1013
set.seed(1013)
sample(data$depth, 100, replace = FALSE)
sample(data$depth, 100, replace = TRUE)
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

######################################################
##### Hist �����

hist(data$long)
hist(data$long, main='������ϼ��D', freq=FALSE)

######################################################
##### Pie  ����

time_spend = c('�|�p'       = 2, 
               '�έp'       = 2.5, 
               '�^��'       = 1, 
               '�]��'       = 1.5, 
               '����'       = 5, 
               '�o�b'       = 2,
               '�l�@��game' = 2,
               '��ı'       = 8)
pie(time_spend)

pct  = round(time_spend / sum(time_spend) * 100)
lbls = paste(names(time_spend), pct, "%")
pie(time_spend, main = '�����ϼ��D', lbls)

######################################################
##### Plot ���G��

plot(data$long, data$depth, main = '�����G�ϼ��D')
plot(data, main = '�U�ܼƤ��G��')

######################################################
##### Plot ��ƹ�

#�`�A����pdf
f1 = function(x){
  mu       = 60
  variance = 400
  return((1/(2*pi*variance)^(1/2))*exp(-((x-mu)^2)/(2*variance)))
}

#(1/x)*sin(x)
f2 = function(x){
  return((1/x)*sin(x))
}

#log(x, 10) + 0.05*sin(x)
f3 = function(x){
  return(log(x, 10) + 0.05*sin(x))
}

#�гy�ܱK��x�b�ƭȡA���I�X��x�ȩҹ�������ƭ�(type = 'l' : �H�u�s���۾F�����I)
x = seq(0, 100, 0.005)
plot(x, f1(x), type = 'l')
plot(x, f2(x), type = 'l')
plot(x, f3(x), type = 'l')

######################################################
##### Boxplot �c����

boxplot(data$lat, main = '���c���ϼ��D')

######################################################
##### �����k�褧ø��

dev.off()









