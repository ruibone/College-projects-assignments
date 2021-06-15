ricexx = read.csv("c:/Users/Darui Yen/Desktop/rice0919_allele_2019__10__02__04028.csv", header = T, sep = ",")
rice = ricexx

marker_name = c("sample_name", "RV211", "RV212", "RV213", "RV221", "RV222", "RV223", 
                "RV231", "RV232", "RV233", "RV241", "RV242", "RV243")

for (m in seq(3, 25, by = 2)){
  for (i in c(1:101)){
    if (rice[i,m] == rice[i,m+1]){
      rice[i,m] = rice[i,m] 
      }else{
      rice[i,m] = paste(rice[i,m], rice[i,m+1], sep = ",")
      }
  }
}

for (j in seq(26, 4, by = -2)){
  rice[,j] <- NULL
}

rice[,1] <-NULL
rice <- rice[c(1,2,4,11,6,7,9,3,12,13,5,8,10)]

for (k in c(1:13)){
  colnames(rice)[k] <- marker_name[k]
}

write.table(rice, file = "C:/Users/Darui Yen/Desktop/r08621110_rice_SSR_genotype_assignment.csv",sep = ",", 
            row.names = F)
