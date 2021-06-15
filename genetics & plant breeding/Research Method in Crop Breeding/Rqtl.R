cro <- read.cross("csv", file = "filll_ABH_fin.csv", genotypes = c("A","H","B"), na.strings = ".", map.function = "kosambi")

print(dup <- findDupMarkers(cro, exact.only=F))
cro <- drop.markers(cro, dup)

cro <- drop.markers(cro, rownames(gt[gt$P.value < 0.05/totmar(cro),]))

cro <- calc.errorlod(cro, err=0.05)#非常久先不跑

cro <- est.rf(cro)

plotMap(cro)
