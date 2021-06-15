##############################################################
# R code for QTL mapping
# Master's course in "Methodolody in Plant Breeding"
# Department of Agronomy, National Taiwan university
#
# Based on the R code of "A shorter tour of R/qtl" and 
#			    of "Genetic map construction with R/qtl" by Karl W Broman
# downloaded from http://www.rqtl.org/tutorials
#
# Modified by Yung-Fen Huang 
#
# November 2018 - November 2019
##############################################################
## Change library path --> Specific for YFH
.libPaths("D:/Software/R/library")

############################################################
# Preliminaries
############################################################
## Install the R/qtl package (if you haven't had it on your computer)
install.packages("qtl") 
library(qtl)

# Have a look of the function for data import
?read.cross

############################################################
# Data import
############################################################
# Please be aware that file path needs to be modified according to your setup
# Oakley et al. (2018)
# Files downloaded from dryad are directly usable

# Before launching the following code, refer to the datasets that you have checked in a text editor
# and ?read.cross, tell the class why the parameters are assgined as such.

cro <- read.cross(format= "csvs", genfile = "geno.csv", phefile = "pheno.csv", 
			na.strings=c("-","NA"), genotypes=c("a","b"), estimate.map=FALSE, 
			map.function= "kosambi", crosstype = "riself")

 
# Have a look of the output after reading in the files

############################################################
# Have a look at the data
#
# HEADS-UP: if you want to modify the cross object
#		YFH recommend you to generate a new cross object
#		such as cro1 = cro
#		and make changes in cro1
#		so you won't lose the original data
############################################################
summary(cro)

nind(cro) # number of individuals
nchr(cro) # number of "chromosomes" (more exactly, no. of linkage groups)
totmar(cro) # number of total markers
nmar(cro) 
nphe(cro)
phenames(cro)
markernames(cro)

plot(cro)

plotMissing(cro)
plotMap(cro)
plotPheno(cro, pheno.col=1)
plotPheno(cro, pheno.col=2)
plotPheno(cro, pheno.col=3)
plotPheno(cro, pheno.col=4)
plotPheno(cro, pheno.col=5)
plotPheno(cro, pheno.col=6)

plotPheno(cro, pheno.col=11)
plotPheno(cro, pheno.col=12)
plotPheno(cro, pheno.col=13)
# Please continue yourself

# Are you going to use the data as they are or do some cleaning?

##===============================================##
## Examine the marker data using a figure
##===============================================##
plotMissing(cro)

# Draw missing data per individual and per marker
par(mfrow=c(1,2), las=1, cex=0.8)
plot(ntyped(cro), ylab="No. typed markers", main="No. genotypes by individual")
plot(ntyped(cro, "mar"), ylab="No. typed individuals",
     main="No. genotypes by marker")  

## ==> Decent data set. Why?

##===============================================##
## Identify duplicate individuals (if any)
##===============================================##
cg <- comparegeno(cro)
par(mar=c(4.1,4.1,0.1,0.6),las=1)
hist(cg[lower.tri(cg)], breaks=seq(0, 1, len=101), xlab="No. matching genotypes",
     main="")
rug(cg[lower.tri(cg)]) 

## What would you say?

wh <- which(cg > 0.9, arr=TRUE) # Feel free to vary the number and make your own decision in the final report
wh <- wh[wh[,1] < wh[,2],] # remove duplicated information
wh

## Check some pairs
g <- pull.geno(cro)
dim(g)
g[1:6,1:6]
table(g[1,], g[479,]) 
table(g[211,], g[492,])


## If you want to remove some individuals that are "too similar", you can use the following:
cro <- subset(cro, ind=-wh[,2]) 
	# but this removes systematically individuals of bigger row number
	# In real life, you may want to remove individuals with more missing data, then you will need to do other things
	# You can also go back to the tutorial of R/qtl to see their other way of doing ## code chunk number 16

##===============================================##
## Identify duplicate markers (if any)
##===============================================##
print(dup <- findDupMarkers(cro, exact.only=FALSE))
table(g[,"X2_4621443"], g[,"X2_5049931"]) 
	# You can make your own decision on whether remove the duplicated marker or not, with reasonable argument

##========================================================##
## Check for segregation distorsion within each locus
##========================================================##
gt <- geno.table(cro)
gt[gt$P.value < 0.05/totmar(cro),]  # Is it necessary to remove them? why?


##========================================================##
## Check for genotypic frequency within individuals
##========================================================##
gfreq <- apply(g, 1, function(a) table(factor(a, levels=1:2)))
gfreq <- t(t(gfreq) / colSums(gfreq))
par(mfrow=c(1,2), las=1)
for(i in 1:2)
  plot(gfreq[i,], ylab="Genotype frequency", main=c("AA", "BB")[i],
       ylim=c(0,1))

	## What's your thought?


############################################################
# Construction of the linkage map
############################################################
##========================================================##
## Keep a copy of the original map
##========================================================##
map0 = pull.map(cro)

##========================================================##
## Estimate pair-wise recombination fraction
##========================================================##
cro <- est.rf(cro)

##========================================================##
## Plot recombination fraction and LOD scores
##========================================================##
plotRF(cro)

##========================================================##
## Group markers
##========================================================##
lg <- formLinkageGroups(cro, max.rf=0.35, min.lod=6)
table(lg[,2])
	# Does this match to your expectation?
	# If not, please change the parameters to better fit your expectation


cro1 <- formLinkageGroups(cro, max.rf=0.3, min.lod=6, reorgMarkers= TRUE)

## Check the LG order between cro1 and cro
nmar(cro1)
nmar(cro)

## Using cro's LG nomenclature for cro1
cro1$geno = cro1$geno[c(1,4,3,5,2)]
nmar(cro1)
names(cro1$geno) = c("1", "2", "3", "4", "5")

## Change rf order in cro1
cro1$rf = cro1$rf[c(1:90, 238:295, 171:237, 296:348, 91:170),c(1:90, 238:295, 171:237, 296:348, 91:170)]
cro1$rf[91:95, 91:95]

plotRF(cro1)
plotRF(cro)

#========================================================##
## Order markers
##========================================================##
system.time(revcross <- orderMarkers(cro1, chr = 4, use.ripple = FALSE)) 
	# on YFH's computer: 4.74 sec Core Intel i7-4790K@4GHz; 32G

system.time(rp4 <- ripple(revcross, chr = 4, map.function = "kosambi"))
	# on YFH's computer: 0.16 sec

system.time(revcross <- orderMarkers(cro1, chr = 4, use.ripple = TRUE, map.function = "kosambi")) 
	# on YFH's computer: 74.85 sec

system.time(cro1 <- orderMarkers(cro1, use.ripple = TRUE, map.function = "kosambi"))
system.time(mapT <- est.map(cro1))
	# estimated time on YFH's computer: 15 min
 
plotMap(cro, mapT)
dev.new()
plotMap(cro, cro1)

## ==> What are you going to do?
## Compare your map and the original map

save.image("r_oakley.RData")
