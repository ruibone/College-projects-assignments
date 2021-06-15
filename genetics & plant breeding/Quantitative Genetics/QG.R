#########################################################
## Exercise on Linkage Map & QTL Mapping
## Date: December 2017
#########################################################
##!!!!! If you have problem for an R function, type ?function in R to see the help
##      Of course, you can also google or just ask people.

## Check where you are (= working directory)
getwd()

## If you are not in your Quantitative Genetics working directory
## move to there
setwd("X:/THE_PATH_OF_YOUR_WD")

## Install the R/qtl package (ignore if you have it)
install.packages("qtl") 

## Load/Activate the R/qtl package
library(qtl)

## Load one of the map (cmap1 or cmap2) downloaded from CEIBA
load("cmap1.RData") # 

##===================================##
## Generate the simulated data
##===================================##
## Based on the instruction, create your simulation data
## Please remember to change the parameters

##---------------##
## example for F2 
##---------------##
## Create the QTL information matrix
	mod1 <- rbind(c(1, 45, 4, 1),c(5, 70, 2, 0)) # in c(), the order is chr, pos, a, d

ff2 <- sim.cross(cmap1, model= mod1, n.ind=50, 
                type = "f2",error.prob=0.01, missing.prob=0, partial.missing.prob=0,
                keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                map.function= "haldane")

##---------------##
## example for BC 
##---------------##
## Create the QTL information matrix
	mod1b <- rbind(c(1, 45, 3),c(5, 70, 2))

fbc <- sim.cross(cmap1, model= mod1b, n.ind=50, 
                type = "bc",error.prob=0.01, missing.prob=0, partial.missing.prob=0,
                keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                map.function= "haldane")


## If you are interested to know the "true" genotype of QTL, you can use fbc$qtlgeno to check it out


##================================================##
## Test for segregation distortion for each locus
##================================================##
gt <- geno.table(fbc)
head(gt) 
gt[gt$P.value < 0.05/totmar(fbc),]   # Use Bonferroni correction for multiple tests
						 # totmar() is a function in R/qtl, it turns the total no. of marker of the cross object

	# Any marker show segregation distortion?


##================================================##
## Construct a genetic map
##================================================##
##-----------------------------------##
##  Estimate recombination fraction  ##
##-----------------------------------##
fbc <- est.rf(fbc)

## Plot of LOD scores versus estimated recombination fractions for all marker pairs
plotRF(fbc, what = "both", xlab = "LOD", ylab = "Rf", col.scheme = "redblue")

	# The upper triangle is recombination fractions
	# The lower right triangle shows the LOD score
	# Red means high LOD and small rf, blue means low LOD and high rf

	# What would you say about the figure?

##-----------------------------------##
##  Form linkage groups 	       ##
##-----------------------------------##
lg <- formLinkageGroups(fbc, max.rf=0.35, min.lod=3) 
	# Feel free to try different max.rf and min.lod according to your data!

# Have a look of the result
lg
	# What do you see? 
	# Are your LG identical to the "true" LG?
	# If not, since the "origchr" is the "true" one, 
	# change the parameters (max.rf & min.lod) to see whether you can make the LG = chr
	# If you can, please follow the next step.
	# If after trial and errors, you still cannot to reach the same number as the true one,
	# please still follow the next step.

# Replace the previous map by the present map 
# which organise LG according to no. of markers (decreasing order)

fbc <- formLinkageGroups(fbc, max.rf=0.35, min.lod=3, reorgMarkers=TRUE)

# Plot again recombination frequency
par(mar=c(4.1,4.1,2.1,2.1), las=1)
plotRF(fbc, main="", col.scheme = "redblue")
	# => Examine the graph. It tells you the necessity to order markers
	# Actually, we haven't order marker yet.

##-----------------------------------##
##  Order markers			       ##
##-----------------------------------##
fbc <- orderMarkers(fbc, chr=1:12, window=7, use.ripple=TRUE, 
			  error.prob=0.01, map.function= "haldane")

## Plot again rf vs. LOD in a new window and compare with the one before ordering
dev.new()
fbc <- est.rf(fbc)
par(mar=c(4.1,4.1,1.6,1.6), las=1)
plotRF(fbc, main="", col.scheme = "redblue") 


## Check the map summary and detail in your map and the reference
summaryMap(fbc) 
summaryMap(cmap1) # the initial map

# Or output into a text file to facilitate further comparison work
T1 <- map2table(cmap1) 


##==========================================#
## QTL analysis (to come soon)
##==========================================#
 



