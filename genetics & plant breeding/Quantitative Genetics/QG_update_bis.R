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
load("cmap1.RData")  
load("cmap2.RData")

## Check what you have in the workspace
ls() # You should see cmap1 or cmap2 that you have just loaded into the workspace

##===================================##
## Examine the map
##===================================##
str(cmap1) # Check the structure of cmap
summary(cmap2) # Get an idea of cmap1's major characteristics:
		   # No. of marker per linkage group and per whole map
		   # Length per linkage group and per whole map
		   # Average interval between markers per linkage group and per whole map
		   # Maximum interval between markers per linkage group and per whole map

cmap1 # Have a look of cmap1
	# How are markers named? 
	# (It can serve as reference at map construction)

plot(cmap1, main = "cmap1") # Plot the map. "main=" is to give the title of the plot
savePlot("cmap1.tiff", type = "tiff") # You can use this function to save plots.
						  # There are other file format. Use ?savePlot to see more.

##===================================##
## Generate the simulated data
##===================================##
## Based on the instruction, create your simulation data
## Please remember to change the parameters

##---------------##
## Example: F2 
##---------------##
## Create the QTL information matrix
	#-----------------#
	# Simple trait
	#-----------------#
	mod1 <- rbind(c(1, 45, 4, 1),c(5, 70, 2, 0)) # in c(), the order is chr, pos, a, d
								   # This creates a matrix of 2 rows x 4 columns

	#-----------------#
	# Complex trait
	#-----------------#
	mod2 <- rbind(c(1, 45, 3, 1), 
			  c(2, 25, 3, 0),
			  c(2, 50, -1.5, -0.5), 
			  c(5, 5, 1, 1),
			  c(7, 50, 2, 0.5))

## Generate data
f2.1.50s <- sim.cross(cmap2, model= mod1, n.ind=200, 
                      type = "f2",error.prob=0.01, missing.prob=0, partial.missing.prob=0,
                      keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                      map.function= "haldane")

f2.1.50c <- sim.cross(cmap2, model= mod2, n.ind=200, 
                      type = "f2",error.prob=0.01, missing.prob=0, partial.missing.prob=0,
                      keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                      map.function= "haldane")

f2.1.50se <- sim.cross(cmap2, model= mod1, n.ind=200, 
                      type = "f2",error.prob=0.001, missing.prob=0, partial.missing.prob=0,
                      keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                      map.function= "haldane")

f2.1.50ce <- sim.cross(cmap2, model= mod2, n.ind=200, 
                      type = "f2",error.prob=0.001, missing.prob=0, partial.missing.prob=0,
                      keep.qtlgeno=TRUE, keep.errorind=TRUE, m=0, p=0,
                      map.function= "haldane")


## If you are interested to know the "true" genotype of QTL, you can use f2.1.50s$qtlgeno to check it out

## sim.cross creates object of class "cross", have a look of it
summary(f2.1.50s)

## To facilitate further analyses, you can put the objects into a list

f2.1.50 <- list(f2.1.50s = f2.1.50s, f2.1.50se = f2.1.50se, f2.1.50c =f2.1.50c, f2.1.50ce=f2.1.50ce)

## Have a look of the summary
lapply(f2.1.50, summary) # "apply" series is very practical. 
				 # "lapply" means "apply the same function to all the elements of the list"
				 # so you can shorten your code.

## To avoid further confusion, remove indivdual simulation objects
rm(list = c("f2.1.50c", "f2.1.50ce", "f2.1.50s", "f2.1.50se"))

##================================================##
## Test for segregation distortion for each locus
##================================================##
# Apply the test to all cross objects
gt.1.50 <- lapply(f2.1.50, geno.table)
lapply(gt.1.50, head)
lapply(gt.1.50, function(x) x[x$P.value < 0.05/totmar(cmap2)]) 

	# Any marker show segregation distortion?


##================================================##
## Construct a genetic map
##================================================##
##-----------------------------------##
##  Estimate recombination fraction  ##
##-----------------------------------##
# Individual version #
f2.1.50s <- est.rf(f2.1.50s)

## Plot of LOD scores versus estimated recombination fractions for all marker pairs

plotRF(f2.1.50s, what = "both", col.scheme = "redblue", main ="F2 cmap1 50 ind")


# Apply to all the elements #
f2.1.50 <- lapply(f2.1.50, est.rf)

pdf("cmap1_f2_50_plotRF01.pdf")

for (i in 1:length(f2.1.50))
{
	t1 <- f2.1.50[[i]]
	plotRF(t1, what = "both", col.scheme = "redblue", main = names(f2.1.50)[i])
}

dev.off()

	# The upper triangle is recombination fractions
	# The lower right triangle shows the LOD score
	# Red means high LOD and small rf, blue means low LOD and high rf

	# What would you say about the figure?
		# You will see 7 chromosomes
		# because a map of 7 LG was used to produce the data

##-----------------------------------##
##  Form linkage groups 	       ##
##-----------------------------------##
# Apply the function to all elements #
lg.1.50 <- lapply(f2.1.50, function(x) formLinkageGroups(x, max.rf=0.35, min.lod = 3))
	# At this step, lapply() can do a first trial.
	# After, I would suggest you to take a cross and to adjust the parameters
	# Once the parameters adjusted, you can apply it to all crosses since the data are simulated based on the same map

# Have a look of the result, take the 1 cross as example
lg.1.50[[1]]
	# origchr = original chomosome, 
	# LG = new LG formed based on the pair-wise rf estimated using est.rf
	# The nomenclautre of new LG is based on no. of markers in decreasing order.
	# i.e., the one with the highest no. of marker = 1 and so on.
	# Are your no. of LG identical to the "true" no. of LG?
	# If not, since the "origchr" is the "true" one, 
	# change the parameters (max.rf & min.lod) to see whether you can make the no. of LG close to the no. of origchr
	# Use your knowledge from cmap (max spacing, the map plot) 
	# to help you have an expectation of the no. of LG.

	# You can take out the cross object from the list temporarely
  	# And apply the parameters to all using lapply

  #-----------------#
  # Parameter test
  #-----------------#
t1 <- f2.1.50[[4]]
tlg1 <- formLinkageGroups(t1, max.rf=0.4, min.lod=3)
tlg1
lg.1.50[[4]] # Compare with the results of the 1st set of parameters

rm(list = c("t1", "tlg1")) # Clean workspace
	# If after trial and errors, you cannot reach the same number as the true one,
	# That means that your data can only group markers into that number of LG
	# So, just continue.

  ## [[1]] = 0.35, 3
  ## [[2]] = 0.35, 3
  ## [[3]] = 0.35, 3
  ## [[4]] = 0.35, 3

# Replace the previous map by the present map ("reorgMarkers=TRUE")
# which organise LG according to no. of markers (decreasing order)

f2.1.50 <- lapply(f2.1.50, function(x) formLinkageGroups(x, max.rf=0.35, min.lod=3, reorgMarkers=TRUE))

# Plot again recombination frequency
pdf("cmap1_f2_50_plotRF_reorg.pdf")

for (i in 1:length(f2.1.50))
{
	t1 <- f2.1.50[[i]]
	plotRF(t1, what = "both", col.scheme = "redblue", main = names(f2.1.50)[i])
}

dev.off()

	# => Examine the graph. It looks pretty OK.
	# Actually, we haven't order marker yet.
	# It is because the genotypic data are organised according to marker order.

##-----------------------------------##
##  Order markers			       ##
##-----------------------------------##
## There are two degrees of genotyping error
## You can order markers according to the genotyping error that you know for each dataset
## Or to use an overall error probability, 0.01 or 0.001

system.time(f2.1.50 <- lapply(f2.1.50, function(x) orderMarkers(x, chr=1:7, window=7, 
								    use.ripple=TRUE, error.prob=0.01, map.function= "haldane")))
# 1022 sec
	
	#nchr() is a fucntion which determines the no. of LG in a cross or map object
	#window : the no. of markers used for marker ordering
	#We use the default criteria which counts the no. of crossover and the order with the smallest no. of crossover is considered as the best order
	#This can take some time.

## Plot again rf vs. LOD in a new window and compare with the one before ordering
f2.1.50 <- lapply(f2.1.50, est.rf)

pdf("cmap1_f2_50_plotRF_ord.pdf")

for (i in 1:length(f2.1.50))
{
	t1 <- f2.1.50[[i]]
	plotRF(t1, what = "both", col.scheme = "redblue", main = names(f2.1.50)[i])
}

dev.off()

	#Compare plotRF_reorg.pdf and plotRF_ord.pdf
	#Is there any difference?

## Pull out the map into a list
f2.1.50map <- lapply(f2.1.50, pull.map)

## Add reference map into the object
f2.1.50map[["cmap1"]] <- cmap1

# Check the outcome of the combination
str(f2.1.50map)

## Check the map summary and detail in your map and the reference
lapply(f2.1.50map, summary)

# You can output maps into a dataframe format to facilitate further comparison
tmaps <- lapply(f2.1.50map, map2table)

# Pull markers into a column
for (i in 1:length(tmaps))
{
	t1 <- tmaps[[i]]		#pull the element
	names(t1) <- paste(names(tmaps)[i], names(t1),sep = ".") # rename the column for after
	t1$mk <- rownames(t1) 	#add marker names
	tmaps[[i]] <- t1 		#replace the element
	rm(t1)			#clean the workspace
}

# Check the outcome of the action
lapply(tmaps, head)

# Assemble all the maps into a data fram
tmapsd <- Reduce(function(...) merge(..., by = "mk", all=T), tmaps)
head(tmapsd)

# Order the marker according to the "true" order
tmapsd <- tmapsd[order(tmapsd$cmap1.chr, tmapsd$cmap1.pos),]

# Check the LG name and marker order 
# Note them.
# 1) Re-name LG based on "true" LG name
# 2) Arrange marker order according to "true" marker order 

tmapsd

## Then you can re-name the LG based on the nomenclature in reference map
## If one ref LG is cut into several ones in your construction
## A suggestion is to name your LG as 1.1, 1.2, etc. based on ref 1
## And it will be easier to make comparison.

# Rename the LG
f2.1.50b <- f2.1.50

names(f2.1.50b[[1]]$geno) <- c("5", "6", "2.2", "7.2", "1.2", "3.1", "4.2", "1.1", "3.2", "4.1", "7.1", "2.1")
names(f2.1.50b[[2]]$geno) <- c("1", "2", "3", "5", "6", "7.3", "4.2", "4.1", "7.1", "7.2")
names(f2.1.50b[[3]]$geno) <- c("4", "6", "7", "5.2", "1.2", "2.3", "3.1", "3.2", "1.1", "2.2", "2.1", "5.1")
names(f2.1.50b[[4]]$geno) <- c("3", "5", "6", "1.2", "4.2", "7.2", "2.1", "2.2", "1.1", "4.1", "7.1")

# Order LG according to their no.
for(i in 1:length(f2.1.50b))
{
  t1 <- f2.1.50b[[i]] 
  t1$geno <- t1$geno[order(names(t1$geno))]
  f2.1.50b[[i]] <- t1
  rm(t1)
}


##==========================================#
## QTL analysis 
##==========================================#
##~~~~~~~~~~~~~~~~~~~~##
## Simple trait
##~~~~~~~~~~~~~~~~~~~~##
##--------------------------##
## Marker regression
##--------------------------##
# Let's make a simple marker regression
# We will take one cross to illustrate the method
# Before that, let's examine the phenotype
f2.1.50s <- f2.1.50b[[1]] # We will use the map that you have made

nphe(f2.1.50s) # no. of phenotype
plotPheno(f2.1.50s,  pheno.col=1) # Check the distribution of the data
plot(f2.1.50s) # This will give you three plots 

# Let's make a marker regression
out.mr<- scanone(f2.1.50s, pheno.col= 1, method="mr")

# Let's make 1000-times permutation to determine the threshold
perm.mr <- scanone(f2.1.50s, pheno.col= 1, method="mr", n.perm=1000)

# Have a look of the alpha = 0.05 threshold for the trait of interest
summary(perm.mr, alpha=0.05)

# Look at the results: 

## Let's make a plot for visual inspection
 plot(out.mr, lodcolumn=1,main="[Marker regression]")
 add.threshold(out.mr, perms=perm.mr, lodcolumn=1, alpha=0.05)
 add.threshold(out.mr, perms=perm.mr, lodcolumn=1, alpha=0.1,lty=2)

## There are many formats, I chose one table by trait.
## You can try others.
summary.mr <- summary(out.mr, threshold = summary(perm.mr, alpha=0.05), 
					format="tabByCol", perms = perm.mr1, 
					ci.function = "lodint", drop = 1.5) 
	# "ci.function" and "drop" means to derive confidence interval from the peak LOD value using the LOD-1.5 criteria
	# It is not relevant in the case of marker regression. It will be output automatically using format = "tablByCol"

summary.mr # Have a look of these markers and their position
str(summary.mr)

## You can lower the threshold (alpha = 0.1 etc.) to see what you will get

##-----------------------------------------------------##
## Calculate conditional genotypic probabilities
##-----------------------------------------------------##
f2.1.50s <- calc.genoprob(f2.1.50s, step = 1, error.prob = 0.01, map.function = "haldane")
	# Important thing is: use the same mapping function as you have used for genetic map construction

###--------------------------###
### Simple interval mapping
### Haley and Knott 
###--------------------------###
out1.f2.1.50s <- scanone(f2.1.50s, pheno.col=1, model="normal", method="hk")

# Check the new object
head(out1.f2.1.50s)
str(out1.f2.1.50s)

perm4 <- scanone(f2.1.50ce, pheno.col=1, model="normal", method="hk", n.perm=1000)

# Check the new object
summary(perm1)

## Let's make an file to help the visual inspection
 plot(out1.f2.1.50s, lodcolumn=1,main="[SIM]")
 add.threshold(out1.f2.1.50s, perms=perm1, lodcolumn=1, alpha=0.05)
 add.threshold(out1.f2.1.50s, perms=perm1, lodcolumn=1, alpha=0.1,lty=2)


# Look at the result in table format
summary.sim.f2.1.50c <- summary(out1.f2.1.50c, threshold = summary(perm3, alpha=0.05), 
				format="tabByCol", perms = perm3, 
				ci.function = "lodint", drop = 1.5) 
summary.mr
summary.sim.f2.1.50s

##==================================#
## Estimate the QTL size and effect
##==================================#
#~~~ Marker regression
# Make a "QTL" object based on QTL result
summary.mr
qm4.7 <- makeqtl(f2.1.50ce, chr = 7, pos = 51.2, what = "prob")

# Estimate QTL effect
fm4.7 <- fitqtl(f2.1.50ce, pheno.col=1, qtl = qm4.7, formula = y ~ Q1, method="hk", get.ests = T)
summary(fm4.7, pvalues=F)

#~~~ Interval mapping
# Make a "QTL" object based on QTL result
summary.sim.f2.1.50s
q4.7 <- makeqtl(f2.1.50ce, chr = 7, pos = 49, what = "prob")

# Estimate QTL effect
f4.7 <- fitqtl(f2.1.50ce, pheno.col=1, qtl = q4.7, formula = y ~ Q1, method="hk", get.ests = T)
summary(f4.7, pvalues=F)

	# ==> Compare marker regression, IM, and true value
	#	What do you observe? What would you say about it?

##======================================================#
## Let us look into the multiple QTL mapping procedure
##======================================================#
## Based on the SIM results, there seems to be only 1 QTL controlling the trait
## meanwhile, we know that there is a second QTL on LG05
## Let us make a multiple QTL model based on SIM result
## and scan again the genome to see wh  ether there are other QTLs

# Let's see whether there are other possible QTLs
aq1 <- addqtl(f2.1.50[[1]], pheno.col=1, qtl=q1, formula=y~Q1, method="hk")

 # Let's visualize whether there are additional QTL
plot(aq1)
add.threshold(out1.f2.1.50s, perms=perm1, lodcolumn=1, alpha=0.05)
add.threshold(out1.f2.1.50s, perms=perm1, lodcolumn=1, alpha=0.1,lty=2)
	# => at alpha = 0.05 (SIM permutation), a new QTL appear at LG05

# Make a new QTL object based on the result
summary(aq1)
q2 <- addtoqtl(f2.1.50s, q1, chr = 5, pos = 76)

# Refine QTL position
r2 <- refineqtl(f2.1.50s, pheno.col=1, qtl = q2, method="hk")
r2

# Re-estimate the QTL effect based on the new position
f2 <- fitqtl(f2.1.50s, pheno.col=1, qtl = q2, formula = y ~ Q1 + Q2 , method="hk", get.ests = T)
summary(f2, pvalues=F)
summary(f1, pvalues=F)
	# You can see the change of the total variance explained by the model
	# as well as the individual QTL

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## For complexe trait, you can continue to addqtl(), refineqtl(), fitqtl()
## in order to find out all the QTL
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### R/qtl has implemented Composite Interval Mapping: cim()
### as well as Multiple Interval Mapping: stepwiseqtl().
###
### Defalt covariate selection in cim() is not flexible [you can only control the no. of covariate]
### But you can make your personal covariate selection based on marker regression or SIM 
### and give your marker choice in a list.
###
### The working algorithm of stepwiseqtl() is a bit different from the initial paper of Kao 2000
### Please refer to the R/qtl book for more details.




save.image("QG.RData")
