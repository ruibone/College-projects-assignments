#########################################################
## Exercise on Linkage Map & QTL Mapping
## Date: December 2017
#########################################################
## Objective of this file (for_map_formatting.R): 
## 1) re-name the LG according to the "true" LG nomenclature
## 2) output the ordered and re-named map into the same format to facilitate cross-comparison
## An accompagnied Rworkspace (for_map_formatting.RData) is available as illustration
##	- f2.1.50: a list of 4 cross objects, each contains a map whose LG nomenclautre needs to be modified
##	- other objects are created along this file

## Load/Activate the R/qtl package
library(qtl)

## Check what you have in the workspace
ls() 

##----------------------##
## 1) Rename the LG
##----------------------##
## Pull out the map into a list
f2.1.50map <- lapply(f2.1.50, pull.map)

## Add reference map into the object
f2.1.50map[["cmap1"]] <- cmap1

# Check the outcome of the combination
str(f2.1.50map)

## Check the map summary and detail in your map and the reference
lapply(f2.1.50map, summary)

# Output maps into a dataframe
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

# Make a new list and modify the new one
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

# Pull maps from crosses
f2.1.50map2 <- lapply(f2.1.50b, pull.map)

# Output the list and share it via the cloud: https://drive.google.com/drive/folders/1FLmLAtcy8Ohe87n6BrhCdNGbuiDzTknl?usp=sharing
save(f2.1.50map2, file = "f2.1.50.RData")

# To load the map objects to your workspace, use load()
load("f2.1.50.RData")


##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## Below shows two possibilities to compare two maps using dotplot			
## 1. plotMap() implemented in R/qtl
##	[Condition]: Identical LG nomenclature between 2 maps
## 2. Dotplot using R basic graphic function plot()
## 	[Condition]: LG nomenclautres are different between maps (1 vs. 1.1 and 1.2)
## 	[Principle]: Create cumulative distance to facilitate plotting
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##

## Add reference map into the object
f2.1.50map2[["cmap1"]] <- cmap1

# Check the outcome of the combination
str(f2.1.50map2)

## Check the map summary and detail in your map and the reference
lapply(f2.1.50map2, summary)

##------------------------##
## 1. plotMap() in R/qtl
##------------------------##
## Graphical comparison between two maps (only chr of same name can be compared) 
plotMap(f2.1.50map2[[5]], f2.1.50map2[[2]], chr = 1:3, main = "cmap1 vs. f2.1.50se") # both of them should be map objects using the same nomenclature
savePlot("cmap1_f2.1.50se.tiff", type = "tiff")

##--------------------------------------##
## 2. Dotplot (simple plot function)
##--------------------------------------##
## Prepare working objects
smap2 <- lapply(f2.1.50map2, summary)
lmap2 <- lapply(smap2, function(x) x[1:(nrow(x)-2), 2]) # from LG1 to final-1 LG
toadd <- lapply(lmap2, function(x) c(0, cumsum(x)))

tmaps2 <- lapply(f2.1.50map2, map2table)

# Pull markers into a column + Add plotting coordinate
for (i in 1:length(tmaps2))
{
	t1 <- tmaps2[[i]]		#pull the element

	# Add cumulative distance for drawing
	t2 <- toadd[[i]]
	t3 <- table(t1[1])
	t1$add <- rep(t2, t3)
	t1$plotx <- t1$pos + t1$add

	# Rename the columns 
	names(t1) <- paste(names(tmaps2)[i], names(t1),sep = ".") 

	# Add marker names
	t1$mk <- rownames(t1) 	

	tmaps2[[i]] <- t1 			#replace the element
	rm(list = c("t1", "t2", "t3"))	#clean the workspace
}

# Check the outcome of the action
lapply(tmaps2, head)

# Assemble all the maps into a data fram
tmapsd2 <- Reduce(function(...) merge(..., by = "mk", all=T), tmaps2)
head(tmapsd2)
names(tmapsd2)

# Order the marker according to the "true" order
tmapsd2 <- tmapsd2[order(tmapsd2$cmap1.chr, tmapsd2$cmap1.pos),]

# Compare maps
pdf("f2_1_50_compMaps.pdf")

for (i in c(5,9,13,17))
{
  plot(tmapsd2[,21], tmapsd2[,i] , xlab = names(tmapsd2)[21], ylab = names(tmapsd2)[i], pch = 16)

  # Add LG limit
  abline(v = unique(tmapsd2[,20]), col = "lightgray")
  abline(h = unique(tmapsd2[,i-1]), col = "lightgray")
}

dev.off()
