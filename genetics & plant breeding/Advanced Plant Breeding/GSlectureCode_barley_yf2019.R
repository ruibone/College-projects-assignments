#### THIS SCRIPT IS COURTESY FROM JEAN-LUC JANNINK
#### with modifications by Yung-Fen Huang 
#### 1st edit: 2015-05
#### 2nd edit: 2017-06
#### 3rd edit: 2018-05
#### 4rd edit: 2019-05

#### INSTALL rrBLUP PACKAGE ####
install.packages("rrBLUP")  

#### LOAD PACKAGES ####
library(rrBLUP)

#### SET THE DIRECTORY THAT CONTAINS FILES ####
#### NOTE: You will have to change this portion of the code for it to work ####
#### Here we assume you have put the files in a folder "GS_workshop" on the Desktop ####
setwd("C:/Users/huangy/Desktop/GS_workshop")

#### LOAD DATA ####
# Genotypic data
# Rows are markers, columns(from the fourth) are lines
genoData.raw <- read.table("genotype.hmp.txt", header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, row.names=1)
dim(genoData.raw) #1532 291
genoData.raw[1:6, 1:6]
str(genoData.raw[1:6])

genoData <- t(genoData.raw[,-c(1:3)])  # transpose the data so rows are lines and columns are markers
dim(genoData) #288 1532
genoData[1:6,1:6]

nMrk <- ncol(genoData)  #number of markers
nInd <- nrow(genoData)  #number of lines

temp <- genoData.raw[,c("chrom", "pos")]
unique(temp$chrom)
sum(temp$chrom %in% paste(1:7, "H", sep = "")) #833
temp$pos[temp$chrom %in% c("U", "UNK")] 

temp2 <- temp[temp$chrom %in% paste(1:7, "H", sep = ""),]
dim(temp2) #833 2
temp2$ch <- substr(temp2$chrom,1,1)

plot(temp2$ch, temp2$pos, type = "p", pch = 3) 



# Raw phenotypic data
fhbData.raw <- read.table("traits.txt", header=TRUE, na.strings="-999", stringsAsFactors=FALSE, sep="\t")
dim(fhbData.raw) #12727 3
head(fhbData.raw)
str(fhbData.raw)

fhbData.raw <- fhbData.raw[fhbData.raw$line %in% rownames(genoData),] # get rid of lines with no genotype
dim(fhbData.raw) #3168 3

fhbData.raw$FHB.Severity <- as.numeric(fhbData.raw$FHB.Severity) # declare the variable into appropriate class
fhbData <- fhbData.raw[apply(fhbData.raw, 1, function(vec) !any(is.na(vec))),] # get rid of lines with no phenotypic data
dim(fhbData) #958 3
hist(fhbData$FHB.Severity) # distribution of the phenotypic data

year <- sapply(strsplit(fhbData$trial, split="_", fixed=TRUE), function(vec) vec[2]) # extract "year" information from trial

fhb.trans <- asin(sqrt(fhbData$FHB.Severity / 100)) # data transformation to approximate normal distribution
hist(fhb.trans)    # check the distribution of the transformed phenotype
                   # To NTU students: you are welcome to try other transformation

# added by YFH 2015-06-16
location <- sapply(strsplit(fhbData$trial, split="_", fixed=TRUE), function(vec) vec[3]) # extract "location" information from trial
table(location)
table(fhbData$trial)
for(i in 1:length(location)) {location[i] <- ifelse(location[i] == "Crookston07", "Crookston", location[i])}

# Create a final dataset
fhbData <- cbind(fhbData, year, location, fhb.trans)
head(fhbData)
str(fhbData)
dim(fhbData) #958 6

table(fhbData$year, fhbData$location)
apply(table(fhbData$location, fhbData$line), 1, sum) # to see no. of lines per location
table(apply(table(fhbData$location, fhbData$line), 2, sum)) # no. of trials/line


# Names of the lines that were evaluated in 2006, 2007, 2008
lines2006 <- unique(fhbData$line[fhbData$year == 2006])
lines2007 <- unique(fhbData$line[fhbData$year == 2007])
lines2008 <- unique(fhbData$line[fhbData$year == 2008])
trials2006 <- unique(fhbData$trial[fhbData$year == 2006])
trials2007 <- unique(fhbData$trial[fhbData$year == 2007])
trials2008 <- unique(fhbData$trial[fhbData$year == 2008])

sum(as.character(fhbData$line) %in% rownames(genoData)) #958
sum(rownames(genoData) %in% as.character(fhbData$line)) #288

length(union(lines2006, lines2007)) #191
length(union(lines2006, lines2008)) #192
length(union(lines2007, lines2008)) #193

length(intersect(lines2006, lines2007)) #0
length(intersect(lines2006, lines2008)) #0
length(intersect(lines2007, lines2008)) #0

## Have a look at the variance components
lm.g <- lm(FHB.Severity ~ line, data = fhbData)
summary(aov(lm.g))

lm.all <- lm(FHB.Severity ~ line + year + location, data = fhbData)
summary(aov(lm.all))

lm.gy <-  lm(FHB.Severity ~ line + year , data = fhbData)
summary(aov(lm.gy))

lm.gt <- lm(FHB.Severity ~ line + trial, data = fhbData)
summary(aov(lm.gt))


#### Check to see if there is missing marker data ####
nMissingMrk <- sum(is.na(genoData))
print(paste("Number of missing marker scores:", nMissingMrk))

# Use rrBLUP to calculate an A matrix from the marker data
mrkRelMat <- A.mat(genoData, return.imputed=TRUE)
if (nMissingMrk > 0){ # Do this if you have missing marker data
  genoData.imputed <- mrkRelMat$imputed # We will use the imputed markers later on
  mrkRelMat <- mrkRelMat$A
}
print(dim(mrkRelMat)) #288 288

#### GENOMIC SELECTION ####
s <- Sys.time()
addBlupOut <- kin.blup(fhbData, geno="line", pheno="fhb.trans", K=mrkRelMat, fixed="trial")
print(Sys.time() - s) # To get an idea of calculation time 1.5 sec on HYF's PC
str(addBlupOut)

# Use the reduce=TRUE option to make it go faster
s <- Sys.time()
addBlupOutR <- kin.blup(fhbData, geno="line", pheno="fhb.trans", K=mrkRelMat, fixed="trial")
print(Sys.time() - s)
str(addBlupOutR)
plot(addBlupOut$g, addBlupOutR$g)

# Predict lines that were tested in 2008 on the basis of data from 2006 and 2007
fhbData$mask2008 <- fhbData$fhb.trans
fhbData$mask2008[fhbData$year == 2008] <- NA
addBlupOutR <- kin.blup(fhbData, geno="line", pheno="mask2008", K=mrkRelMat, fixed="trial")

# To NTU students: you can also try to predict lines of 2007 based on 2006 or other combinations

# Now see how well the predictions were, for each trial
corByTrial <- NULL
pdf("add_prediction.pdf")
for (trial in c(trials2007, trials2008)){
  phenotypes <- fhbData$fhb.trans[fhbData$trial == trial]
  predictions <- addBlupOutR$g[fhbData$line[fhbData$trial == trial]]
  corForTrial <- cor(predictions, phenotypes)
  mainTitle <- paste(trial, "[Additive]", round(corForTrial, 2))
  plot(predictions, phenotypes, main=mainTitle, xlab="Additive Prediction", ylab="Observed FHB Severity")
  corByTrial <- c(corByTrial, corForTrial)
}
dev.off()


# If you don't have lines structured by year, a general approach is to use cross validation:
# Split the training population into n parts.  Each part is called a "fold".  For each fold in turn
# remove the fold from the training population, fit the model, then predict that fold.
# This process thus avoids predicting lines that were also in the training set (a.k.a. cheating).

#### CODE TO RUN CROSS VALIDATION ON THE TRAINING DATA ####
###########################################################################################
# Function to run cross validation on a given training population 
#	with a specified number of folds. The function assumes there is only a single observation
#   for each individual and that the only fixed effect is a population mean.
# The user can repeat the cross validation for a specified number of times 
#	(folds are sampled independently each time) to gain appreciation for 
#	variability across different fold samples
# data: data frame with phenotypes, fixed effects, and covariates
# geno: name of the column with genotype ids
# pheno: name of the column with the phenotype: individuals without phenotypes will be dropped
# K: relationship matrix
# nFolds: split the training population into this number of folds.
# nRepeat: how many times to repeat the cross validation to get a sense of variability
#	both across folds within a repeat and across repeats
###########################################################################################
runFoldCrossValidation <- function(data, geno, pheno, K, nFolds, trial, nRepeats){
	# Retain only individuals who have phenotypes
	hasPheno <- !is.na(data[, pheno])
	data <- data[hasPheno,]
  allID <- unique(data[, geno])
	K <- K[allID, allID]
	trials <- unique(data[, trial])
	# Vectors to hold results of each of nRepeats cross validations
	meanAccuracy <- rep(NA, nRepeats)
  # Matrix to hold the predictions for each observation and each repeat
  crossValPred <- matrix(NA, nrow(data), nRepeats)
	# Loop to do nRepeat independent cross validations
	for (repetition in 1:nRepeats){
		print(paste("Repeat", repetition))
		folds <- sample(rep(1:nFolds, length.out=length(allID)))
		for (fold in 1:nFolds){
			# Identify the individuals associated with this fold
			indInFold <- which(folds == fold)
      index <- data[, geno] %in% allID[indInFold]
			# Create a phenotype and set those individuals to missing
			data$crossValPheno <- data[, pheno]
			data$crossValPheno[index] <- NA
			# Run genomic prediction while removing indInFold from training but including them in prediction
			addBlupOut <- kin.blup(data, geno, "crossValPheno", K=K, fixed="trial")
			crossValPred[index, repetition] <- addBlupOut$g[data[, geno][index]]
		}
		corByTrial <- NULL
		for (tr in trials){
		  phenotypes <- data[, pheno][data[, trial] == tr]
		  predictions <- crossValPred[data[, trial] == tr, repetition]
		  corForTrial <- cor(predictions, phenotypes)
		  corByTrial <- c(corByTrial, corForTrial)
		}
		meanAccuracy[repetition] <- mean(corByTrial)
	}
	return(list(meanAccOverTrials=meanAccuracy, crossValidatedPredictions=crossValPred))
}#END runCrossVal

#### Use runCrossValidation to estimate what future prediction accuracy would be ####
# Warning: depending on the power of your computer, this might take a little while
#          You are welcom to try larger nRepeat, 3 is small.
crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRelMat, nFolds=5, trial="trial", nRepeats=3)
print(round(crossValOut$meanAccOverTrials, 3))
plot(crossValOut$crossValidatedPredictions[,1:2])

#### CODE TO TEST THE EFFECT OF TRAINING POPULATION SIZE ON ACCURACY ####
###########################################################################################
# Function to test the effect of traing population size
# 	on accuracy.
# The function uses training population sizes of 8, 16, 
# 	32, 64, 128, and 192 for a total number of Ntimes
# 	per size.
# The validation population are the lines from 2008
# The results are then shown in a boxplot and it can be
# 	seen that the accuracy increases as the training 
# 	population increases.  Nevertheless, accuracy is quite high
#   even for TP size = 8.  This may be caused by different levels
#   of the trait among subpopulations (see below)
###########################################################################################
# Rather than k-fold cross validation, sample a random training population of size trainSize
# then either validate the model against the rest of the population (if is.null(validPop))
# or validate against a given validation population
runSampleCrossValidation <- function(data, geno, pheno, K, trainSize, validPop=NULL, trial, nRepeats){
  # Retain only individuals who have phenotypes
  hasPheno <- !is.na(data[, pheno])
  data <- data[hasPheno,]
  allID <- unique(data[, geno])
  K <- K[allID, allID]
  trials <- unique(data[, trial])
  # Vectors to hold results of each of nRepeats cross validations
  meanAccuracy <- rep(NA, nRepeats)
  # Matrix to hold the predictions for each observation and each repeat
  crossValPred <- matrix(NA, nrow(data), nRepeats)
  # Loop to do nRepeat independent cross validations
  for (repetition in 1:nRepeats){
    print(paste("Repeat", repetition))
    # sample the training population and by difference the validation population
    trainPop <- sample(setdiff(allID, validPop), trainSize)
    if (is.null(validPop)) testPop <- setdiff(allID, trainPop) else testPop <- validPop
    index <- data[, geno] %in% testPop
    # Create a phenotype and set those individuals to missing
    data$crossValPheno <- data[, pheno]
    data$crossValPheno[!(data[, geno] %in% trainPop)] <- NA
    # Run genomic prediction while removing testPop from training but including them in prediction
    addBlupOut <- kin.blup(data, geno, "crossValPheno", K=K, fixed="trial")
    crossValPred[index, repetition] <- addBlupOut$g[data[, geno][index]]
    corByTrial <- NULL
    for (tr in trials){
      phenotypes <- data[, pheno][data[, trial] == tr]
      predictions <- crossValPred[data[, trial] == tr, repetition]
      corForTrial <- cor(predictions, phenotypes, use="pairwise.complete.obs")
      corByTrial <- c(corByTrial, corForTrial)
    }
    meanAccuracy[repetition] <- mean(corByTrial, na.rm=TRUE)
  }
  return(list(meanAccOverTrials=meanAccuracy, crossValidatedPredictions=crossValPred))
}#END runCrossVal

# Do sample cross validation with the range of training population sizes in tpSizeVec
testTrainingSize <- function(data, geno, pheno, K, validPop, tpSizeVec, trial, nRepeats=10){
  hasPheno <- !is.na(data[, pheno])
  data <- data[hasPheno,]
  allID <- unique(data[, geno])
  maxTPsize <- length(setdiff(allID, validPop))
  tpSizeVec <- tpSizeVec[tpSizeVec <= maxTPsize]
  outPut <- NULL
	for (tpSize in tpSizeVec){
		print(paste("Training Population Size", tpSize))
    nR <- ifelse(tpSize == maxTPsize, 1, nRepeats) # only do it once if all non VP individuals are in TP
    crossValOut <- runSampleCrossValidation(data, geno, pheno, K, tpSize, validPop, trial, nRepeats=nR)
		outPut <- rbind(outPut, cbind(trainingSize=tpSize, accuracy=crossValOut$meanAccOverTrials))
	}
	return(outPut)
}

#### This may take a while, but to get a good estimate of the accuracy at a given TP size
#### You need to repeat many times (100 times here...)
tpSizeVec <- c(8, 16, 32, 64, 128, length(c(lines2006, lines2007)))
trainingSizeOut <- testTrainingSize(fhbData, "line", "fhb.trans", mrkRelMat, lines2008, tpSizeVec, "trial", 10)
boxplot(accuracy ~ trainingSize, data=trainingSizeOut, xlab="Training Population Size", ylab="Accuracy")
medianAcc <- tapply(trainingSizeOut[,"accuracy"], trainingSizeOut[,"trainingSize"], median)
plot(tpSizeVec, medianAcc, xlim=c(0, max(tpSizeVec)), ylim=c(0, max(medianAcc)))

## TO NTU students: you are welcome to varying the parameters and try out different things!