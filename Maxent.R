options(java.parameters = "-Xmx15g" )
Sys.setenv(NOAWT=TRUE) 
library(raster)
library(dismo) #has BRTs functions adapted by Elith & Leathwick
library(rJava) #For talking to Maxent
library(rgdal)  #'rgdal' not available for R version 3.1.2
library(gbm)   #Boosted Regression Trees
library(randomForest) #Random Forest
library(mgcv) #Generalized Additive Models
library(rgeos)
library(maptools)

setwd("~/Documents/MSc.Thesis/BRTs")


##################################################################################################################
# Read in Datasets
##################################################################################################################
rf<-read.csv("RosyFinch.csv")
rf<-rf[,2:3]

#Read in Current Environmental Layers
files.current<-list.files(path='./Current', pattern='tif', full.names=TRUE)
predictors <- stack(files.current)
#plot(predictors)

#Read in Future Climate (RCP85_2070) Avg. of 17 GCMs
files.85.2070 <- list.files(path='./Future/RCP85_2070', pattern='.tif', full.names=TRUE)
pred.85.2070 <- stack(files.85.2070)
#plot(pred.85.2070)

#Read in Future Climate (RCP26_2070) Avg. of 15 GCMs
files.26.2070<-list.files(path='./Future/RCP26_2070', pattern='.tif', full.names=TRUE)
pred.26.2070 <- stack(files.26.2070)
#plot(pred.26.2070)
##################################################################################################################
#Create 'mask' from which to draw random background points
vegetation<-raster(predictors, layer=10)

set.seed(0) #set seed to assure that the examples will always have same random sample

#Sample 10,000 random points that are not 'NA' in Vegetation.
background<-randomPoints(vegetation, p=rf, excludep=TRUE, n=10000, tryf=10, warn=2) 
dim(background) #10,000 points


#Plot the Results
#par(mfrow=c(1,1))
#plot(!is.na(mask), legend=FALSE)
#points(background, cex=0.2)

##################################################################################################################
# Creation of Training/Testing Datasets
##################################################################################################################
#Use 5 folds to create Presence train/test  (80% train, 20% test)
k <- 5
fold <- kfold(rf, k)
fold
pres_train<-rf[fold !=1, ]
dim(pres_train)
pres_test <- rf[fold ==1, ]
dim(pres_test)


#Use 5 folds to create Background train/test (80% train, 20% test) 
colnames(background) <- c('X', 'Y')
names(background)
head(background)
fold2 <- kfold(background, k)
bkgr_train <- background[fold2 !=1, ]
dim(bkgr_train)
bkgr_test <- background[fold2 ==1, ]
dim(bkgr_test)
bkgr_train <- as.data.frame(bkgr_train)  #this allows me to combine 'bkgr_train' with 'pres_train' using the rbind command.
class(bkgr_train)

#Build the Training Dataset
train <- rbind(pres_train, bkgr_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(bkgr_train))) #pb = presence/background
envtrain <- extract(predictors, train)
envtrain <- data.frame(cbind(pa=pb_train, envtrain))
envtrain[,'vegetation'] = factor(envtrain[,'vegetation'])
envtrain[,'cliffs'] = factor(envtrain[,'cliffs'])
head(envtrain)

#Build the Testing Dataset
testpres <- data.frame(extract(predictors, pres_test))
testbackg <- data.frame(extract(predictors, bkgr_test))
testpres[, 'vegetation'] <- as.factor(testpres[, 'vegetation'])
testpres[,'cliffs'] <- as.factor(testpres[, 'cliffs'])
testbackg[, 'vegetation'] <- as.factor(testbackg[, 'vegetation'])
testbackg[, 'cliffs'] <- as.factor(testbackg[, 'cliffs'])


jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava)) {
  rf.me <- maxent(predictors, p=pres_train, a=bkgr_train, removeDuplicates=TRUE, factors=c('cliffs', 'vegetation'), path="~/Documents/MSc.Thesis/Maxent/R_Analysis",
                  args=c("-J", "-P"))
  rf.me.current <- predict(rf.me, predictors, args=c("outputformat=logistic"), filename='rosyfinch_maxent_current.tif')        
}
