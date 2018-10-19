#install.packages(c('raster','rJava','dismo','gbm', 'randomForest', 'mgcv', 'rgdal', 'maptools', 'rgeos', 'ROCR', 'vcd', 'boot', 'snow'))

library(raster)
library(dismo) #has BRTs functions adapted by Elith & Leathwick
#library(rJava) #For talking to Maxent
library(rgdal)  #'rgdal' not available for R version 3.1.2
library(gbm)   #Boosted Regression Trees
library(randomForest) #Random Forest
library(mgcv) #Generalized Additive Models
#library(snow)
#library(rgeos)
#library(maptools)
#library(ROCR)
#library(boot)
#library(vcd)


setwd("~/Documents/MSc.Thesis/Models")
load("~/Documents/MSc.Thesis/Models/SDMs.Rdata")

#Useful commands to determine versions of R & packages:
#vignette('brt', package="dismo") #there's a version from Sept.9,2014
#vignette('sdm', package="dismo") #there's a version from Sept.9,2014
#sessionInfo()
#version$version.string   #or just type 'version'  ; in a shell, type 'R --version'
#packageVersion("dismo")

#browseVignettes()
##################################################################################################################
# Read in Datasets
##################################################################################################################
rf<-read.csv("RosyFinch_Few_Duplicates.csv")
dups<-duplicated(rf); sum(dups)  # Identify 8 duplicates
rf<-rf[!dups, ]       # Remove points with exact same coordinates 

#Read in Current Environmental Layers
setwd("~/Documents/MSc.Thesis/Models")
files.current<-list.files(path='./Current_Res_828x828m', pattern='tif', full.names=TRUE)
files.current.nocliffs <- files.current[,-21]
predictors <- stack(files.current) #; rm(files.current)
#plot(predictors)

# Remove Duplicate Occurrences in Same Raster Cell
bio1 <- raster(predictors, layer=1)
rf.raster <- rasterize(x=rf, y=bio1, field=1, fun='last', na.rm=TRUE) 
rf.points <- rasterToPoints(rf.raster, fun=function(x){x==1}); 
dups <- duplicated(rf.points); sum(dups); dim(rf.points)  
rf.csv <- rf.points[,-3]
#write.csv(rf.csv, "RosyFinch_few_dups.csv", row.names=FALSE); 
rm(rf.raster); rm(bio1); rm(rf.csv); 

#### Didn't Work at getting rid of the 4 points with NAs
#TEST_pres <- extract(predictors, rf.points[,-3])
#dups <- duplicated(TEST_pres); sum(dups)
#TEST_pres <- TEST_pres[!dups, ]; dim(TEST_pres)
#pb.test <- c(rep(1, nrow(TEST_pres)), rep(0, nrow(bckgvals)))
#rf.data.test <- data.frame(cbind(pb.test, rbind(TEST_pres, bckgvals)))
#rf.data.test[,'cliffs'] <- as.factor(rf.data.test[,'cliffs'])
#rf.data.test[,'vegetation'] <- as.factor(rf.data.test[,'vegetation'])
#dim(rf.data.test)  # 10,000 random background points plus 1,1399 Rosy-Finch occurrences.
#str(rf.data) # cliffs', and 'vegetation' are designated factors.

#rf.data.test <- is.na(rf.data.test); summary(rf.data.test)
#rf.data.test <- na.omit(rf.data.test); summary(rf.data.test)
#rm(TEST_pres); rm(dups); rm(pb.test); rm(rf.data.test)
#################################################################
#Read in Future Climate (RCP85_2070) Avg. of 17 GCMs
#files.85.2070 <- list.files(path='./Future/RCP85_2070_Res_828x828m', pattern='.tif', full.names=TRUE)
#pred.85.2070 <- stack(files.85.2070); rm(files.85.2070)
#plot(pred.85.2070)

#Read in Future Climate (RCP26_2070) Avg. of 15 GCMs
#files.26.2070<-list.files(path='./Future/RCP26_2070_Res_828x828m', pattern='.tif', full.names=TRUE)
#pred.26.2070 <- stack(files.26.2070); rm(files.26.2070)
#plot(pred.26.2070)
##################################################################################################################
#Create MASK from which to draw random background points
biome<-raster(predictors, layer=20)

#Sample 10,000 random points that are not 'NA' in Vegetation; Presence locations have a chance of being selected
set.seed(1) #set seed to assure that models will always have same random sample
background<-randomPoints(biome, n=10000, excludep=FALSE, tryf=10, warn=2, lonlatCorrection=FALSE) 
background<-as.data.frame(background)
names(background) <- c('X','Y'); dim(background) #10,000 points
dups<-duplicated(background); sum(dups); rm(dups) # zero duplicates
plot(background, type='p', cex=.03, xlab='', ylab='', xaxt='n', yaxt='n')

#test <- (is.na(background)); sum(test)
#points(background, cex=0.1)
#############################################################
# Check for Spatial Sorting Bias:
#############################################################
nr <- nrow(rf.points[,-3])
set.seed(1)
s <- sample(nr, nr*.25, replace=FALSE)
pres_train_pwd <- rf[-s, ]
pres_test_pwd <- rf[s, ]; rm(s); rm(nr)

nrbk <- nrow(background)
set.seed(1)
sbk <- sample(nrbk, nrbk*.25, replace=FALSE)
back_train_pwd <- background[-sbk, ]
back_test_pwd <- background[sbk, ]; rm(nrbk); rm(sbk)

ssb <- ssb(p=pres_test_pwd, a=back_test_pwd, reference=pres_train_pwd, lonlat=FALSE)
# Convert To Meters
ssb <- ssb/1000; ssb  #Train/test presences are 10.83029 km apart whereas train presence and background test are 301.6848 km apart.
ssb.calculated <- ssb[,1]/ssb[,2]; ssb.calculated # High 'Spatial Sorting Bias': value = 0.03589935

# Pair-wise Distance Sampling
i <- pwdSample(pres_test_pwd, back_test_pwd, pres_train_pwd, nearest=TRUE, n=1, tr=.33, lonlat=FALSE)
pres_test_pwd <- pres_test_pwd[!is.na(i[,1]), ]
back_test_pwd <- back_test_pwd[na.omit(as.vector(i)), ]

# Check again for 'Spatial Sorting Bias'
sb2 <- ssb(pres_test_pwd, back_test_pwd, pres_train_pwd)
sb2 <- sb2/1000; sb2 # Train/test presence-pairs are 320.1349 km apart while Training presence/testing background pairs are now 306.6133 km apart.
sb2.calculated <- sb2[,1]/sb2[,2]; sb2.calculated # No more 'Spatial Sorting Bias': value = 1.0441
rm(ssb); rm(sb2); rm(i)

# Create Evaluation Datasets. This will be used to calculate 'Calibrated AUC' 
pres_test_cal <- data.frame(extract(predictors, pres_test_pwd))
back_test_cal <- data.frame(extract(predictors, back_test_pwd))

pres_test_cal[,'cliffs'] <- as.factor(pres_test_cal[,'cliffs'])
pres_test_cal[,'biome'] <- as.factor(pres_test_cal[,'biome'])
back_test_cal[,'cliffs'] <- as.factor(back_test_cal[,'cliffs'])
back_test_cal[,'biome'] <- as.factor(back_test_cal[,'biome'])
# Don't remove these objects --> rm(pres_test_pwd); rm(pres_train_pwd); rm(back_test_pwd); rm(back_train_pwd); necessary for Maxent evaluation since Maxent doesn't need environmental values at points.


predictors.nocliffs <- predictors[,-21]

#####################################################################################################################################
# Create Data frame with values at Presence and Background for Training Models    
#####################################################################################################################################
presvals <- extract(predictors, rf.points[,-3])
dups <- duplicated(presvals); sum(dups) # Identify whether or not individual cells have more than 1 presence
presvals <- presvals[!dups, ]; dim(presvals)        # Remove presences from cells with more than 1 presence.
bckgvals <- extract(predictors, background)

pb <- c(rep(1, nrow(presvals)), rep(0, nrow(bckgvals)))
rf.data <- data.frame(cbind(pb, rbind(presvals, bckgvals)))
rf.data[,'cliffs'] <- as.factor(rf.data[,'cliffs'])
rf.data[,'biome'] <- as.factor(rf.data[,'biome'])
dim(rf.data)  # 10,000 random background points plus 1,1399 Rosy-Finch occurrences.
str(rf.data) # cliffs', and 'vegetation' are designated factors.
rm(presvals); rm(bckgvals); rm(pb); rm(dups)

test <- is.na(rf.data); summary(test)
rf.data <- na.omit(rf.data); rm(test)  # Now 1,395 Presences
#########################################################################################################################################
# Boosted Regression Trees (aka "Gradient Boosting Machine", "Gradient Boost", "Stochastic Gradient Boosting", "Gradient Tree Boosting"
#########################################################################################################################################
#gbm.step estimates the best number of trees to be used with the model using 10-fold cross-validation. USE set.seed(1) prior to building models so cross-validation uses the exact same 10 subsets. 
#Note that the output is on a logit scale (Figure 6, p. 809; Elith et al., 2009)
#gbm.step function does the following:
# 1) Randomly divide available data (rf.data) into 10 subsets
# 2) Makes 10 different training sets comprised of 9 subsets. Each training set therefore has a unique ommitted testing dataset.
# 3) Cross-validation model fitting 

# !!! All BRT experiments showed that bio7 & bio9 had 0 contribution; drop these; Bagging Fraction experiments showed that bf of .6 performed best; Tree complexity experiments showed increasing complexity increases predictive skill
rf.data.brt <- rf.data
rf.data.brt <- rf.data.brt[,c(-18,-20)]; names(rf.data.brt)


########################
# BRT FINAL
set.seed(1)
brt.tc10.lr01 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=10, learning.rate=0.01, bag.fraction=0.6, n.folds=10, prev.stratify=FALSE); summary(brt.tc10.lr01)
summary(brt.tc10.lr01)

set.seed(1)
brt.tc10.lr01.simp <- gbm.step(data=rf.data.brt, gbm.x=2:20, gbm.y=1, family="bernoulli", tree.complexity=10, learning.rate=0.01, bag.fraction=0.6, n.folds=10, prev.stratify=FALSE); summary(brt.tc10.lr01.simp)
summary(brt.tc10.lr01.simp)

summary(brt.tc10.lr01,
         cBars=length(brt.tc10.lr01$var.names),
         n.trees=brt.tc10.lr01$n.trees,
         plotit=TRUE,
         order=TRUE,
         method=relative.influence,
         normalize=TRUE,
         las=1,
         cex.lab=1)

summary(brt.tc10.lr01.simp,
        cBars=length(brt.tc10.lr01.simp$var.names),
        n.trees=brt.tc10.lr01.simp$n.trees,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE,
        las=1)

# No Cliffs Experiment
rf.data.noCliffs <- rf.data[,-22]
brt.tc10.lr01.nocliffs <- gbm.step(data=rf.data.noCliffs, gbm.x=2:21, gbm.y=1, family="bernoulli", tree.complexity=10, learning.rate=0.01, bag.fraction=0.6, n.folds=10, prev.stratify=FALSE); summary(brt.tc10.lr01.nocliffs)
# AUC 0.978 0.001
brt.nocliffs.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=brt.tc10.lr01.nocliffs, n.trees=brt.tc10.lr01.nocliffs$gbm.call$best.trees); brt.nocliffs.cal
# cAUC

#Check to see if dropping variables improves predictive performance. #It doesn't.
set.seed(1)
brt.simp <- gbm.simplify(brt.tc10.lr01, n.folds=10, n.drops="auto", plot=TRUE); names(brt.simp)
brt.simp$deviance.summary 
brt.simp$drop.count; brt.simp$final.drops 

#Plot the Interactions
brt.int <- gbm.interactions(brt.tc10.lr01); names(brt.int)
brt.int$interactions; brt.int$rank.list  #See the largest interactions; 

# Model Evaluation: Uncalibrated AUC
names(brt.tc10.lr01)
brt.tc10.lr01$cv.statistics
brt.tc10.lr01$cv.roc.matrix
brt.ev <- mean(brt.tc10.lr01$cv.roc.matrix); brt.ev; sd(brt.tc10.lr01$cv.roc.matrix)

# Model Evaluation: Calibrated AUC
brt.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=brt.tc10.lr01, n.trees=brt.tc10.lr01$gbm.call$best.trees); brt.cal
brt.cal <- 0.9749554

#Plot the functions & fitted values from the model
#gbm.plot(brt.tc5.lr05, n.plots=10, show.contrib=TRUE)

#Plot fitted values in relation to each of the predictors (better representation than gbm.plot)
#gbm.plot.fits(rf.tc4.lr05.bf.5)


#pretty.gbm.tree(brt.tc5.lr05, i.tree=1) #first split reduces loss function (bernoulli loss function) the most (compare to final tree, tree #1000, which barely reduces loss function)
#print.gbm(brt.tc5.lr05)

#gbm.perspec(brt.tc5.lr05, x=1, y=6, x.range=c(-25, 30), x.label='Annual Mean Temperature', y.range=c(0, 1.0), y.label='Temperature Seasonality', z.range=c(0,1.0), theta=55, phi=40)
#gbm.perspec(brt.tc5.lr05, x=1, y=8, x.range=c(-25, 30), x.label='Annual Mean Temperature', y.range=c(10, 45), y.label='Temperature Annual Range', z.range=c(0,1.0), theta=55, phi=40)
#gbm.perspec(brt.tc5.lr05, x=9, y=4, x.range=c(0, 1.0), x.label='Suitable Cliffs', y.range=c(0, 22.0), y.label='Mean Diurnal Range', z.range=c(0, 0.3), theta=55, phi=40)
#gbm.perspec(brt.tc5.lr05, x=5, y=3, x.range=c(0, 1.0), x.label='Isothermality', y.range=c(0, 400.0), y.label='Precip. Warmest Quarter', z.range=c(0, 0.5), theta=255, phi=40)

########################
#Experiment with various bag fractions; Bag Fractions rarely changed according to De'Ath (2009) and typically range from 0.4 - 0.6.
brt.tc5.lr05.bf.4 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.05, bag.fraction=0.4, n.folds=10, prev.stratify=FALSE)
mean(brt.tc5.lr05.bf.4$cv.roc.matrix) #cross-validation AUC = 0.97799 se = 
evaluate(p=pres_test_cal, a=back_test_cal, model=brt.tc5.lr05.bf.4, n.trees=brt.tc5.lr05.bf.4$gbm.call$best.trees)
brt.tc5.lr05.bf.6 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.05, bag.fraction=0.6, n.folds=10, prev.stratify=FALSE)
mean(brt.tc5.lr05.bf.6$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt.tc5.lr05.bf.6, n.trees=brt.tc5.lr05.bf.6$gbm.call$best.trees)
brt.tc5.lr05.bf.75 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.05, bag.fraction=0.75, n.folds=10, prev.stratify=FALSE)
mean(brt.tc5.lr05.bf.75$cv.roc.matrix) #cross-validation AUC = 0.97621 se = 0.002; 1400 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt.tc5.lr05.bf.75, n.trees=brt.tc5.lr05.bf.75$gbm.call$best.trees)

#Experiments with varying the number of nodes in a single tree (i.e. tree complexity)
set.seed(1)
brt_test_tc1 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=1, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc1$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc1, n.trees=brt_test_tc1$gbm.call$best.trees)
set.seed(1)
brt_test_tc2 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=2, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc2$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc2, n.trees=brt_test_tc2$gbm.call$best.trees)
set.seed(1)
brt_test_tc3 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=3, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc3$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc3, n.trees=brt_test_tc3$gbm.call$best.trees)
set.seed(1)
brt_test_tc4 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=4, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc4$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc4, n.trees=brt_test_tc4$gbm.call$best.trees)
set.seed(1)
brt_test_tc5 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc5$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc5, n.trees=brt_test_tc5$gbm.call$best.trees)
set.seed(1)
brt_test_tc6 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=6, learning.rate=0.005, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc6$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc6, n.trees=brt_test_tc6$gbm.call$best.trees)
set.seed(1)
brt_test_tc7 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=7, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc7$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc7, n.trees=brt_test_tc7$gbm.call$best.trees)
set.seed(1)
brt_test_tc8 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=8, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc8$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc8, n.trees=brt_test_tc8$gbm.call$best.trees)
set.seed(1)
brt_test_tc9 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=9, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc9$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc9, n.trees=brt_test_tc9$gbm.call$best.trees)
set.seed(1)
brt_test_tc10 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=10, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc10$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc10, n.trees=brt_test_tc10$gbm.call$best.trees)
set.seed(1)
brt_test_tc20 <- gbm.step(data=rf.data, gbm.x=2:22, gbm.y=1, family="bernoulli", tree.complexity=20, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
mean(brt_test_tc20$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
evaluate(p=pres_test_cal, a=back_test_cal, model=brt_test_tc20, n.trees=brt_test_tc20$gbm.call$best.trees)
#########################################################################################################################################
# Random Forests
#########################################################################################################################################
#Optimize 'mtry' value; optimal value for this parameter is 3.  
rf.data.pb <- as.factor(rf.data$pb)
rf.data.reg <- rf.data[,1]
tuneRF(x=rf.data[,-1], y=rf.data.reg, ntreeTry=1000, improve=0.05, trace=TRUE, plot=TRUE) # 'pb' is a factor for random forest classification trees

#Training/testing datasets
pres <- rf.data[rf.data[,1] ==1, 2:22]
back <- rf.data[rf.data[,1] ==0, 2:22]
k <- 10
group <- kfold(pres, k)
group2 <- kfold(back, k)

# Create Random Forest Classification Model (bootstrapped samples w/ replacement) using 10-fold cross-validation; Classification because the response is discrete: presence/background.
rForest.e <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  pb <- as.factor(pb)
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  set.seed(1)
  randomForest <- randomForest(y=rf.train[,1], x=rf.train[,-1], ntree=1000, mtry=8, nodesize=1, importance=FALSE, proximity=FALSE) #importance & proximity look at randomForest.inquiry below
  rForest.e[[i]] <- evaluate(p=pres.test, a=back.test, model=randomForest)
}

# Random Forest (Classification) Model Evaluation: Uncalibrated AUC
summary(randomForest) #All predictors (model terms) are statistically significant
randomForest.auc <- sapply(rForest.e, function(x){slot(x, 'auc')})
randomForest.ev <- mean(randomForest.auc); randomForest.ev; sd(randomForest.auc)

randomForest.cor <- sapply(rForest.e, function(y){slot(y, 'cor')})
randomForest_cor <- mean(randomForest.cor); randomForest_cor
sd(randomForest.cor)

# Random Forest (Classification) Model Evaluation: Calibrated AUC
randomForest.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=randomForest); randomForest.cal
randomForest.cal <- 0.9482759

plot(randomForest)

randomForest.var.importance <- importance(randomForest.reg) #extractor function for variable importance from function, randomForest when importance=TRUE
# Provides the 'vote' for the most common value for each predictor of the two classes: presence or background
#1st Measure given, 'Accuracy' is from 'permuting out-of-bag data. The error rate is recorded.
#2nd Measure given, 'Total decrease in node impurities from splitting on the variable, averaged over all trees'.
# Node impurity is measured by the Gini Index for Classification Trees.

randomForest.inquiry <- randomForest
#impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
#op <- par(mfrow=c(2, 3))
#for (i in seq_along(impvar)) {
#  partialPlot(randomForest, rf.data, impvar[i], xlab=impvar[i], main=paste("Partial Dependence on", impvar[i]), 
#              ylim=c(0,70))
#}


#plot(margin(randomForest)) # larger magnitude, positive values give greater confidence in the classifier.
# margin converges to 1, which is good (not sure if that's how to interpret)

#MDSplot(randomForest, rf.data[, 1]) # requires 'proximity=TRUE' in randomForest function

varImpPlot2(randomForest.reg) # "dotchart of variable importance as measured by a Random Forest"
#partialPlot(randomForest, rf.data, x.var=cliffs, which.class="1")  #confused...



# Create Random Forest Regression Model (bootstrapped sample w/ replacement) using 10-fold cross-validation; Regression Trees.
rForest.e.reg <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  set.seed(1)
  randomForest.reg <- randomForest(y=rf.train[, 1], x=rf.train[, -1], ntree=1000, mtry=7, replace=TRUE, importance=FALSE)
  #rForest.e.reg[[i]] <- evaluate(p=pres.test, a=back.test, model=randomForest.reg)
}
randomForest.reg


varImpPlot2(randomForest.reg) 


randomForest.reg.nocv <- randomForest(y=rf.data[, 1], x=rf.data[, -1], ntree=1000, mtry=7, replace=TRUE, importance=TRUE)
summary(randomForest.reg.nocv)
varImpPlot2(randomForest.reg.nocv)
importance(randomForest.reg, type=1)
# Random Forest (Regression) Model Evaluation: Uncalibrated AUC
summary(randomForest.reg) #All predictors (model terms) are statistically significant
randomForest.reg.auc <- sapply(rForest.e.reg, function(x){slot(x, 'auc')})
sd(randomForest.reg.auc)
randomForest_reg_ev <- mean(randomForest.reg.auc); randomForest_reg_ev

randomForest.reg.cor <- sapply(rForest.e.reg, function(y){slot(y, 'cor')})
randomForest_reg_cor <- mean(randomForest.reg.cor); randomForest_reg_cor
sd(randomForest.reg.cor)

# Random Forest (Classification) Model Evaluation: Calibrated AUC
randomForest.reg.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=randomForest.reg); randomForest.reg.cal


#### No Cliffs Experiment
rf.data.noCliffs <- rf.data[,-22]
tuneRF(x=rf.data.noCliffs[,-1], y=rf.data.noCliffs[,1], ntreeTry=1000, improve=0.05, trace=TRUE, plot=TRUE)
randomForest.reg.nocliffs.nocv <- randomForest(y=rf.data[, 1], x=rf.data[, c(-1,-22)], ntree=1000, mtry=6, replace=TRUE, importance=TRUE)

pres.nocliffs <- rf.data.noCliffs[rf.data.noCliffs[,1] ==1, 2:21]
back.nocliffs <- rf.data.noCliffs[rf.data.noCliffs[,1] ==0, 2:21]
k <- 10
set.seed(1)
group.nocliffs <- kfold(pres.nocliffs, k)
set.seed(1)
group2.nocliffs <- kfold(back.nocliffs, k)

rForest.e.reg.nocliffs <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres.nocliffs[group.nocliffs !=i, ]
  back.train <- back.nocliffs[group2.nocliffs !=i, ]
  pres.test <- pres.nocliffs[group.nocliffs == i, ]
  back.test <- back.nocliffs[group2.nocliffs == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  set.seed(1)
  randomForest.reg.nocliffs <- randomForest(y=rf.train[, 1], x=rf.train[, -1], ntree=1000, mtry=6, replace=TRUE, importance=TRUE)
  rForest.e.reg.nocliffs[[i]] <- evaluate(p=pres.test, a=back.test, model=randomForest.reg.nocliffs)
}

randomForest.reg.nocliffs.auc <- sapply(rForest.e.reg.nocliffs, function(x){slot(x, 'auc')})
sd(randomForest.reg.nocliffs.auc)
# AUC 0.978 (0.004)
randomForest_reg_ev_nocliffs <- mean(randomForest.reg.nocliffs.auc); randomForest_reg_ev_nocliffs
varImpPlot2(randomForest.reg.nocliffs)

randomForest.nocliffs.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=randomForest.reg.nocliffs); randomForest.nocliffs.cal
# cAUC 0.979

##############################################################################################################################################################################################
##############################################################################################################################################################################################
# Generalized Additive Models, GAMs
##############################################################################################################################################################################################
##############################################################################################################################################################################################
gam.e.all <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  gam.all <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio6) + s(bio7) + s(bio8) + s(bio9) + s(bio10) + s(bio11) + s(bio12) + s(bio13) + s(bio14) + s(bio15) + s(bio16) + s(bio17) + s(bio18) + s(bio19) + cliffs + vegetation, family=binomial(link="logit"), data=rf.train)
  gam.e.all[[i]] <- evaluate(p=pres.test, a=back.test, model=gam.all)
}


AIC(gam.all, k=2) # AIC= (all predictors)

# GAM; dropping 'vegetation'
gam.e.reduced <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  gam.reduced <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio6) + s(bio7) + s(bio8) + s(bio9) + s(bio10) + s(bio11) + s(bio12) + s(bio13) + s(bio14) + s(bio15) + s(bio16) + s(bio17) + s(bio18) + s(bio19) + cliffs, family=binomial(link="logit"), data=rf.train)
  gam.e.reduced[[i]] <- evaluate(p=pres.test, a=back.test, model=gam.reduced)
}

AIC(gam.reduced, k=2) # AIC= (vegetation is dropped); # Akaike's 'An Information Criterion' for which log-likelihood value can be obtained, according to the formula: -2*log-likelihood + k*npar where npar=# of parameters in fitted model and k=2 for AIC or k=log(n) for BIC (k is the penalty per term to be used)

# Model Evaluation: Uncalibrated AUC
gam.auc <- sapply(gam.e.all, function(x){slot(x, 'auc')} )
gam.ev <- mean(gam.auc); gam.ev
sd(gam.auc)
gam.auc.reduced <- sapply(gam.e.reduced, function(x){slot(x, 'auc')} )
gam.ev.reduced <- mean(gam.auc.reduced); gam.ev.reduced

# Model Evaluation: Calibrated AUC
gam.cal <- evaluate(p=pres_test_cal, a=back_test_cal, model=gam.all); gam.cal
gam.cal.reduced <- evaluate(p=pres_test_cal, a=back_test_cal, model=gam.reduced); gam.cal.reduced
gam.cal<-.7729637



#Visualize how Response (the logit) varies with pairs of two variables.
# READ ME: ?vis.gam states "Note that variables coerced to factors in the model formula won't work as view variables, and 
## vis.gam can not detect that this has happened when setting defaults."
#vis.gam(gam.all, view=c("bio1", "cliffs"), plot.type="persp", color="cm", theta=50, type="response", ticktype="detailed")
#vis.gam(gam.all, view=c("bio3", "cliffs"), plot.type="persp", color="cm", theta=-50, type="response", ticktype="detailed")
#vis.gam(gam.all, view=c("bio5", "cliffs"), plot.type="persp", color="cm", theta=-50, type="response", ticktype="detailed")
#vis.gam(gam.all, view=c("bio1", "vegetation"), plot.type="persp", color="cm", theta=50, type="response", ticktype="detailed")
#vis.gam(gam.all, view=c("bio1", "bio2"), plot.type="persp", color="cm", theta=70, type="response", ticktype="detailed")
##############################################################################################################################################################################################
##############################################################################################################################################################################################
# MaxEnt
##############################################################################################################################################################################################
##############################################################################################################################################################################################
Sys.setenv(NOAWT=TRUE)  # Run before Loading 'rJava'
options(java.parameters="-Xmx15g") # Run before Loading 'dismo'

pres.maxent <- rf.points[,-3]   # rf.points was created above at spatial sorting bias section
back.maxent <- background
k <- 10
group.max <- kfold(pres.maxent, k)
group2.max <- kfold(back.maxent, k)

###############################
# All Feature Types allowed ###
###############################
maxent.e.April2 <- list()
for (i in 1:k) {
  pres.train <- pres.maxent[group.max !=i, ]
  back.train <- back.maxent[group2.max !=i, ]
  pres.test <- pres.maxent[group.max == i, ]
  back.test <- back.maxent[group2.max == i, ]
  maxent.April2 <- maxent(x=predictors, p=pres.train, a=back.train, factors=c('cliffs', 'biome'), removeDuplicates=TRUE, args=c("writebackgroundpredictions=TRUE"), progress='text', path="./MaxentOutput_R/TEST")
  maxent.e.April2[[i]] <- evaluate(p=pres.test, a=back.test, model=maxent, x=predictors, factors=c('cliffs', 'biome'))
}
summary(maxent)

# Model Evaluation: Uncalibrated AUC
maxent.auc <- sapply(maxent.e, function(x){slot(x, 'auc')} )
sd(maxent.auc)
maxent.ev <- mean(maxent.auc); maxent.ev


maxent.kappa <- sapply(maxent.e, function(x){slot(x,'kappa')})
maxent.kappa <- mean(maxent.kappa); maxent.kappa

# Model Evaluation: Calibrated AUC
maxent.cal <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.April, x=predictors); maxent.cal  # use 'pres_test_pwd' b/c need only X,Y coordinates, not environmental values at these points.
maxent.cal2 <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.April, x=predictors, factors=c('cliffs', 'biome')); maxent.cal2  
maxent.cal3 <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.April2, x=predictors); maxent.cal3  
maxent.cal4 <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.April2, x=predictors, factors=c('cliffs', 'biome')); maxent.cal4  



maxent.cal <- 0.7658294
#rm(group.max); rm(group2.max); rm(pres.maxent); rm(back.maxent)
###############################################################
# Experiment with only Hinge & Categorical Features Allowed ###
###############################################################
maxent.e.reduced <- list()
for (i in 1:k) {
  pres.train <- pres.maxent[group.max !=i, ]
  back.train <- back.maxent[group2.max !=i, ]
  pres.test <- pres.maxent[group.max == i, ]
  back.test <- back.maxent[group2.max == i, ]
  maxent.reduced <- maxent(x=predictors, p=pres.train, a=back.train, factors=c('cliffs', 'vegetation'), args=c("nolinear", "noquadratic", "noproduct", "nothreshold", "-J", "-P", "addsamplestobackground=FALSE", "writebackgroundpredictions=TRUE"), removeDuplicates=TRUE, path="./MaxentOutput_R/OnlyHingeFeatures")
  maxent.e.reduced[[i]] <- evaluate(p=pres.test, a=back.test, model=maxent.reduced, x=predictors, factors=c('cliffs', 'vegetation'))
}
summary(maxent.reduced)

# Model Evaluation: Uncalibrated AUC
maxent.hinge.auc <- sapply(maxent.e.reduced, function(x){slot(x, 'auc')} )
maxent.hinge.ev <- mean(maxent.hinge.auc); maxent.hinge.ev

# Model Evaluation: Calibrated AUC
maxent.hinge.cal <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.reduced, x=predictors); maxent.hinge.cal  # use 'pres_test_pwd' b/c need only X,Y coordinates, not environmental values at these points.

##########################################################################
# Experiment with only Linear, Quadratic, Categorical Features Allowed ###
##########################################################################
maxent.e.reduced2 <- list()
for (i in 1:k) {
  pres.train <- pres.maxent[group.max !=i, ]
  back.train <- back.maxent[group2.max !=i, ]
  pres.test <- pres.maxent[group.max == i, ]
  back.test <- back.maxent[group2.max == i, ]
  maxent.reduced2 <- maxent(x=predictors, p=pres.train, a=back.train, factors=c('cliffs', 'vegetation'), args=c("nohinge", "noproduct", "nothreshold", "-J", "-P", "addsamplestobackground=FALSE", "writebackgroundpredictions=TRUE"), removeDuplicates=TRUE, path="./MaxentOutput_R/LinearQuadraticFeatures")
  maxent.e.reduced2[[i]] <- evaluate(p=pres.test, a=back.test, model=maxent.reduced2, x=predictors, factors=c('cliffs', 'vegetation'))
}
summary(maxent.reduced2)

# Model Evaluation: Uncalibrated AUC
maxent.lq.auc <- sapply(maxent.e.reduced2, function(x){slot(x, 'auc')} )
maxent.lq.ev <- mean(maxent.lq.auc); maxent.lq.ev

# Model Evaluation: Calibrated AUC
maxent.lq.cal <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent.reduced2, x=predictors); maxent.lq.cal  # use 'pres_test_pwd' b/c need only X,Y coordinates, not environmental values at these points.

# "randomtestpercentage=25" : this model evaluation technique is the 'holdout approach'. Higher variance than k-fold cross-validation.

######################################################################################################################################################################################
#Combining Models
######################################################################################################################################################################################
# Prior to getting Maxent to run in R, I just read in output from Maxent (from the java program).
#First read in Maxent model
#rf.current.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP85_2070_bkgr_10000_addnoSamples/RosyFinch_avg.grd")
#proj4string(rf.current.maxent)
#proj4string(rf.current.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#ev_maxent <- 0.965

#rf.RCP26_2070.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP26_2070_bkgr_10000_addnoSamples/RosyFinch_RCP26_2070_avg.grd")
#proj4string(rf.RCP26_2070.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#rf.RCP85_2070.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP85_2070_bkgr_10000_addnoSamples/RosyFinch_RCP85_2070_avg.grd")
#proj4string(rf.RCP85_2070.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"



models <- stack(Current.brt, Current.gam, Current.randomForest, Current.maxent)
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")
plot(models)

models2.6 <- stack(RCP26_2070.brt, RCP26_2070.gam, RCP26_2070.rf, RCP26_2070.maxent )
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")
plot(models2.6)

models8.5 <- stack(RCP85_2070.brt, RCP85_2070.gam, RCP85_2070.rf, RCP85_2070.maxent)
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")
plot(models8.5)


# Simple Model Averaging
#m <- mean(models)
#plot(m, main='Average Score')



# Weight the Models by their Uncalibrated AUC Scores
weight <- vector()
weight[1] <- (brt.ev-0.5)^2
weight[2] <- (gam.ev-0.5)^2
weight[3] <- (randomForest.ev-0.5)^2
weight[4] <- (maxent.ev-0.5)^2


# Weight the Models by their Calibrated AUC Scores
weight_cal <- vector()
weight_cal[1] <- (brt.cal-0.5)^2
weight_cal[2] <- (gam.cal-0.5)^2
weight_cal[3] <- (randomForest.cal-0.5)^2
weight_cal[4] <- (maxent.cal-0.5)^2


# Final Prediction based on Uncalibrated AUC Weighted mean ("BRT", "GAM", "Random Forest", and "Maxent")
Uncalibrated.WM.Current <- weighted.mean(models, weight, filename="Uncalibrated_WM_Current.tif", progress='text', format="GTiff", datatype='FLT4S')

# Final Prediction based on Calibrated AUC Weighted mean ("BRT", "GAM", "Random Forest", and "Maxent")
Calibrated.WM.Current <- weighted.mean(models, weight_cal, filename="Calibrated_WM_Current.tif", progress='text', format="GTiff", datatype='FLT4S')


Weight.Mean.Cal.RCP26_2070 <- weighted.mean(models2.6, weight_cal, filename="Calibrated_WM_RCP26_2070.tif", progress='text', format="GTiff", datatype='FLT4S')



Weight.Mean.RCP26_2070 <- weighted.mean(models2.6, weight, filename="Weight_Mean_RCP26_2070.tif", progress='text', format="GTiff", datatype='FLT4S')
Weight.Mean.RCP85_2070 <- weighted.mean(models8.5, weight, filename="Weight_Mean_RCP85_2070.tif", progress='text', format="GTiff", datatype='FLT4S')



Weight.Mean.Cal.RCP85_2070 <- weighted.mean(models8.5, weight_cal, filename="Calibrated_WM_RCP85_2070.tif", progress='text', format="GTiff", datatype='FLT4S')

###########################################################################################################################################################################
###########################################################################################################################################################################
# Null Models
###########################################################################################################################################################################
###########################################################################################################################################################################
# Geographic Distance Null Model that doesn't account for Values of predictors, but only distance/proximity of training/testing points.
pres.null <- rf
back.null <- background
k <- 10
set.seed(1)
group.null <- kfold(pres.null, k)
group2.null <- kfold(back.null, k)

null.e <- list()
for (i in 1:k) {
  pres.test <- pres.null[group.null !=i, ]
  back.train <- back.null[group2.null !=i, ]
  pres.test <- pres.null[group.null == i, ]
  back.test <- back.null[group2.null == i, ]
  distm <- geoDist(p=pres.train, a=back.train, lonlat=FALSE)
  null.e[[i]] <- evaluate(distm, p=pres.test, a=back.test)
}
summary(distm)

plot(distm)

beginCluster()
null.current <- clusterR(vegetation, fun=predict, args=list(model=distm, mask=TRUE), filename="Null_Model_current.tif", format="GTiff", datatype='FLT4S', progress='text')
endCluster()

# Model Evaluation: Uncalibrated AUC
null.auc <- sapply(null.e, function(x){slot(x, 'auc')} )
null.ev <- mean(null.auc); null.ev


# Model Evaluation: Calibrated AUC
null.cal <- evaluate(p=pres_test_pwd, a=back_test_pwd, distm); null.cal  # use 'pres_test_pwd' b/c need only X,Y coordinates, not environmental values at these points.