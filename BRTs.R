#install.packages(c('raster','rJava','dismo','gbm', 'randomForest', 'mgcv', 'rgdal', 'maptools', 'rgeos', 'ROCR', 'vcd', 'boot', 'snow'))

library(raster)
library(dismo) #has BRTs functions adapted by Elith & Leathwick
#library(rJava) #For talking to Maxent
library(rgdal)  #'rgdal' not available for R version 3.1.2
library(gbm)   #Boosted Regression Trees
library(randomForest) #Random Forest
library(mgcv) #Generalized Additive Models
library(snow)
#library(rgeos)
#library(maptools)
#library(ROCR)
#library(boot)
#library(vcd)


setwd("~/Documents/MSc.Thesis/BRTs")
#load("~/Documents/MSc.Thesis/BRTs/BRTs.Rdata")
#load("~/Documents/MSc.Thesis/BRTs/GAM_GLM.Rdata")


#Useful commands to determine versions of R & packages:
#vignette('brt', package="dismo") #there's a version from Sept.9,2014
#vignette('sdm', package="dismo") #there's a version from Sept.9,2014
#sessionInfo()
#version$version.string   #or just type 'version'  ; in a shell, type 'R --version'
#packageVersion("dismo")

##################################################################################################################
# Read in Datasets
##################################################################################################################
rf<-read.csv("RosyFinch.csv")
rf<-rf[,-1]

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
set.seed(1) #set seed to assure that models will always have same random sample

#Sample 10,000 random points that are not 'NA' in Vegetation & exclude presence locations.
background<-randomPoints(vegetation, excludep=TRUE, n=10000, tryf=10, warn=2) 
background<-as.data.frame(background)
dim(background) #10,000 points

#####################################################################################################################################
# Create Data frame with values at Presence and Background      
#####################################################################################################################################
presvals <- extract(predictors, rf)
bckgvals <- extract(predictors, background)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(bckgvals)))
rf.data <- data.frame(cbind(pb, rbind(presvals, bckgvals)))
rf.data[,'cliffs'] <- as.factor(rf.data[,'cliffs'])
rf.data[,'vegetation'] <- as.factor(rf.data[,'vegetation'])
dim(rf.data)  # 10,000 random background points plus 1,589 Rosy-Finch occurrences.
str(rf.data) # cliffs', and 'vegetation' are designated factors.
#########################################################################################################################################
# Boosted Regression Trees (aka "Gradient Boosting Machine", "Gradient Boost", "Stochastic Gradient Boosting", "Gradient Tree Boosting"
#########################################################################################################################################
#gbm.step estimates the best number of trees to be used with the model using 10-fold cross-validation.
#Took >4000 trees to acheive lowest deviance
#rf.tc5.lr01 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.01, bag.fraction=0.5)
#Only 850 trees to acheive lowest deviance
#rf.tc5.lr05 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.05, bag.fraction=0.5)

#Took 1950 trees to acheive lowest deviance; Model is built using 80% of the dataset (i.e. 'envtrain')
#Note that the output is on a logit scale (Figure 6, p. 809; Elith et al., 2009)
#gbm.step function does the following:
# 1) Randomly divide available data (rf.data) into 10 subsets
# 2) Makes 10 different training sets comprised of 9 subsets. Each training set therefore has a unique
#ommitted testing dataset.
# 3) Cross-validation model fitting 
#rf.tc3.lr05.bf.5 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=3, learning.rate=0.05, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
#mean(rf.tc3.lr05.bf.5$cv.roc.matrix)

set.seed(1)
rf.tc5.lr04.bf.5 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.04, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
#rf.tc7.lr03.bf.5 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=7, learning.rate=0.03, bag.fraction=0.5, n.folds=10, prev.stratify=FALSE)
#mean(rf.tc7.lr03.bf.5$cv.roc.matrix) #AUC goes up to 0.97712 from 0.97637

names(rf.tc5.lr04.bf.5)
rf.tc5.lr04.bf.5$cv.statistics
rf.tc5.lr04.bf.5$cv.roc.matrix
ev_brt <- mean(rf.tc5.lr04.bf.5$cv.roc.matrix) #cross-validation AUC = 0.97647 se = 0.002; 1650 trees

#Experiment with various bag fractions
#rf.tc5.lr04.bf.6 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.04, bag.fraction=0.6, n.folds=10)
#mean(rf.tc5.lr04.bf.6$cv.roc.matrix) #cross-validation AUC = 0.97659 se = 0.001; 1250 trees
#rf.tc5.lr04.bf.75 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.04, bag.fraction=0.75, n.folds=10)
#mean(rf.tc5.lr04.bf.75$cv.roc.matrix) #cross-validation AUC = 0.97704 se = 0.002; 1550 trees
#rf.tc5.lr04.bf1.00 <- gbm.step(data=rf.data, gbm.x=2:11, gbm.y=1, family="bernoulli", tree.complexity=5, learning.rate=0.04, bag.fraction=1.00, n.folds=10)
#mean(rf.tc5.lr04.bf1.00$cv.roc.matrix) #cross-validation AUC = 0.97621 se = 0.002; 1400 trees


#Check to see if dropping variables improves predictive performance. #It doesn't.
set.seed(1)
rf.simp <- gbm.simplify(rf.tc5.lr04.bf.5, n.folds=10, n.drops="auto", plot=TRUE)
names(rf.simp)
rf.simp$deviance.summary #shows that deviance increases rather than decreasing by dropping variables
#NO Variables should be dropped based on this function.
rf.simp$final.drops #shows that bio5 and vegetation contribute the least
summary(rf.tc5.lr04.bf.5) #shows that bio5 and vegetation contribute the least

#Plot the functions & fitted values from the model
gbm.plot(rf.tc5.lr04.bf.5, n.plots=10, show.contrib=TRUE)

#Plot fitted values in relation to each of the predictors (better representation than gbm.plot)
#gbm.plot.fits(rf.tc5.lr03)

#Plot the Interactions
find.int <- gbm.interactions(rf.tc5.lr04.bf.5)
names(find.int)
find.int$interactions #Gives the values for all interactions
find.int$rank.list  #See the largest interactions; #shows that there is a significant interaction btw bio3 & bio5. #don't drop bio5!

#pretty.gbm.tree(rf.tc5.lr04.bf.5, i.tree=1) #first split reduces loss function (bernoulli loss function) the most (compare to final tree, tree #1000, which barely reduces loss function)
#print.gbm(rf.tc5.lr04.bf.5)

#gbm.perspec(rf.tc5.lr04.bf.5, x=1, y=6, x.range=c(-25, 30), x.label='Annual Mean Temperature', y.range=c(0, 1.0), y.label='Temperature Seasonality', z.range=c(0,1.0), theta=55, phi=40)
#gbm.perspec(rf.tc5.lr04.bf.5, x=1, y=8, x.range=c(-25, 30), x.label='Annual Mean Temperature', y.range=c(10, 45), y.label='Temperature Annual Range', z.range=c(0,1.0), theta=55, phi=40)
#gbm.perspec(rf.tc5.lr04.bf.5, x=9, y=4, x.range=c(0, 1.0), x.label='Suitable Cliffs', y.range=c(0, 22.0), y.label='Mean Diurnal Range', z.range=c(0, 0.3), theta=55, phi=40)
#gbm.perspec(rf.tc5.lr04.bf.5, x=5, y=3, x.range=c(0, 1.0), x.label='Isothermality', y.range=c(0, 400.0), y.label='Precip. Warmest Quarter', z.range=c(0, 0.5), theta=255, phi=40)



# BRTs Spatial Predictions (Current, RCP2.6_2070, and RCP8.5_2070 Rosy-Finch distributions)
library(snow)
beginCluster()
rf.current.brt <- clusterR(predictors, fun=predict, args=list(model=rf.tc5.lr04.bf.5, factors=c('cliffs','vegetation'), type="response", n.trees=rf.tc5.lr04.bf.5$gbm.call$best.trees), progress='text', filename="rf_current_brt.tif", format="GTiff", datatype='FLT4S')
rf.RCP26_2070.brt <- clusterR(pred.26.2070, fun=predict, args=list(model=rf.tc5.lr04.bf.5, factors=c('cliffs','vegetation'), type="response", n.trees=rf.tc5.lr04.bf.5$gbm.call$best.trees), progress='text', filename="rf_RCP26_2070_brt.tif", format="GTiff", datatype='FLT4S')
rf.RCP85_2070.brt <- clusterR(pred.85.2070, fun=predict, args=list(model=rf.tc5.lr04.bf.5, factors=c('cliffs','vegetation'), type="response", n.trees=rf.tc5.lr04.bf.5$gbm.call$best.trees), progress='text', filename="rf_RCP85_2070_brt.tif", format="GTiff", datatype='FLT4S')
endCluster()
#########################################################################################################################################
# Random Forests
#########################################################################################################################################
#Optimize 'mtry' value; optimal value for this parameter is 3.  
#tuneRF(rf.data[,-1], rf.data$pb, ntreeTry=1000, improve=0.05, trace=TRUE, plot=TRUE) # 'pb' is a factor for random forest classification trees

#Training/testing datasets
pres <- rf.data[rf.data[,1] ==1, 2:11]
back <- rf.data[rf.data[,1] ==0, 2:11]
k <- 10
set.seed(1)
group <- kfold(pres, k)
group2 <- kfold(back, k)

# Create Random Forest model using 10-fold cross-validation
rForest.e <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.randomForest <- randomForest(pb ~ ., data=rf.train, ntree=1000, mtry=3)
  rForest.e[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.randomForest)
}

#varImpPlot(rf.randomForest)
#partialPlot(rf.randomForest, rf.data, cliffs)  #confused...

#Evaluation
summary(rf.randomForest) #All predictors (model terms) are statistically significant
rf.randomForest.auc <- sapply(rForest.e, function(x){slot(x, 'auc')} )
ev_rf <- mean(rf.randomForest.auc)
#str(rForest.e)

# Random Forest Spatial Prediction Maps
beginCluster()
rf.current.rf <- clusterR(predictors, fun=predict, args=list(model=rf.randomForest, factors=c('cliffs', 'vegetation'), type="response", na.rm=TRUE), progress='text', filename="rf_current_rForest.tif", format="GTiff", datatype='FLT4S')
rf.RCP26_2070.rf <- clusterR(pred.26.2070, fun=predict, args=list(model=rf.randomForest, factors=c('cliffs', 'vegetation'), type="response", na.rm=TRUE), progress='text', filename="rf_RCP26_2070_rForest.tif", format="GTiff", datatype='FLT4S')
rf.RCP85_2070.rf <- clusterR(pred.85.2070, fun=predict, args=list(model=rf.randomForest, factors=c('cliffs', 'vegetation'), type="response", na.rm=TRUE), progress='text', filename="rf_RCP85_2070_rForest.tif", format="GTiff", datatype='FLT4S')
endCluster()
##############################################################################################################################################################################################
##############################################################################################################################################################################################
# Generalized Additive Models, GAMs
##############################################################################################################################################################################################
##############################################################################################################################################################################################
gam.e <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) +s(bio7) + s(bio15) + s(bio18) + cliffs, family=binomial(link="logit"), data=rf.train)
  gam.e[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam)
}

# GAM Evaluation
summary(rf.gam) #All predictors (model terms) are statistically significant except 'vegetation'.
rf.gam.auc <- sapply(gam.e, function(x){slot(x, 'auc')} )
ev_gam <- mean(rf.gam.auc)
ev_gam

# GAM Spatial Predictions:
#install.packages("snow") #multi-core computing
library(snow)
beginCluster()
rf.current.gam <- clusterR(predictors, fun=predict, args=list(model=rf.gam, factors=c('cliffs'), type="response"), progress='text', filename="rf_current_gam.tif", format="GTiff", datatype='FLT4S')
rf.RCP26_2070.gam <- clusterR(pred.26.2070, fun=predict, args=list(model=rf.gam, factors=c('cliffs'), type="response"), progress='text', filename="rf_RCP26_2070_gam.tif", format="GTiff", datatype='FLT4S')
rf.RCP85_2070.gam <- clusterR(pred.85.2070, fun=predict, args=list(model=rf.gam, factors=c('cliffs'), type="response"), progress='text', filename="rf_RCP85_2070_gam.tif", format="GTiff", datatype='FLT4S')
endCluster()

#Visualize how Response (the logit) varies with pairs of two variables.
# READ ME: ?vis.gam states "Note that variables coerced to factors in the model formula won't work as view variables, and 
## vis.gam can not detect that this has happened when setting defaults."
#vis.gam(rf.gam, view=c("bio1", "cliffs"), plot.type="persp", color="cm", theta=50, type="response", ticktype="detailed")
#vis.gam(rf.gam, view=c("bio3", "cliffs"), plot.type="persp", color="cm", theta=-50, type="response", ticktype="detailed")
#vis.gam(rf.gam, view=c("bio5", "cliffs"), plot.type="persp", color="cm", theta=-50, type="response", ticktype="detailed")
#vis.gam(rf.gam, view=c("bio1", "vegetation"), plot.type="persp", color="cm", theta=50, type="response", ticktype="detailed")
#vis.gam(rf.gam, view=c("bio1", "bio2"), plot.type="persp", color="cm", theta=70, type="response", ticktype="detailed")
######################################################################################################################################################################################
#Combining Models
######################################################################################################################################################################################
#First read in Maxent model
rf.current.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP85_2070_bkgr_10000_addnoSamples/RosyFinch_avg.grd")
proj4string(rf.current.maxent)
proj4string(rf.current.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ev_maxent <- 0.965


rf.RCP26_2070.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP26_2070_bkgr_10000_addnoSamples/RosyFinch_RCP26_2070_avg.grd")
proj4string(rf.RCP26_2070.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

rf.RCP85_2070.maxent <- raster("~/Documents/MSc.Thesis/Maxent/Output/BioclimCliff/RCP85_2070_bkgr_10000_addnoSamples/RosyFinch_RCP85_2070_avg.grd")
proj4string(rf.RCP85_2070.maxent) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

rf.current.brt <- raster("rf_current_brt.tif")
models <- stack(rf.current.brt, rf.current.gam, rf.current.rf, rf.current.maxent)
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")
plot(models)

models2.6 <- stack(rf.RCP26_2070.brt, rf.RCP26_2070.gam, rf.RCP26_2070.rf, rf.RCP26_2070.maxent )
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")

models8.5 <- stack(rf.RCP85_2070.brt, rf.RCP85_2070.gam, rf.RCP85_2070.rf, rf.RCP85_2070.maxent)
names(models) <- c("BRT", "GAM", "Random Forest", "Maxent")



# Simple Model Averaging
#m <- mean(models)
#plot(m, main='Average Score')

# Weight the Models by their AUC Scores
weight <- vector()
weight[1] <- (ev_brt-0.5)^2
weight[2] <- (ev_gam-0.5)^2
weight[3] <- (ev_rf-0.5)^2
weight[4] <- (ev_maxent-0.5)^2

# Final Prediction based on weighted mean ("BRT", "GAM", "Random Forest", and "Maxent")
rf.weight.mean.current <- weighted.mean(models, weight, filename="RosyFinch_weighted_mean_current.tif", progress='text', format="GTiff", datatype='FLT4S')
rf.weight.mean.RCP26_2070 <- weighted.mean(models2.6, weight, filename="RosyFinch_weight_mn_RCP26_2070.tif", progress='text', format="GTiff", datatype='FLT4S')
rf.weight.mean.RCP85_2070 <- weighted.mean(models8.5, weight, filename="RosyFinch_weight_mn_RCP85_2070.tif", progress='text', format="GTiff", datatype='FLT4S')

#########################################################################################
#########################################################################################
# GLMs
#########################################################################################
#head(envtrain)
#rf.glm <- glm(pb~bio1+bio2+bio3+bio4+bio5+bio7+bio15+bio18+cliffs+vegetation, family=binomial(link="logit"), data=rf.data)
#summary(rf.glm)
#coef(rf.glm)
#anova(rf.glm, test='Chisq') #shows the deviance in the Null model, as well as the deviance as each term is added.

#Look at AUC, cor, and 'max TPR+TNR'
#glm1_ev <- evaluate(testpres, testbackg, glm1)

#pred_pres_glm1 <- predict(predictors, glm1, factors=c('cliffs', 'vegetation'), na.rm=TRUE, ext='vegetation', progress='text', filename="rf_current_glm.tif", format="GTiff", datatype='FLT4S')
#par(mfrow=c(1,2))
#plot(pred_pres_glm1, main='GLM/binomial, raw values')