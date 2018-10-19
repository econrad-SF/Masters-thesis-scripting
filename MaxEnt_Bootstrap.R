library(PresenceAbsence)
library(dismo)
library(raster)
setwd("~/Documents/MSc.Thesis/Models")
########################################################################################
#Training/testing datasets

#pres.maxent <- rf.points[,-3]   # rf.points was created above at spatial sorting bias section
#back.maxent <- background
k <- 10
group.max <- kfold(pres.maxent, k)
group2.max <- kfold(back.maxent, k)

# Background Points
#Read in Current Environmental Layers
files.current<-list.files(path='./Current_Res_828x828m', pattern='tif', full.names=TRUE)
predictors <- stack(files.current); rm(files.current)

maxent.e <- list()
for (i in 1:k) {
  pres.train <- pres.maxent[group.max !=i, ]
  back.train <- back.maxent[group2.max !=i, ]
  pres.test <- pres.maxent[group.max == i, ]
  back.test <- back.maxent[group2.max == i, ]
  maxent <- maxent(x=predictors, p=pres.train, a=back.train, factors=c('cliffs', 'biome'), removeDuplicates=TRUE, args=c("writebackgroundpredictions=TRUE"), progress='text', path="./MaxentOutput_R/Bootstrap")
  maxent.e[[i]] <- evaluate(p=pres.test, a=back.test, model=maxent, x=predictors, factors=c('cliffs', 'biome'))
}

########################################################################################
# Prepare Dataset to calculate AUC & Kappa (BRT Model is the 'brt.tc10.lr01' object).

# Plot ID
setwd("~/Documents/MSc.Thesis/Models/MaxentOutput_R/Bootstrap")
bckgr_pred <- read.csv("species_backgroundPredictions.csv"); names(bckgr_pred)
bckgr_pred <- bckgr_pred$logistic
length(bckgr_pred); class(bckgr_pred); typeof(bckgr_pred); is.atomic(bckgr_pred)

pres_pred <- read.csv("species_samplePredictions.csv"); names(pres_pred)
pres_pred <- pres_pred$Logistic.prediction
length(pres_pred); class(pres_pred); typeof(pres_pred); is.atomic(pres_pred)

# Plot ID
plotID <- as.numeric(seq(1:10255))
class(plotID); length(plotID); is.numeric(plotID)

# Presences - 1,258 + background pts. 8,997 = 10,255
observed <- c(rep(1,1258),rep(0,8997))
class(observed); length(observed); is.atomic(observed); head(observed)

# Predicted Values at all these points
preds <- c(pres_pred, bckgr_pred)
class(preds); length(preds); is.atomic(preds); head(preds)

# Create Data Frame for using PresenceAbsence Package
maxent.data <- data.frame(cbind(plotID, observed, preds))

# AUC 
nbit = 10000
pres.obs.auc <- dim(maxent.data[maxent.data$observed==1, ])[1]
back.obs.auc <- dim(maxent.data[maxent.data$observed==0, ])[1]
maxent.auc <- rep(NA, nbit)
for (i in 1:nbit) {
  sdm.pres.boot <- maxent.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- maxent.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  maxent.auc[i] <- auc(sdm.data, st.dev=FALSE, which.model=1, na.rm=TRUE)  # calculate AUC using the 'PresenceAbsence' package; remove the 4 NA's.
}  

mean(maxent.auc) #0.976
sd(maxent.auc)
quantile(maxent.auc, c(0.025, 0.975))  # 2.5%: 0.9732237 / 97.5%: 0.9782142 
median(maxent.auc) #0.976

# Kappa
nbit <-10000
maxent.confusion <- array(data=NA, dim=c(2,2,nbit))
maxent.Kappa <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- maxent.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- maxent.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  maxent.confusion[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Kappa using 'PresenceAbsence' package)
  maxent.Kappa[i] <- Kappa(maxent.confusion[,,i], st.dev=FALSE)     # Calculate the Kappa Statistic
}

mean(maxent.Kappa); sd(maxent.Kappa) #0.746
quantile(maxent.Kappa, c(0.025, 0.975))  # 2.5%: 0.7305359 / 97.5%: 0.7615508 
median(maxent.Kappa) #0.746

# Non-bootstrap Kappa Estimate.
confusion.matrix <- cmx(gam.data, threshold=0.5, na.rm=TRUE)
Kappa(confusion.matrix, st.dev=FALSE); confusion.matrix

# cAUC Statistic
nbit=10000
maxent.cAUC <- list()
pres.obs.cAUC <- dim(pres_test_pwd)[1]     # used 'pres_test_pwd' b/c need only X,Y coordinates, not environmental values at these points.
back.obs.cAUC <- dim(back_test_pwd)[1]
for (i in 1:nbit) {
  sdm.pres.boot.cAUC <- pres_test_pwd[sample(pres.obs.cAUC, replace=TRUE), ]
  sdm.back.boot.cAUC <- back_test_pwd[sample(back.obs.cAUC, replace=TRUE), ]
  #sdm.data.cAUC <- rbind(sdm.pres.boot.cAUC, sdm.back.boot.cAUC)
  maxent.cAUC[[i]] <- evaluate(p=sdm.pres.boot.cAUC, a=sdm.back.boot.cAUC, model=maxent, x=predictors, factors=c('cliffs', 'biome'))
}  
maxent.calAUC <- sapply(maxent.cAUC, function(x){slot(x, 'auc')} )
quantile(maxent.calAUC, c(0.025, 0.975))   # 2.5%: 0.7114556 / 97.5%: 0.8275577 
mean(maxent.calAUC) #0.771
median(maxent.calAUC) #0.772

# Calculate the Standard Error of the Sampling Distribution of the cAUC.
sd(maxent.calAUC) # 

test <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent, x=predictors, factors=c('cliffs','biome')); test
test2 <- evaluate(p=pres_test_pwd, a=back_test_pwd, model=maxent, x=predictors); test2

# Sensitivity (Proportion of pesences correctly predicted)
nbit <-10000
maxent.confusion <- array(data=NA, dim=c(2,2,nbit))
maxent.sensitivity <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- maxent.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- maxent.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  maxent.confusion[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  maxent.sensitivity[i] <- sensitivity(maxent.confusion[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(maxent.sensitivity); sd(maxent.sensitivity) #0.698
quantile(maxent.sensitivity, c(0.025,0.975))
median(maxent.sensitivity) #0.698

# Specificity (Proportion of background points correctly predicted)
nbit <-10000
maxent.confusion <- array(data=NA, dim=c(2,2,nbit))
maxent.specificity <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- maxent.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- maxent.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  maxent.confusion[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  maxent.specificity[i] <- specificity(maxent.confusion[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(maxent.specificity); sd(maxent.specificity) #0.984
quantile(maxent.specificity, c(0.025,0.975))
median(maxent.specificity) #0.984

####Read in the Datasets to calculate cAUC or load ("SDM_cAUC_datasets.rData") ######################################################################################################

#Sample 10,000 random points that are not 'NA' in Vegetation; Presence locations have a chance of being selected
biome<-raster(predictors, layer=20)

set.seed(1) #set seed to assure that models will always have same random sample
background<-randomPoints(biome, n=10000, excludep=FALSE, tryf=10, warn=2, lonlatCorrection=FALSE) 
background<-as.data.frame(background)
names(background) <- c('X','Y'); dim(background) #10,000 points
dups<-duplicated(background); sum(dups); rm(dups) # zero duplicates

bckgvals <- extract(predictors, background)
##########################################################################################################
# Presence Points
rf<-read.csv("../../RosyFinch_Few_Duplicates.csv")
dups<-duplicated(rf); sum(dups)  # Identify 8 duplicates
rf<-rf[!dups, ]       # Remove points with exact same coordinates 

# Remove Duplicate Occurrences in Same Raster Cell
bio1 <- raster(predictors, layer=1)
rf.raster <- rasterize(x=rf, y=bio1, field=1, fun='last', na.rm=TRUE) 
rf.points <- rasterToPoints(rf.raster, fun=function(x){x==1}); 

presvals <- extract(predictors, rf.points[,-3])
dups <- duplicated(presvals); sum(dups) 
presvals <- presvals[!dups, ]; dim(presvals)

pb <- c(rep(1, nrow(presvals)), rep(0, nrow(bckgvals)))
rf.data <- data.frame(cbind(pb, rbind(presvals, bckgvals)))
rf.data[,'cliffs'] <- as.factor(rf.data[,'cliffs'])
rf.data[,'biome'] <- as.factor(rf.data[,'biome'])

test <- is.na(rf.data); summary(test)
rf.data <- na.omit(rf.data); rm(test)  # Now 1,395 Presences

# MaxEnt specific Data
pres.maxent <- rf.points[,-3]
back.maxent <- background

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
set.seed(1)
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

rm(background,bckgvals,presvals,rf,rf.points,bio1,biome,nbit,predictors,rf.raster)
rm(pres_test_pwd,pres_train_pwd,back_test_pwd,back_train_pwd)