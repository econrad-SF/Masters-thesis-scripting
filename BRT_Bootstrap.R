library(PresenceAbsence)
library(dismo)
library(gbm)
library(raster)
########################################################################################
# Prepare Dataset to calculate AUC & Kappa (BRT Model is the 'brt.tc10.lr01' object).

# Plot ID
plotID <- as.numeric(seq(1:11399))
class(plotID); length(plotID)

# Presences - 1,399 (1 NA) + background pts. 10,000 (3 more NAs) = 11,399
Observed <- brt.tc10.lr01$data$y 
class(observed); length(observed); is.atomic(observed)

# Predicted Values at all these points
Predicted <- brt.tc10.lr01$fitted
class(preds); length(observed); is.atomic(observed)

# Create Data Frame for using 'PresenceAbsence' Package
brt.data <- data.frame(cbind(plotID, Observed, Predicted))
head(brt.data, n=20)
head(brt.data[brt.data$Observed==0,], n=20)
auc.roc.plot(DATA=brt.data, find.auc=TRUE, color=TRUE, lwd=1, na.rm=TRUE, add.legend=FALSE, xlab='False Positives (1-Specificity)', ylab='True Positives (Sensitivity)') #opt.methods=4  # 0.36 is the value that Maximizes Kappa (Boosted Regression Trees_


# AUC 
nbit = 10000
pres.obs.auc <- dim(brt.data[brt.data$observed==1, ])[1]
back.obs.auc <- dim(brt.data[brt.data$observed==0, ])[1]
brt.auc <- rep(NA, nbit)
for (i in 1:nbit) {
  sdm.pres.boot <- brt.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- brt.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  brt.auc[i] <- auc(sdm.data, st.dev=FALSE, which.model=1, na.rm=TRUE)  # calculate AUC using the 'PresenceAbsence' package; remove the 4 NA's.
}  
mean(brt.auc);sd(brt.auc)  #0.998
quantile(brt.auc, c(0.025, 0.975))  # 2.5%: 0.9978888 / 97.5%: 0.9987575
median(brt.auc) #0.998


# Kappa
nbit <-10000
brt.confusion <- array(data=NA, dim=c(2,2,nbit))
brt.Kappa <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- brt.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- brt.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  brt.confusion[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Kappa using 'PresenceAbsence' package)
  brt.Kappa[i] <- Kappa(brt.confusion[,,i], st.dev=FALSE)     # Calculate the Kappa Statistic
}
mean(brt.Kappa);sd(brt.Kappa) #0.956
quantile(brt.Kappa, c(0.025, 0.975))  # 2.5%: 0.9498524 / 97.5%: 0.9622685
median(brt.Kappa) #0.956
# Non-bootstrap Kappa Estimate.
confusion.matrix <- cmx(brt.data, threshold=0.5, na.rm=TRUE)
Kappa(confusion.matrix, st.dev=FALSE); confusion.matrix



# cAUC Statistic
nbit=10000
brt.cAUC <- list()
pres.obs.cAUC <- dim(pres_test_cal)[1]
back.obs.cAUC <- dim(back_test_cal)[1]
for (i in 1:nbit) {
  sdm.pres.boot.cAUC <- pres_test_cal[sample(pres.obs.cAUC, replace=TRUE), ]
  sdm.back.boot.cAUC <- back_test_cal[sample(back.obs.cAUC, replace=TRUE), ]
  #sdm.data.cAUC <- rbind(sdm.pres.boot.cAUC, sdm.back.boot.cAUC)
  brt.cAUC[[i]] <- evaluate(p=sdm.pres.boot.cAUC, a=sdm.back.boot.cAUC, model=brt.tc10.lr01, n.trees=brt.tc10.lr01$gbm.call$best.trees)
}  

brt.calAUC <- sapply(brt.cAUC, function(x){slot(x, 'auc')} ) 
quantile(brt.calAUC, c(0.025, 0.975))   # 2.5%: 0.9494121 / 97.5%: 0.9986578
mean(brt.calAUC); sd(brt.calAUC) #0.971
median(brt.calAUC) #0.972


# Calculate Sensitivity (proportion of presences predicted correctly)
nbit <-10000
brt.confusion2 <- array(data=NA, dim=c(2,2,nbit))
brt.sensitivity <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- brt.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- brt.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  brt.confusion2[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  brt.sensitivity[i] <- sensitivity(brt.confusion2[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(brt.sensitivity); sd(brt.sensitivity) #0.943
quantile(brt.sensitivity, c(0.025,0.975))
median(brt.sensitivity) #0.943


# Calculate Sensitivity (proportion of presences predicted correctly)
brt.specificity <- rep(NA, nbit)
for (i in 1:nbit) {
  sdm.pres.boot <- brt.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- brt.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  brt.confusion2[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  brt.specificity[i] <- specificity(brt.confusion2[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(brt.specificity); sd(brt.specificity) #0.997
quantile(brt.specificity, c(0.025,0.975))
median(brt.specificity) #0.997

####Read in the Datasets to calculate cAUC or load ("SDM_cAUC_datasets.rData") ######################################################################################################
# Background Points
#Read in Current Environmental Layers
files.current<-list.files(path='../../Current_Res_828x828m', pattern='tif', full.names=TRUE)
predictors <- stack(files.current); rm(files.current)

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