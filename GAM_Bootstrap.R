library(PresenceAbsence)
library(dismo)
library(mgcv)
library(raster)
########################################################################################
#Training/testing datasets
test <- is.na(rf.data); summary(test)
rf.data <- na.omit(rf.data); rm(test)  # Now 1,395 Presences

pres <- rf.data[rf.data[,1] ==1, 2:22]
back <- rf.data[rf.data[,1] ==0, 2:22]
k <- 10
group <- kfold(pres, k)
group2 <- kfold(back, k)

#gam.e.all <- list() #create empty list object to put in the evaluation statistics for each fold (i.e. 'i').
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  gam.all <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio6) + s(bio7) + s(bio8) + s(bio9) + s(bio10) + s(bio11) + s(bio12) + s(bio13) + s(bio14) + s(bio15) + s(bio16) + s(bio17) + s(bio18) + s(bio19) + cliffs + biome, family=binomial(link="logit"), data=rf.train)
  #gam.e.all[[i]] <- evaluate(p=pres.test, a=back.test, model=gam.all)
}

########################################################################################
# Prepare Dataset to calculate AUC & Kappa (BRT Model is the 'brt.tc10.lr01' object).

# Plot ID
plotID <- as.numeric(seq(1:10255))
class(plotID); length(plotID)

# Presences - 1,258 + background pts. 8,948 = 10,206
observed <- gam.all$y
class(observed); length(observed); is.atomic(observed)

# Predicted Values at all these points
preds <- gam.all$fitted.values
class(preds); length(preds); is.atomic(preds)


# Create Data Frame for using PresenceAbsence Package
gam.data <- data.frame(cbind(plotID, observed, preds))

# AUC 
nbit = 10000
pres.obs.auc <- dim(gam.data[gam.data$observed==1, ])[1]
back.obs.auc <- dim(gam.data[gam.data$observed==0, ])[1]
gam.auc <- rep(NA, nbit)
for (i in 1:nbit) {
  sdm.pres.boot <- gam.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- gam.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  gam.auc[i] <- auc(sdm.data, st.dev=FALSE, which.model=1, na.rm=TRUE)  # calculate AUC using the 'PresenceAbsence' package; remove the 4 NA's.
}  
mean(gam.auc); sd(gam.auc) #0.980
quantile(gam.auc, c(0.025, 0.975))  # 2.5%: 0.9774762 / 97.5%: 0.9819153 
median(gam.auc) #0.980


# Kappa
nbit <-10000
gam.confusion <- array(data=NA, dim=c(2,2,nbit))
gam.Kappa <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- gam.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- gam.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  gam.confusion[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Kappa using 'PresenceAbsence' package)
  gam.Kappa[i] <- Kappa(gam.confusion[,,i], st.dev=FALSE)     # Calculate the Kappa Statistic
}
mean(gam.Kappa); sd(gam.Kappa) #0.787
quantile(gam.Kappa, c(0.025, 0.975))  # 2.5%: 0.7723696 / 97.5%: 0.8011281 
median(gam.Kappa) #0.787

# Non-bootstrap Kappa Estimate.
confusion.matrix <- cmx(gam.data, threshold=0.5, na.rm=TRUE)
Kappa(confusion.matrix, st.dev=FALSE); confusion.matrix

# cAUC Statistic
nbit=10000
gam.cAUC <- list()
pres.obs.cAUC <- dim(pres_test_cal)[1]
back.obs.cAUC <- dim(back_test_cal)[1]
for (i in 1:nbit) {
  sdm.pres.boot.cAUC <- pres_test_cal[sample(pres.obs.cAUC, replace=TRUE), ]
  sdm.back.boot.cAUC <- back_test_cal[sample(back.obs.cAUC, replace=TRUE), ]
  sdm.data.cAUC <- rbind(sdm.pres.boot.cAUC, sdm.back.boot.cAUC)
  gam.cAUC[[i]] <- evaluate(p=sdm.pres.boot.cAUC, a=sdm.back.boot.cAUC, model=gam.all)
}  

gam.calAUC <- sapply(gam.cAUC, function(x){slot(x, 'auc')} )
quantile(gam.calAUC, c(0.025, 0.975))   # 2.5%: 0.7131172 / 97.5%: 0.8299433 
mean(gam.calAUC);sd(gam.calAUC) #0.774
median(gam.calAUC) #0.776

# Sensitity (proportion of presences correctly predicted)
nbit <-10000
gam.confusion2 <- array(data=NA, dim=c(2,2,nbit))
gam.sensitivity <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- gam.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- gam.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  gam.confusion2[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  gam.sensitivity[i] <- sensitivity(gam.confusion2[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(gam.sensitivity); sd(gam.sensitivity)
quantile(gam.sensitivity, c(0.025, 0.975))
median(gam.sensitivity) #0.757

# Specificity (proportion of background correctly predicted)
nbit <-10000
gam.confusion2 <- array(data=NA, dim=c(2,2,nbit))
gam.specificity <- rep(NA, nbit)

for (i in 1:nbit) {
  sdm.pres.boot <- gam.data[sample(pres.obs.auc, replace=TRUE), ]
  sdm.back.boot <- gam.data[sample(back.obs.auc, replace=TRUE), ]
  sdm.data <- rbind(sdm.pres.boot, sdm.back.boot)
  gam.confusion2[,,i] <- cmx(sdm.data, threshold=0.5, na.rm=TRUE)   # Create a Confusion Matrix (necessary to calculate Sensitivity using 'PresenceAbsence' package)
  gam.specificity[i] <- specificity(gam.confusion2[,,i], st.dev=FALSE)     # Calculate Sensitivity
}
mean(gam.specificity); sd(gam.specificity) #0.981
quantile(gam.specificity, c(0.025, 0.975))
median(gam.specificity) #0.981

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