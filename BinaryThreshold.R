
install.packages("PresenceAbsence")
library(PresenceAbsence)


# BRT Threshold: Maximum Kappa
# Plot ID
plotID <- as.numeric(seq(1:11399))
class(plotID); length(plotID)

# Presences - 1,395 + background pts. 10,000 = 11,395
observed <- brt.tc10.lr01$data$y 
class(observed); length(observed); is.atomic(observed)

# Predicted Values at all these points
preds <- brt.tc10.lr01$fitted
class(preds); length(observed); is.atomic(observed)

# Create Data Frame for using PresenceAbsence Package
threshold.data <- data.frame(cbind(plotID, observed, preds))

# Threshold Dependent Measures
auc.roc.plot(DATA=threshold.data, opt.methods=4, find.auc=FALSE)  # 0.36 is the value that Maximizes Kappa (Boosted Regression Tree)
auc.roc.plot(DATA=threshold.data, opt.methods=3, find.auc=FALSE)  # 0.29 therefore, more absences will be included in prediction (overprediction)

# Calculate Kappa Statistic
confusion.matrix <- cmx(threshold.data, threshold=0.5)
Kappa(confusion.matrix, st.dev=TRUE)
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# Random Forest Threshold: Maximum Kappa

# Plot ID
plotID <- as.numeric(seq(1:10255))
class(plotID); length(plotID)

# Presences - 1,258 + background pts. 8,948 = 10,206
observed <- randomForest.reg$y
class(observed); length(observed); is.atomic(observed)

# Predicted Values at all these points
preds <- randomForest.reg$predicted
class(preds); length(preds); is.atomic(preds)


# Create Data Frame for using PresenceAbsence Package
threshold.data <- data.frame(cbind(plotID, observed, preds))

# Threshold Dependent Measures
auc.roc.plot(DATA=threshold.data, opt.methods=4, find.auc=TRUE)  # 0.34 is the value that Maximizes Kappa (RandomForest)
auc.roc.plot(DATA=threshold.data, opt.methods=3, find.auc=FALSE)  # 0.19 therefore, more absences will be included in prediction (overprediction)

confusion.matrix <- cmx(threshold.data, threshold=0.5)
Kappa(confusion.matrix, st.dev=TRUE); confusion.matrix
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# GAM Threshold: Maximum Kappa

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
threshold.data <- data.frame(cbind(plotID, observed, preds))

# Threshold Dependent Measures
auc.roc.plot(DATA=threshold.data, opt.methods=4, find.auc=TRUE)  # 0.42 is the value that Maximizes Kappa (GAM)
auc.roc.plot(DATA=threshold.data, opt.methods=3, find.auc=FALSE)  # 0.18 therefore, more absences will be included in prediction (overprediction)


confusion.matrix <- cmx(threshold.data, threshold=0.5)
Kappa(confusion.matrix, st.dev=TRUE); confusion.matrix
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# MaxEnt Threshold: Maximum Kappa
setwd("~/Documents/MSc.Thesis/Models/MaxentOutput_R")
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
threshold.data <- data.frame(cbind(plotID, observed, preds))

# Threshold Dependent Measures
auc.roc.plot(DATA=threshold.data, opt.methods=4, find.auc=TRUE)  # 0.44 is the value that Maximizes Kappa (MaxEnt)
auc.roc.plot(DATA=threshold.data, opt.methods=3, find.auc=FALSE)  # 0.24


therefore, more absences will be included in prediction (overprediction)

confusion.matrix <- cmx(threshold.data, threshold=0.5)
Kappa(confusion.matrix, st.dev=TRUE)