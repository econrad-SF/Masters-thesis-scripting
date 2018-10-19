####################################################################################################
# BRTs Spatial Predictions (Current, RCP2.6_2070, and RCP8.5_2070 Rosy-Finch distributions)
#library(snow); library(raster); library(rgdal); library(dismo); library(randomForest); library(gbm); library(mgcv); library(rJava)
#load("~/Documents/MSc.Thesis/Models/SDMs.Rdata")

#predictors.brt <- dropLayer(predictors, c(17,19)); names(predictors.brt) 

beginCluster()
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/Current")
Current.brt <- clusterR(predictors.brt, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees, progress='text', filename="Current_brt.tif", format="GTiff", datatype='FLT4S')
                        
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/RCP26_2070")
                        
files.26.2070.BC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/BC', pattern='tif', full.names=TRUE);
files.26.2070.CC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/CC', pattern='tif', full.names=TRUE);
files.26.2070.CN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/CN', pattern='tif', full.names=TRUE);
files.26.2070.GD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GD', pattern='tif', full.names=TRUE);
files.26.2070.GF <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GF', pattern='tif', full.names=TRUE);
files.26.2070.GS <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GS', pattern='tif', full.names=TRUE);
files.26.2070.HD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/HD', pattern='tif', full.names=TRUE);
files.26.2070.HE <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/HE', pattern='tif', full.names=TRUE);
files.26.2070.IP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/IP', pattern='tif', full.names=TRUE);
files.26.2070.MC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MC', pattern='tif', full.names=TRUE);
files.26.2070.MC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MC', pattern='tif', full.names=TRUE);
files.26.2070.MG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MG', pattern='tif', full.names=TRUE);
files.26.2070.MI <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MI', pattern='tif', full.names=TRUE);
files.26.2070.MP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MP', pattern='tif', full.names=TRUE);
files.26.2070.MR <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MR', pattern='tif', full.names=TRUE);
files.26.2070.NO <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/NO', pattern='tif', full.names=TRUE);

pred.26.2070.BC <- stack(files.26.2070.BC); names(pred.26.2070.BC); 
pred.26.2070.BC <- dropLayer(pred.26.2070.BC, c(17,19)); names(pred.26.2070.BC)
pred.26.2070.CC <- stack(files.26.2070.CC); names(pred.26.2070.CC); rm(files.26.2070.CC)
pred.26.2070.CC <- dropLayer(pred.26.2070.CC, c(17,19)); names(pred.26.2070.CC)
pred.26.2070.CN <- stack(files.26.2070.CN); names(pred.26.2070.CN); rm(files.26.2070.CN)
pred.26.2070.CN <- dropLayer(pred.26.2070.CN, c(17,19)); names(pred.26.2070.CN)
pred.26.2070.GD <- stack(files.26.2070.GD); names(pred.26.2070.GD); rm(files.26.2070.GD)
pred.26.2070.GD <- dropLayer(pred.26.2070.GD, c(17,19)); names(pred.26.2070.GD)
pred.26.2070.GF <- stack(files.26.2070.GF); names(pred.26.2070.GF); rm(files.26.2070.GF)
pred.26.2070.GF <- dropLayer(pred.26.2070.GF, c(17,19)); names(pred.26.2070.GF)
pred.26.2070.GS <- stack(files.26.2070.GS); names(pred.26.2070.GS); rm(files.26.2070.GS)
pred.26.2070.GS <- dropLayer(pred.26.2070.GS, c(17,19)); names(pred.26.2070.GS)
pred.26.2070.HD <- stack(files.26.2070.HD); names(pred.26.2070.HD); rm(files.26.2070.HD)
pred.26.2070.HD <- dropLayer(pred.26.2070.HD, c(17,19)); names(pred.26.2070.HD)
pred.26.2070.HE <- stack(files.26.2070.HE); names(pred.26.2070.HE); rm(files.26.2070.HE)
pred.26.2070.HE <- dropLayer(pred.26.2070.HE, c(17,19)); names(pred.26.2070.HE)
pred.26.2070.IP <- stack(files.26.2070.IP); names(pred.26.2070.IP); rm(files.26.2070.IP)
pred.26.2070.IP <- dropLayer(pred.26.2070.IP, c(17,19)); names(pred.26.2070.IP)
pred.26.2070.MC <- stack(files.26.2070.MC); names(pred.26.2070.MC); rm(files.26.2070.MC)
pred.26.2070.MC <- dropLayer(pred.26.2070.MC, c(17,19)); names(pred.26.2070.MC)
pred.26.2070.MG <- stack(files.26.2070.MG); names(pred.26.2070.MG); rm(files.26.2070.MG)
pred.26.2070.MG <- dropLayer(pred.26.2070.MG, c(17,19)); names(pred.26.2070.MG)
pred.26.2070.MI <- stack(files.26.2070.MI); names(pred.26.2070.MI); rm(files.26.2070.MI)
pred.26.2070.MI <- dropLayer(pred.26.2070.MI, c(17,19)); names(pred.26.2070.MI)
pred.26.2070.MP <- stack(files.26.2070.MP); names(pred.26.2070.MP); rm(files.26.2070.MP)
pred.26.2070.MP <- dropLayer(pred.26.2070.MP, c(17,19)); names(pred.26.2070.MP)
pred.26.2070.MR <- stack(files.26.2070.MR); names(pred.26.2070.MR); rm(files.26.2070.MR)
pred.26.2070.MR <- dropLayer(pred.26.2070.MR, c(17,19)); names(pred.26.2070.MR)
pred.26.2070.NO <- stack(files.26.2070.NO); names(pred.26.2070.NO); rm(files.26.2070.NO)
pred.26.2070.NO <- dropLayer(pred.26.2070.NO, c(17,19)); names(pred.26.2070.NO)

beginCluster(); RCP26_2070.brt.BC <- clusterR(pred.26.2070.BC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees, progress='text', filename="RCP26_2070_brt_BC.tif", format="GTiff", datatype='FLT4S'); endCluster()
beginCluster(); RCP26_2070.brt.CC <- clusterR(pred.26.2070.CC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_CC.tif", format="GTiff", datatype='FLT4S'); endCluster()              
beginCluster(); RCP26_2070.brt.CN <- clusterR(pred.26.2070.CN, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_CN.tif", format="GTiff", datatype='FLT4S'); endCluster()
beginCluster(); RCP26_2070.brt.GD <- clusterR(pred.26.2070.GD, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_GD.tif", format="GTiff", datatype='FLT4S'); endCluster()
beginCluster(); RCP26_2070.brt.GF <- clusterR(pred.26.2070.GF, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_GF.tif", format="GTiff", datatype='FLT4S'); endCluster()                         
beginCluster(); RCP26_2070.brt.GS <- clusterR(pred.26.2070.GS, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_GS.tif", format="GTiff", datatype='FLT4S'); endCluster()                          
beginCluster(); RCP26_2070.brt.HD <- clusterR(pred.26.2070.HD, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_HD.tif", format="GTiff", datatype='FLT4S'); endCluster()
beginCluster(); RCP26_2070.brt.HE <- clusterR(pred.26.2070.HE, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_HE.tif", format="GTiff", datatype='FLT4S', overwrite=TRUE)
beginCluster(); RCP26_2070.brt.IP <- clusterR(pred.26.2070.IP, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_IP.tif", format="GTiff", datatype='FLT4S'); endCluster();                        
beginCluster(); RCP26_2070.brt.MC <- clusterR(pred.26.2070.MC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_MC.tif", format="GTiff", datatype='FLT4S'); endCluster();                           
beginCluster(); RCP26_2070.brt.MG <- clusterR(pred.26.2070.MG, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_MG.tif", format="GTiff", datatype='FLT4S'); endCluster();                         
beginCluster(); RCP26_2070.brt.MI <- clusterR(pred.26.2070.MI, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_MI.tif", format="GTiff", datatype='FLT4S'); endCluster();                             
beginCluster(); RCP26_2070.brt.MP <- clusterR(pred.26.2070.MP, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_MP.tif", format="GTiff", datatype='FLT4S'); endCluster();                            
beginCluster(); RCP26_2070.brt.MR <- clusterR(pred.26.2070.MR, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_MR.tif", format="GTiff", datatype='FLT4S'); endCluster();                          
beginCluster(); RCP26_2070.brt.NO <- clusterR(pred.26.2070.NO, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP26_2070_brt_NO.tif", format="GTiff", datatype='FLT4S'); endCluster();
                              
                              

### BRT RCP85_2070 Individual GCM Predicitons
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/RCP85_2070")
files.85.2070.AC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/AC', pattern='tif', full.names=TRUE);
pred.85.2070.AC <- stack(files.85.2070.AC); names(pred.85.2070.AC);
pred.85.2070.AC <- dropLayer(pred.85.2070.AC, c(17,19)); names(pred.85.2070.AC); rm(files.85.2070.AC)
RCP85_2070.brt.AC <- predict(pred.85.2070.AC, model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees, progress='text', filename="RCP85_2070_brt_AC.tif", format="GTiff", datatype='FLT4S')                                  

files.85.2070.BC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/BC', pattern='tif', full.names=TRUE);
pred.85.2070.BC <- stack(files.85.2070.BC); names(pred.85.2070.BC);
pred.85.2070.BC <- dropLayer(pred.85.2070.BC, c(17,19)); names(pred.85.2070.BC); rm(files.85.2070.BC)
beginCluster()
RCP85_2070.brt.BC <- clusterR(pred.85.2070.BC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_BC.tif", format="GTiff", datatype='FLT4S')                                  
endCluster()

files.85.2070.CC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/CC', pattern='tif', full.names=TRUE)
pred.85.2070.CC <- stack(files.85.2070.CC); names(pred.85.2070.CC); rm(files.85.2070.CC)
pred.85.2070.CC <- dropLayer(pred.85.2070.CC, c(17,19)); names(pred.85.2070.CC)
beginCluster()
RCP85_2070.brt.CC <- clusterR(pred.85.2070.CC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_CC.tif", format="GTiff", datatype='FLT4S')           
endCluster()

files.85.2070.CN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/CN', pattern='tif', full.names=TRUE);
pred.85.2070.CN <- stack(files.85.2070.CN); names(pred.85.2070.CN); rm(files.85.2070.CN)
pred.85.2070.CN <- dropLayer(pred.85.2070.CN, c(17,19)); names(pred.85.2070.CN)
beginCluster()
RCP85_2070.brt.CN <- clusterR(pred.85.2070.CN, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_CN.tif", format="GTiff", datatype='FLT4S')           
endCluster()

files.85.2070.GF <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/GF', pattern='tif', full.names=TRUE);
pred.85.2070.GF <- stack(files.85.2070.GF); names(pred.85.2070.GF); rm(files.85.2070.GF)
pred.85.2070.GF <- dropLayer(pred.85.2070.GF, c(17,19)); names(pred.85.2070.GF)
beginCluster()
RCP85_2070.brt.GF <- clusterR(pred.85.2070.GF, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_GF.tif", format="GTiff", datatype='FLT4S')           
endCluster()

files.85.2070.GS <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/GS', pattern='tif', full.names=TRUE);
pred.85.2070.GS <- stack(files.85.2070.GS); names(pred.85.2070.GS); rm(files.85.2070.GS)
pred.85.2070.GS <- dropLayer(pred.85.2070.GS, c(17,19)); names(pred.85.2070.GS)
RCP85_2070.brt.GS <- clusterR(pred.85.2070.GS, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_GS.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HD', pattern='tif', full.names=TRUE);
pred.85.2070.HD <- stack(files.85.2070.HD); names(pred.85.2070.HD); rm(files.85.2070.HD)
pred.85.2070.HD <- dropLayer(pred.85.2070.HD, c(17,19)); names(pred.85.2070.HD)
RCP85_2070.brt.HD <- clusterR(pred.85.2070.HD, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_HD.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HE <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HE', pattern='tif', full.names=TRUE);
pred.85.2070.HE <- stack(files.85.2070.HE); names(pred.85.2070.HD); rm(files.85.2070.HE)
pred.85.2070.HE <- dropLayer(pred.85.2070.HE, c(17,19)); names(pred.85.2070.HE)
RCP85_2070.brt.HE <- clusterR(pred.85.2070.HE, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_HE.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HG', pattern='tif', full.names=TRUE);
pred.85.2070.HG <- stack(files.85.2070.HG); names(pred.85.2070.HG); rm(files.85.2070.HG)
pred.85.2070.HG <- dropLayer(pred.85.2070.HG, c(17,19)); names(pred.85.2070.HG)
RCP85_2070.brt.HG <- clusterR(pred.85.2070.HG, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_HG.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.IN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IN', pattern='tif', full.names=TRUE);
pred.85.2070.IN <- stack(files.85.2070.IN); names(pred.85.2070.IN); rm(files.85.2070.IN)
pred.85.2070.IN <- dropLayer(pred.85.2070.IN, c(17,19)); names(pred.85.2070.IN)
RCP85_2070.brt.IN <- clusterR(pred.85.2070.IN, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_IN.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.IP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IP', pattern='tif', full.names=TRUE);
pred.85.2070.IP <- stack(files.85.2070.IP); names(pred.85.2070.IP); rm(files.85.2070.IP)
pred.85.2070.IP <- dropLayer(pred.85.2070.IP, c(17,19)); names(pred.85.2070.IP)
RCP85_2070.brt.IP <- clusterR(pred.85.2070.IP, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_IP.tif", format="GTiff", datatype='FLT4S')           

# Redo GCM "IP" to check for Error
files.85.2070.IP2 <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IP2', pattern='tif', full.names=TRUE);
pred.85.2070.IP2 <- stack(files.85.2070.IP2); #names(pred.85.2070.IP2); rm(files.85.2070.IP2)
pred.85.2070.IP2 <- dropLayer(pred.85.2070.IP2, c(17,19)); names(pred.85.2070.IP2)
RCP85_2070.brt.tc5.IP2 <- predict(pred.85.2070.IP2, model=brt.tc5.lr05.bg.5, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc5.lr05.bg.5$gbm.call$best.trees, progress='text', filename="RCP85_2070_brt_tc5_IP2.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.IP2 <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IP2', pattern='tif', full.names=TRUE);
pred.85.2070.IP2 <- stack(files.85.2070.IP2); #names(pred.85.2070.IP2); rm(files.85.2070.IP2)
RCP85_2070.brt.tc5.ALL.IP2 <- predict(pred.85.2070.IP2, model=brt.tc5.lr05.bg.5.ALL, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc5.lr05.bg.5.ALL$gbm.call$best.trees, progress='text', filename="RCP85_2070_brt_tc5_ALL_IP2.tif", format="GTiff", datatype='FLT4S')           


brt.tc5.lr05.bg.5.ALL




files.85.2070.MC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MC', pattern='tif', full.names=TRUE);
pred.85.2070.MC <- stack(files.85.2070.MC); names(pred.85.2070.MC); rm(files.85.2070.MC)
pred.85.2070.MC <- dropLayer(pred.85.2070.MC, c(17,19)); names(pred.85.2070.MC)
RCP85_2070.brt.MC <- clusterR(pred.85.2070.MC, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_MC.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MG', pattern='tif', full.names=TRUE);
pred.85.2070.MG <- stack(files.85.2070.MG); names(pred.85.2070.MG); rm(files.85.2070.MG)
pred.85.2070.MG <- dropLayer(pred.85.2070.MG, c(17,19)); names(pred.85.2070.MG)
RCP85_2070.brt.MG <- clusterR(pred.85.2070.MG, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_MG.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MI <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MI', pattern='tif', full.names=TRUE);
pred.85.2070.MI <- stack(files.85.2070.MI); names(pred.85.2070.MI); rm(files.85.2070.MI)
pred.85.2070.MI <- dropLayer(pred.85.2070.MI, c(17,19)); names(pred.85.2070.MI)
RCP85_2070.brt.MI <- clusterR(pred.85.2070.MI, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_MI.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MP', pattern='tif', full.names=TRUE);
pred.85.2070.MP <- stack(files.85.2070.MP); names(pred.85.2070.MP); rm(files.85.2070.MP)
pred.85.2070.MP <- dropLayer(pred.85.2070.MP, c(17,19)); names(pred.85.2070.MP)
RCP85_2070.brt.MP <- clusterR(pred.85.2070.MP, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_MP.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MR <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MR', pattern='tif', full.names=TRUE);
pred.85.2070.MR <- stack(files.85.2070.MR); names(pred.85.2070.MR); rm(files.85.2070.MR)
pred.85.2070.MR <- dropLayer(pred.85.2070.MR, c(17,19)); names(pred.85.2070.MR)
RCP85_2070.brt.MR <- clusterR(pred.85.2070.MR, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_MR.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.NO <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/NO', pattern='tif', full.names=TRUE);
pred.85.2070.NO <- stack(files.85.2070.NO); names(pred.85.2070.NO); rm(files.85.2070.NO)
pred.85.2070.NO <- dropLayer(pred.85.2070.NO, c(17,19)); names(pred.85.2070.NO)
RCP85_2070.brt.NO <- clusterR(pred.85.2070.NO, fun=predict, args=list(model=brt.tc10.lr01, factors=c('cliffs','vegetation'), type="response", n.trees=brt.tc10.lr01$gbm.call$best.trees), progress='text', filename="RCP85_2070_brt_NO.tif", format="GTiff", datatype='FLT4S')           
####################################################################################################
# Random Forests Spatial Predictions (Current, RCP2.6_2070, and RCP8.5_2070 Rosy-Finch distributions)
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/Current")
beginCluster()
#Current.randomForest <- clusterR(predictors, fun=predict, args=list(model=randomForest, factors=c('cliffs', 'vegetation'), type="response"), progress='text', filename="Current_randomForest.tif", format="GTiff", datatype='FLT4S')
endCluster()

Current.randomForest.reg <- clusterR(predictors, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs', 'vegetation'), type="response"), progress='text', filename="Current_randomForest_regression.tif", format="GTiff", datatype='FLT4S')
writeRaster(Current.randomForest.reg, filename="Current_rForest_reg.tif", format=GTiff, datatype="FLT4S")

# Random Forest RCP2.6_2070 GCM Predictions                       
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/RCP26_2070")
files.26.2070.BC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/BC', pattern='tif', full.names=TRUE);
pred.26.2070.BC <- stack(files.26.2070.BC); names(pred.26.2070.BC);
RCP26_2070.rForest.BC <- clusterR(pred.26.2070.BC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_BC.tif", format="GTiff", datatype='FLT4S')                                  

files.26.2070.CC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/CC', pattern='tif', full.names=TRUE)
pred.26.2070.CC <- stack(files.26.2070.CC); names(pred.26.2070.CC); rm(files.26.2070.CC)
RCP26_2070.rForest.CC <- clusterR(pred.26.2070.CC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_CC.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.CN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/CN', pattern='tif', full.names=TRUE);
pred.26.2070.CN <- stack(files.26.2070.CN); names(pred.26.2070.CN); rm(files.26.2070.CN)
RCP26_2070.rForest.CN <- clusterR(pred.26.2070.CN, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_CN.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.GD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GD', pattern='tif', full.names=TRUE);
pred.26.2070.GD <- stack(files.26.2070.GD); names(pred.26.2070.GD); rm(files.26.2070.GD)
RCP26_2070.rForest.GD <- clusterR(pred.26.2070.GD, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_GD.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.GF <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GF', pattern='tif', full.names=TRUE);
pred.26.2070.GF <- stack(files.26.2070.GF); names(pred.26.2070.GF); rm(files.26.2070.GF)
RCP26_2070.rForest.GF <- clusterR(pred.26.2070.GF, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_GF.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.GS <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/GS', pattern='tif', full.names=TRUE);
pred.26.2070.GS <- stack(files.26.2070.GS); names(pred.26.2070.GS); rm(files.26.2070.GS)
RCP26_2070.rForest.GS <- clusterR(pred.26.2070.GS, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_GS.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.HD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/HD', pattern='tif', full.names=TRUE);
pred.26.2070.HD <- stack(files.26.2070.HD); names(pred.26.2070.HD); rm(files.26.2070.HD)
RCP26_2070.rForest.HD <- clusterR(pred.26.2070.HD, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_HD.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.HE <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/HE', pattern='tif', full.names=TRUE);
pred.26.2070.HE <- stack(files.26.2070.HE); names(pred.26.2070.HD); rm(files.26.2070.HE)
RCP26_2070.rForest.HE <- clusterR(pred.26.2070.HE, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_HE.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.IP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/IP', pattern='tif', full.names=TRUE);
pred.26.2070.IP <- stack(files.26.2070.IP); names(pred.26.2070.IP); rm(files.26.2070.IP)
RCP85_2070.rForest.IP <- clusterR(pred.26.2070.IP, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_IP.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.MC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MC', pattern='tif', full.names=TRUE);
pred.26.2070.MC <- stack(files.26.2070.MC); names(pred.26.2070.MC); rm(files.26.2070.MC)
RCP26_2070.rForest.MC <- clusterR(pred.26.2070.MC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_MC.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.MG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MG', pattern='tif', full.names=TRUE);
pred.26.2070.MG <- stack(files.26.2070.MG); names(pred.26.2070.MG); rm(files.26.2070.MG)
RCP26_2070.rForest.MG <- clusterR(pred.26.2070.MG, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_MG.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.MI <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MI', pattern='tif', full.names=TRUE);
pred.26.2070.MI <- stack(files.26.2070.MI); names(pred.26.2070.MI); rm(files.26.2070.MI)
RCP26_2070.rForest.MI <- clusterR(pred.26.2070.MI, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_MI.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.MP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MP', pattern='tif', full.names=TRUE);
pred.26.2070.MP <- stack(files.26.2070.MP); names(pred.26.2070.MP); rm(files.26.2070.MP)
RCP26_2070.rForest.MP <- clusterR(pred.26.2070.MP, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_MP.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.MR <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/MR', pattern='tif', full.names=TRUE);
pred.26.2070.MR <- stack(files.26.2070.MR); names(pred.26.2070.MR); rm(files.26.2070.MR)
RCP26_2070.rForest.MR <- clusterR(pred.26.2070.MR, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_MR.tif", format="GTiff", datatype='FLT4S')           

files.26.2070.NO <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP26_2070_Res828x828m/GCMs/NO', pattern='tif', full.names=TRUE);
pred.26.2070.NO <- stack(files.26.2070.NO); names(pred.26.2070.NO); rm(files.26.2070.NO)
RCP26_2070.rForest.NO <- clusterR(pred.26.2070.NO, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP26_2070_rForest_NO.tif", format="GTiff", datatype='FLT4S')           

###### Random Forest RCP8.5_2070 GCM Predictions
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/RCP85_2070")
files.85.2070.AC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/AC', pattern='tif', full.names=TRUE);
pred.85.2070.AC <- stack(files.85.2070.AC); names(pred.85.2070.AC);
RCP85_2070.rForest.AC <- clusterR(pred.85.2070.AC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_AC.tif", format="GTiff", datatype='FLT4S')                                  

files.85.2070.BC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/BC', pattern='tif', full.names=TRUE);
pred.85.2070.BC <- stack(files.85.2070.BC); names(pred.85.2070.BC);
RCP85_2070.rForest.BC <- clusterR(pred.85.2070.BC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_BC.tif", format="GTiff", datatype='FLT4S')                                  

files.85.2070.CC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/CC', pattern='tif', full.names=TRUE)
pred.85.2070.CC <- stack(files.85.2070.CC); names(pred.85.2070.CC); rm(files.85.2070.CC)
RCP85_2070.rForest.CC <- clusterR(pred.85.2070.CC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_CC.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.CN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/CN', pattern='tif', full.names=TRUE);
pred.85.2070.CN <- stack(files.85.2070.CN); names(pred.85.2070.CN); rm(files.85.2070.CN)
RCP85_2070.rForest.CN <- clusterR(pred.85.2070.CN, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_CN.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.GF <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/GF', pattern='tif', full.names=TRUE);
pred.85.2070.GF <- stack(files.85.2070.GF); names(pred.85.2070.GF); rm(files.85.2070.GF)
RCP85_2070.rForest.GF <- clusterR(pred.85.2070.GF, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_GF.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.GS <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/GS', pattern='tif', full.names=TRUE);
pred.85.2070.GS <- stack(files.85.2070.GS); names(pred.85.2070.GS); rm(files.85.2070.GS)
RCP85_2070.rForest.GS <- clusterR(pred.85.2070.GS, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_GS.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HD <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HD', pattern='tif', full.names=TRUE);
pred.85.2070.HD <- stack(files.85.2070.HD); names(pred.85.2070.HD); rm(files.85.2070.HD)
RCP85_2070.rForest.HD <- clusterR(pred.85.2070.HD, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_HD.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HE <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HE', pattern='tif', full.names=TRUE);
pred.85.2070.HE <- stack(files.85.2070.HE); names(pred.85.2070.HD); rm(files.85.2070.HE)
RCP85_2070.rForest.HE <- clusterR(pred.85.2070.HE, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_HE.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.HG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/HG', pattern='tif', full.names=TRUE);
pred.85.2070.HG <- stack(files.85.2070.HG); names(pred.85.2070.HG); rm(files.85.2070.HG)
RCP85_2070.rForest.HG <- clusterR(pred.85.2070.HG, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_HG.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.IN <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IN', pattern='tif', full.names=TRUE);
pred.85.2070.IN <- stack(files.85.2070.IN); names(pred.85.2070.IN); rm(files.85.2070.IN)
RCP85_2070.rForest.IN <- clusterR(pred.85.2070.IN, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_IN.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.IP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/IP', pattern='tif', full.names=TRUE);
pred.85.2070.IP <- stack(files.85.2070.IP); names(pred.85.2070.IP); rm(files.85.2070.IP)
RCP85_2070.rForest.IP <- clusterR(pred.85.2070.IP, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_IP.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MC <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MC', pattern='tif', full.names=TRUE);
pred.85.2070.MC <- stack(files.85.2070.MC); names(pred.85.2070.MC); rm(files.85.2070.MC)
RCP85_2070.rForest.MC <- clusterR(pred.85.2070.MC, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_MC.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MG <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MG', pattern='tif', full.names=TRUE);
pred.85.2070.MG <- stack(files.85.2070.MG); names(pred.85.2070.MG); rm(files.85.2070.MG)
RCP85_2070.rForest.MG <- clusterR(pred.85.2070.MG, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_MG.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MI <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MI', pattern='tif', full.names=TRUE);
pred.85.2070.MI <- stack(files.85.2070.MI); names(pred.85.2070.MI); rm(files.85.2070.MI)
RCP85_2070.rForest.MI <- clusterR(pred.85.2070.MI, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_MI.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MP <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MP', pattern='tif', full.names=TRUE);
pred.85.2070.MP <- stack(files.85.2070.MP); names(pred.85.2070.MP); rm(files.85.2070.MP)
RCP85_2070.rForest.MP <- clusterR(pred.85.2070.MP, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_MP.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.MR <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/MR', pattern='tif', full.names=TRUE);
pred.85.2070.MR <- stack(files.85.2070.MR); names(pred.85.2070.MR); rm(files.85.2070.MR)
RCP85_2070.rForest.MR <- clusterR(pred.85.2070.MR, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_MR.tif", format="GTiff", datatype='FLT4S')           

files.85.2070.NO <- list.files(path='~/Documents/MSc.Thesis/Models/Future/RCP85_2070_Res828x828m/GCMs/NO', pattern='tif', full.names=TRUE);
pred.85.2070.NO <- stack(files.85.2070.NO); names(pred.85.2070.NO); rm(files.85.2070.NO)
RCP85_2070.rForest.NO <- clusterR(pred.85.2070.NO, fun=predict, args=list(model=randomForest.reg, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="RCP85_2070_rForest_NO.tif", format="GTiff", datatype='FLT4S')           
####################################################################################################
# GAM Spatial Predictions (Current, RCP2.6_2070, and RCP8.5_2070 Rosy-Finch distributions)
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/Current")
beginCluster()
Current.gam <- clusterR(predictors, fun=predict, args=list(model=gam.all, factors=c('cliffs','vegetation'), type="response"), progress='text', filename="Current_gam.tif", format="GTiff", datatype='FLT4S')
endCluster()

#RCP26_2070.gam <- clusterR(pred.26.2070, fun=predict, args=list(model=gam.all, factors=c('cliffs'), type="response"), progress='text', filename="RCP26_2070_gam.tif", format="GTiff", datatype='FLT4S')
#RCP85_2070.gam <- clusterR(pred.85.2070, fun=predict, args=list(model=gam.all, factors=c('cliffs'), type="response"), progress='text', filename="RCP85_2070_gam_cal.tif", format="GTiff", datatype='FLT4S')
####################################################################################################





# Maxent Spatial Predictions (Current, RCP2.6_2070, and RCP8.5_2070 Rosy-Finch distributions)
setwd("~/Documents/MSc.Thesis/Models/Maps_Resolution_828x828m/Current")
beginCluster()
Current.maxent <- clusterR(predictors, fun=predict, args=list(model=maxent, outputgrids=FALSE), filename="Current_maxent.tif", format="GTiff", datatype='FLT4S', progress='text')
endCluster()


RCP26_2070.maxent <- clusterR(pred.26.2070, fun=predict, args=list(model=maxent, writemess=TRUE, writeclampgrid=TRUE, outputgrids=FALSE), filename="RCP26_2070_maxent_cal.tif", format="GTiff", datatype='FLT4S', progress='text')
RCP85_2070.maxent <- clusterR(pred.85.2070, fun=predict, args=list(model=maxent, writemess=TRUE, writeclampgrid=TRUE, outputgrids=FALSE), filename="RCP85_2070_maxent_cal.tif", format="GTiff", datatype='FLT4S', progress='text')
#endCluster()


                              