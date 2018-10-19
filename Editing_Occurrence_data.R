setwd("~/Documents/MSc.Thesis/Models/")
BCRF <- read.csv("BCRF.csv")
BCRF <- BCRF[, -1]
dups <- duplicated(BCRF)
sum(dups)

BLRF <- read.csv("BLRF.csv")
BLRF <- BLRF[, -1]
dups <- duplicated(BLRF)
sum(dups)
BLRF <- BLRF[!dups, ]

GCRF <- read.csv("GCRF.csv")
GCRF <- GCRF[, -1]
dups <- duplicated(GCRF)
sum(dups)
GCRF <- GCRF[!dups, ]


# Individual Species 
library(raster)
library(rgdal)
BCRF_pres <- extract(predictors, BCRF)
dups <- duplicated(BCRF_pres)
sum(dups)
BCRF_pres <- BCRF_pres[!dups, ]
dim(BCRF_pres)

BLRF_pres <- extract(predictors, BLRF)
dups <- duplicated(BLRF_pres)
sum(dups)
BLRF_pres <- BLRF_pres[!dups, ]
dim(BLRF_pres)
plot(BLRF_pres)

GCRF_pres <- extract(predictors, GCRF)
dups <- duplicated(GCRF_pres)
sum(dups)
GCRF_pres <- GCRF_pres[!dups, ]
dim(GCRF_pres)


gam.e.red2 <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam.red2 <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio7) + s(bio18) + cliffs, family=binomial(link="logit"), data=rf.train)
  gam.e.red2[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam.red2)
}

AIC(rf.gam.red2, k=2) # AIC=2927.611 (vegetation & bio15 are dropped)

gam.e.red3 <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam.red3 <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio18) + cliffs, family=binomial(link="logit"), data=rf.train)
  gam.e.red3[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam.red3)
}

AIC(rf.gam.red3, k=2) # AIC=2954.594 (vegetation, bio7, and bio15 are dropped)

gam.e.red4 <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam.red4 <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio15) + s(bio18) + cliffs, family=binomial(link="logit"), data=rf.train)
  gam.e.red4[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam.red4)
}

AIC(rf.gam.red4, k=2) # AIC=2907.636 (vegetation & bio7 are dropped)

gam.e.red5 <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam.red5 <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio15) + s(bio18) + cliffs + vegetation, family=binomial(link="logit"), data=rf.train)
  gam.e.red5[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam.red5)
}

AIC(rf.gam.red5, k=2) # AIC=2906.121 (bio7 is dropped)

gam.e.red6 <- list() 
for (i in 1:k) {
  pres.train <- pres[group !=i, ]
  back.train <- back[group2 !=i, ]
  pres.test <- pres[group == i, ]
  back.test <- back[group2 == i, ]
  pb <- c(rep(1, nrow(pres.train)), rep(0, nrow(back.train)))
  rf.train <- data.frame(cbind(pb, rbind(pres.train, back.train)))
  rf.gam.red6 <- gam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio7) + s(bio18) + cliffs + vegetation, family=binomial(link="logit"), data=rf.train)
  gam.e.red6[[i]] <- evaluate(p=pres.test, a=back.test, model=rf.gam.red6)
}

AIC(rf.gam.red6, k=2) # AIC=2927.662 (bio15 is dropped)

