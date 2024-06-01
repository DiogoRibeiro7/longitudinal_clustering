# latrend: A Framework for Clustering Longitudinal Data

library("latrend") 
library("kml")
library("dtwclust")
library("lcmm")
data("PAP.adh") 
head(PAP.adh)

options(latrend.id = "Patient", latrend.time = "Week")

plotTrajectories(PAP.adh, response = "UsageHours", cluster = "Group")

kmlMethod <- lcMethodKML(response = "UsageHours", nClusters = 2) 

kmlMethod


dtwMethod <- lcMethodDtwclust(response = "UsageHours", distance = "dtw_basic") 
lmkmMethod <- lcMethodLMKM(formula = UsageHours ~ Week)
gbtmMethod <- lcMethodLcmmGBTM(fixed = UsageHours ~ Week,
                               mixture = ~ Week, idiag = TRUE)
gmmMethod <- lcMethodLcmmGMM(fixed = UsageHours ~ Week,
                             mixture = ~ Week, random = ~ 1, idiag = TRUE)

kml3Method <- update(kmlMethod, nClusters = 3)


kmlMethods <- lcMethods(kmlMethod, nClusters = 1:6) 
lmkmMethods <- lcMethods(lmkmMethod, nClusters = 1:6) 
dtwMethods <- lcMethods(dtwMethod, nClusters = 2:6) 
gbtmMethods <- lcMethods(gbtmMethod, nClusters = 1:4) 
gmmMethods <- lcMethods(gmmMethod, nClusters = 1:4) 

length(gmmMethods)

lmkm2 <- latrend(lmkmMethod, data = PAP.adh) 
summary(lmkm2)

lmkm3 <- latrend(lmkmMethod, nClusters = 3, data = PAP.adh)

lmkm3 <- update(lmkm2, nClusters = 3)

