# Load required packages
library(data.table)
library(doParallel)
library(doMC)
library("latrend") 
library("klm")

# Define lcMethodExample Class
setClass("lcMethodExample", contains = "lcMethod")

setMethod("getArgumentDefaults", "lcMethodExample", function(object) {
  c(
    formals(stats::kmeans),
    time = quote(getOption("latrend.time")),
    id = quote(getOption("latrend.id")),
    nClusters = 2,
    callNextMethod()
  )
})

setMethod("getName", "lcMethodExample", function(object) "simple example method")
setMethod("getShortName", "lcMethodExample", function(object) "example")

# Instantiate the Method
new("lcMethodExample", nClusters = 3)

# Define the Fit Function
setMethod("fit", "lcMethodExample", function(method, data, envir, verbose, ...) {
  fittedRepresentation <- CODE_HERE
  new("lcModelExample", data = data, model = fittedRepresentation,
      method = method, clusterNames = make.clusterNames(method$nClusters))
})

# Feature-based Clustering Example
repStep <- function(method, data, verbose) {
  repTraj <- function(trajData) {
    lm.rep <- lm(method$formula, data = trajData)
    coef(lm.rep)
  }
  dt <- as.data.table(data)
  coefData <- dt[, as.list(repTraj(.SD)), keyby = c(method$id)]
  coefMat <- as.matrix(subset(coefData, select = -1))
  rownames(coefMat) <- coefData[[method$id]]
  coefMat
}

clusStep <- function(method, data, repMat, envir, verbose) {
  km <- kmeans(repMat, centers = method$nClusters)
  lcModelPartition(response = responseVariable(method), method = method,
                   data = data, trajectoryAssignments = km$cluster, center = mean)
}

# Specifying and Estimating the Method
tsMethod <- lcMethodFeature(response = "UsageHours", formula = UsageHours ~ Week,
                            representationStep = repStep, clusterStep = clusStep)
tsModel <- latrend(tsMethod, data = PAP.adh, nClusters = 5)

# Parallel Configuration
nCores <- parallel::detectCores(logical = FALSE)
if (.Platform$OS.type == "windows") {
  doParallel::registerDoParallel(parallel::makeCluster(nCores))
} else {
  doMC::registerDoMC(nCores)
}

# Define and Batch Estimation Methods
kmlMethods <- lcMethods(kmlMethod, nClusters = 1:6)
kmlList <- latrendBatch(kmlMethods, data = PAP.adh, parallel = TRUE, seed = 1)
gbtmList <- latrendBatch(gbtmMethods, data = PAP.adh, parallel = TRUE, seed = 1)
gmmList <- latrendBatch(gmmMethods, data = PAP.adh, parallel = TRUE, seed = 1)
