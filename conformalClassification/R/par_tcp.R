#############################################
### TCP: Transductive Conformal Prediction
#        for Classification using RF
#############################################
library(randomForest)
library(foreach)
library(doParallel)

# Note: This function is internal to the package, but global
# since it is shared across threads for parallel computing
#' Fits the model and computes p-values, internal to the package
#' @param augTrainSet Augmented training set
#' @param method Method for modeling
#' @param nrTrees Number of trees for RF
#' @return The p-values
tcpPValues = function(augTrainSet, method = "rf", nrTrees = 100)
{
     #first colum should be class labels
     names(augTrainSet)[1] <- "Class"

     augTrainSet$Class <- as.factor(augTrainSet$Class)
     obsLabels = as.numeric(augTrainSet[, 1]) #observed labels

     modelFit = fitModel(augTrainSet, method = method,  nrTrees = nrTrees)
     if(is.null(modelFit))
       return(NULL)

     testLabel = obsLabels[nrow(augTrainSet)] #test case class label

     #consider only those training samples that are labelled as testLabel
     classSamples = which(augTrainSet[, 1] == testLabel)

     predProbability = predict(modelFit, augTrainSet[classSamples, ], type="prob")

     nonconformityScores <- predProbability[, testLabel]

     testPredProb =  predProbability[nrow(predProbability), testLabel] #test case prediction probability
     pVal = length(which(nonconformityScores < testPredProb)) + runif(1)*length(which(nonconformityScores == testPredProb))
     pVal = pVal/(length(nonconformityScores))

     return(pVal)
}

#global/shared variables across the threads for parallel processing
pkg.env <- new.env()
pkg.env$gClsLabel <- 0
pkg.env$k <- 0

#' Class-conditional transductive conformal classifier for multi-class problems, paralled computations
#' @param trainSet Training set
#' @param testSet Test set
#' @param method Method for modeling
#' @param nrTrees Number of trees for RF
#' @param nrClusters Number of clusters
#' @return The p-values
#' @export
parTCPClassification = function(trainSet, testSet, method = "rf", nrTrees = 100, nrClusters = 12)
{
  if(is.null(trainSet) || is.null(testSet) )
  {
    stop("\n 'trainingSet' and 'testSet' are required as input\n")
  }

  nrTestCases = nrow(testSet)
  nrLabels = length(unique(testSet[, 1]))
  pValues = matrix(0, nrTestCases, nrLabels)

  for(i in 1:nrLabels){
    pkg.env$gClsLabel <- i
    cl<-makeCluster(nrClusters)
    clusterExport(cl, c("tcpPValues"))
    registerDoParallel(cl)

    fits <- foreach(pkg.env$k <- 1:nrTestCases, .combine = "rbind", .packages = c("randomForest")) %dopar%
    {
        tempTestCase = testSet[pkg.env$k,]
        tempTestCase[1] = pkg.env$gClsLabel
        tcpTrainSet  =  rbind(trainSet, tempTestCase)
        tcpPValues(tcpTrainSet, method = method, nrTrees  =  nrTrees)
    }
    pValues[, i] = fits
    stopCluster(cl)
  }

  return(pValues)
}
