#############################################################
### Common: This file contains the common functions which can
###         be shared among TCP and ICP
#############################################################

library(randomForest)

#' This function is internal to the package
#' @param trainingSet The training set
#' @param method Method for modeling
#' @param nrTrees Number of trees for RF
#' @return The fitted model
fitModel = function(trainingSet=NULL, method = "rf",  nrTrees = 100)
{
  if(is.null(trainingSet))
  {
    stop("Error: training set is NULL")
    return(NULL)
  }

  if(method != "rf")
  {
    stop("Error: only random forest is supported in the current release")
    return(NULL)
  }

  #the first colum should be class labels, labeled as 1, 2, ...
  names(trainingSet)[1] <- "Class"

  trainingSet$Class <- as.factor(trainingSet$Class)

  rfModel <- randomForest(Class ~ ., data = trainingSet, ntree = nrTrees,
                          norm.votes = TRUE, keep.forest = TRUE,
                          predict.all = TRUE, type = "classification")

  return(rfModel)
}


#' Computes efficiency of a conformal predictor, which is defined as the
#' ratio of predictions with more than one class over the size of the testset
#' @param matPValues Matrix of p-values
#' @param testLabels True labels for the test-set
#' @param sigfLevel Significance level
#' @return The efficiency
#' @export
CPEfficiency = function(matPValues, testLabels, sigfLevel = 0.05)
{
  if (is.null(matPValues) || is.null(testLabels)){
    stop("\n 'matPValues' and 'testLabels' are required as input\n")
  }

  nrTestCases = length(testLabels) #size of the test set

  signifTest = (matPValues  > sigfLevel)*1 #compute the prediction region

  err = 0
  for(i in 1:nrTestCases)
  {
    err = err + ( (sum(signifTest[i, ]) > 1) * 1 )
  }
  result = err/nrTestCases

  return(result)
}

#' Computes error rate of a conformal predictor, which is defined as
#' the ratio of predictions with missing true class lables over the size of the testset
#' @param matPValues Matrix of p-values
#' @param testLabels True labels for the test-set
#' @param sigfLevel Significance level
#' @return The error rate
#' @export
CPErrorRate = function(matPValues, testLabels, sigfLevel = 0.05)
{
  if (is.null(matPValues) || is.null(testLabels)){
    stop("\n 'matPValues' and 'testLabels' are required as input\n")
  }

  nrTestCases = length(testLabels)

  signifTest = (matPValues  > sigfLevel)*1

  err = 0
  for(i in 1:nrTestCases)
  {
    err = err + (signifTest[i,testLabels[i]] == 0)*1
  }
  result = err/nrTestCases

  return(result)
}

#' Computes observed fuzziness, which is defined as
#' the sum of all p-values for the incorrect class labels.
#' @param matPValues Matrix of p-values
#' @param testLabels True labels for the test-set
#' @return The observed fuzziness
#' @export
CPObsFuzziness = function(matPValues, testLabels)
{
  if (is.null(matPValues) || is.null(testLabels)){
    stop("\n 'matPValues' and 'testLabels' are required as input\n")
  }

  nrTestCases = length(testLabels)

  sumPValues = 0
  for(indxTestSet in 1:nrTestCases)
  {
    exclude = testLabels[indxTestSet] #exclude the p-value of the true label
    sumPValues = sumPValues + sum(matPValues[indxTestSet, -exclude])
  }
  result = sumPValues/nrTestCases
  return(result)
}


#' Computes the deviation from exact validity as the Euclidean norm of
#' the difference of the observed error and the expected error
#' @param matPValues Matrix of p-values
#' @param testLabels True labels for the test-set
#' @return The deviation from exact validity
#' @export
CPValidity = function(matPValues = NULL, testLabels = NULL)
{
  if (is.null(matPValues) || is.null(testLabels)){
    stop("\n 'matPValues' and 'testLabels' are required as input\n")
  }

  signifSet = seq(.01, .99, by=.01) #significance level set

  nrTestCases = length(testLabels)
  errAtSignif = rep(0, length(signifSet))

  for(indx in 1: length(signifSet)){
    signifTest = (matPValues  > signifSet[indx])*1

    err = 0
    for(i in 1:nrTestCases)
    {
      err = err + ( (signifTest[i, testLabels[i]] == 0) * 1 )
    }
    err = err/nrTestCases
    errAtSignif[indx] = (err - signifSet[indx])^2
  }

  result = sqrt(sum(errAtSignif))
  return(result)
}

#' Plots the calibration plot
#' @param pValues Matrix of p-values
#' @param testSet The test set
#' @param color colour of the calibration line
#' @return NULL
#' @export
CPCalibrationPlot = function(pValues, testSet, color="blue")
{
  if (is.null(pValues) || is.null(testSet)){
    stop("\n 'pValues' and 'testSet' are required as input\n")
  }

  testLabels = testSet[, 1] #True class labels
  nrTestCases = length(testLabels)

  #compute error rate for the range of significance levels
  sigLevels = seq(0,1, .01)
  errorRate = rep(1, length(sigLevels))
  for(i in 1:(length(sigLevels) - 1)){
    signifTest = (pValues  > sigLevels[i])*1
    err = 0
    for(j in 1:nrTestCases)
    {
      err = err + ( (signifTest[j, testLabels[j]] == 0)*1 )
    }

    errorRate[i] = err/nrTestCases
  }

  plot(sigLevels, errorRate, type = "l", xlab = "significance", ylab = "error rate",
       col = color)
  abline(0, 1, col="red")
}
