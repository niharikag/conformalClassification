#############################################################
### ICP: Inductive Conformal Prediction for Classification
#        using Random Forest
#############################################################

#helper function
computeConformityScores = function(modelFit = NULL, calibrationSet = NULL)
{
  if(is.null(modelFit))
  {
    stop("Error: the input model is NULL")
  }

  if(is.null(calibrationSet))
  {
    stop("Error: calibrationSet is NULL")
  }

  #The first colum should be the class labels
  calibLabels = as.numeric(calibrationSet[, 1])

  predProb = predict(modelFit, calibrationSet[, -1], type="prob")
  nrLabels = ncol(predProb) # number of class labels

  MCListConfScores = list() #Moderian Class wise List of conformity scores
  for(i in 1:nrLabels)
  {
    classMembers = which(calibLabels == i)
    MCListConfScores[[i]] =  predProb[classMembers, i]
  }

  return(MCListConfScores)
}

computePValues = function(MCListConfScores = NULL, testConfScores = NULL)
{
  if(is.null(MCListConfScores) || is.null(testConfScores))
  {
    stop("Error: the list MCListConfScores is NULL")
    return(NULL)
  }

  nrTestCases = nrow(testConfScores)
  nrLabels = ncol(testConfScores)
  pValues = matrix(0, nrTestCases,  nrLabels)

  for(k in 1:nrTestCases)
  {
    for(l in 1:nrLabels)
    {
      alpha = testConfScores[k, l]
      classConfScores = MCListConfScores[[l]]
      pVal = length(which(classConfScores < alpha)) + runif(1)*length(which(classConfScores == alpha))
      pValues[k, l] = pVal/(length(classConfScores)+1)
    }
  }
  return(pValues)
}

#' Class-conditional Inductive conformal classifier for multi-class problems
#' @param trainingSet Training set
#' @param testSet Test set
#' @param ratioTrain The ratio for proper training set
#' @param method Method for modeling
#' @param nrTrees Number of trees for RF
#' @return The p-values
#' @export
ICPClassification = function(trainingSet, testSet,  ratioTrain = 0.7, method = "rf", nrTrees = 100)
{
  if(is.null(trainingSet) || is.null(testSet) )
  {
    stop("\n 'trainingSet' and 'testSet' are required as input\n")
  }

  nTrainSize = nrow(trainingSet)
  #create partition for proper-training set and calibration set.
  result = sample(1:nTrainSize,  ratioTrain*nTrainSize)

  calibSet = trainingSet[-result, ]
  properTrainSet = trainingSet[result, ]

  modelFit = fitModel(properTrainSet, method = method,  nrTrees = nrTrees)
  if(is.null(modelFit))
    return(NULL)

  MCListConfScores = computeConformityScores(modelFit, calibSet)
  testConfScores = predict(modelFit, testSet[, -1], type = "prob")
  pValues = computePValues(MCListConfScores, testConfScores)

  return(pValues)
}

