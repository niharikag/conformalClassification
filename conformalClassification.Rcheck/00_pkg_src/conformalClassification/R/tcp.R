#############################################
### TCP: Transductive Conformal Prediction
#        for Classification using RF
#############################################

#' Class-conditional transductive conformal classifier for multi-class problems
#' @param trainSet Training set
#' @param testSet Test set
#' @param method Method for modeling
#' @param nrTrees Number of trees for RF
#' @return The p-values
#' @export
TCPClassification = function(trainSet, testSet, method = "rf", nrTrees = 100)
{
  if(is.null(trainSet) || is.null(testSet) )
  {
    stop("\n 'trainingSet' and 'testSet' are required as input\n")
  }

  nrTestCases = nrow(testSet)
  nrLabels = length(unique(testSet[, 1]))
  pValues = matrix(0, nrTestCases, nrLabels)

  for(i in 1:nrLabels){
    clsLabel = i
    for(k in 1:nrTestCases)
    {
      tempTestCase = testSet[k, ]
      tempTestCase[1]= clsLabel
      tcpTrainSet = rbind(trainSet, tempTestCase)
      pValues[k, i] = tcpPValues(tcpTrainSet, method = method, nrTrees = nrTrees)
    }
  }
  return(pValues)
}

