library(caret)
library(mlbench)
library(conformalClassification)

#Function: pValues2PerfMetrics
#Desc: Computes performance measures: efficiency and validity of the conformal predictors
#input: matrix of p-values, test set and sifnificance level
#output: validity, efficiency, error rate and observed fuzziness
pValues2PerfMetrics = function(matPValues, testSet, sigfLevel = 0.05){
  if (is.null(matPValues) || is.null(testSet)){
    stop("\n 'matPValues' and 'testLabels' are required as input\n")
  }

  testLabels = testSet[,1]

  errRate = CPErrorRate(matPValues, testLabels, sigfLevel)
  eff = CPEfficiency(matPValues, testLabels, sigfLevel)
  val = CPValidity(matPValues, testLabels)
  obsFuzz = CPObsFuzziness(matPValues, testLabels)

  result = list(val = val, eff = eff, errRate = errRate, obsFuzz = obsFuzz)
  return(result)
}

#test ICP using DNA dataset form mlbench
.testICPDNA = function()
{
  #prepare Iris dataset
  data(DNA)
  originalData <- DNA
  nrAttr = ncol(originalData) #no of attributes
  #make sure first column is always the label
  #and class labels are always 1, 2, ...
  tempColumn = originalData[, 1]
  originalData[, 1] = originalData[, nrAttr]
  originalData[, nrAttr] = tempColumn
  originalData[, 1] = as.factor(originalData[, 1])
  originalData[, 1] = as.numeric(originalData[, 1])

  #partition into training and test set
  result = createDataPartition(originalData[, 1], p = 0.8, list = FALSE)

  trainingSet = originalData[result, ]
  testSet = originalData[-result, ]

  pValues = ICPClassification(trainingSet, testSet)
  perfVlaues = pValues2PerfMetrics(pValues, testSet)
  print(perfVlaues)
  CPCalibrationPlot(pValues, testSet, "blue")
}


#test TCP using DNA dataset form mlbench
.testParTCPDNA = function()
{
  #prepare DNA dataset
  data(DNA)
  originalData <- DNA
  nrAttr = ncol(originalData) #no of attributes
  #make sure first column is always the label
  #and class labels are always 1,2,...
  tempColumn = originalData[, 1]
  originalData[, 1] = originalData[, nrAttr]
  originalData[, nrAttr] = tempColumn
  originalData[, 1] = as.factor(originalData[, 1])
  originalData[, 1] = as.numeric(originalData[, 1])

  #partition into training and test set
  result = createDataPartition(originalData[, 1], p = 0.8, list = FALSE)

  trainingSet = originalData[result, ]
  testSet = originalData[-result, ]

  testSet = testSet[1:10, ]
  pValues = parTCPClassification(trainingSet, testSet)
  perfVlaues = pValues2PerfMetrics(pValues, testSet)
  print(perfVlaues)
  CPCalibrationPlot(pValues, testSet, "blue")
}
