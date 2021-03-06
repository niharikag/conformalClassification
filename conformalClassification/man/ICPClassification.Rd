\name{ICPClassification}
\alias{ICPClassification}
\title{Class-conditional Inductive conformal classifier for multi-class problems}
\usage{
ICPClassification(trainingSet, testSet, ratioTrain = 0.7, method = "rf",
  nrTrees = 100)
}
\arguments{
\item{trainingSet}{Training set}

\item{testSet}{Test set}

\item{ratioTrain}{The ratio for proper training set}

\item{method}{Method for modeling}

\item{nrTrees}{Number of trees for RF}
}
\value{
The p-values
}
\description{
Class-conditional Inductive conformal classifier for multi-class problems
}
\seealso{
  \code{\link{TCPClassification}},
  \code{\link{parTCPClassification}}.
}
\examples{
## load the library
library(mlbench)
#library(caret)
library(conformalClassification)

## load the DNA dataset
data(DNA)
originalData <- DNA

## make sure first column is always the label and class labels are always 1, 2, ...
nrAttr = ncol(originalData) #no of attributes
tempColumn = originalData[, 1]
originalData[, 1] = originalData[, nrAttr]
originalData[, nrAttr] = tempColumn
originalData[, 1] = as.factor(originalData[, 1])
originalData[, 1] = as.numeric(originalData[, 1])

## partition the data into training and test set
#result = createDataPartition(originalData[, 1], p = 0.8, list = FALSE)
size = nrow(originalData)
result = sample(1:size,  0.8*size)

trainingSet = originalData[result, ]
testSet = originalData[-result, ]

##ICP classification
pValues = ICPClassification(trainingSet, testSet)
#perfVlaues = pValues2PerfMetrics(pValues, testSet)
#print(perfVlaues)
#CPCalibrationPlot(pValues, testSet, "blue")
}
