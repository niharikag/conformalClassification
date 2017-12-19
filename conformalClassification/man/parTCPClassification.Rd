\name{parTCPClassification}
\alias{parTCPClassification}
\title{Class-conditional transductive conformal classifier for multi-class problems, paralled computations}
\usage{
parTCPClassification(trainSet, testSet, method = "rf", nrTrees = 100, nrClusters = 12)
}
\arguments{
\item{testSet}{Test set}

\item{method}{Method for modeling}

\item{nrTrees}{Number of trees for RF}

\item{nrClusters}{Number of clusters}

\item{trainSet}{Training set}
}
\value{
The p-values
}
\description{
Class-conditional transductive conformal classifier for multi-class problems, paralled computations
}
\seealso{
  \code{\link{TCPClassification}}.
  \code{\link{ICPClassification}}.
}
\examples{
## load the library
#library(mlbench)
#library(caret)
#library(conformalClassification)

## load the DNA dataset
#data(DNA)
#originalData <- DNA

## make sure first column is always the label and class labels are always 1, 2, ...
#nrAttr = ncol(originalData) #no of attributes
#tempColumn = originalData[, 1]
#originalData[, 1] = originalData[, nrAttr]
#originalData[, nrAttr] = tempColumn
#originalData[, 1] = as.factor(originalData[, 1])
#originalData[, 1] = as.numeric(originalData[, 1])

## partition the data into training and test set
#result = createDataPartition(originalData[, 1], p = 0.8, list = FALSE)
#trainingSet = originalData[result, ]
#testSet = originalData[-result, ]

##ICP classification
#pValues = parTCPClassification(trainingSet, testSet)
#perfVlaues = pValues2PerfMetrics(pValues, testSet)
#print(perfVlaues)
#CPCalibrationPlot(pValues, testSet, "blue")
#not run
}
