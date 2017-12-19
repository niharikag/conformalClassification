\name{CPCalibrationPlot}
\alias{CPCalibrationPlot}
\title{Plots the calibration plot}
\usage{
CPCalibrationPlot(pValues, testSet, color = "blue")
}
\arguments{
\item{testSet}{The test set}

\item{color}{colour of the calibration line}

\item{pValues}{Matrix of p-values}
}
\description{
Plots the calibration plot
}
\seealso{
  \code{\link{CPEfficiency}},
  \code{\link{CPErrorRate}},
  \code{\link{CPValidity}},
  \code{\link{CPObsFuzziness}}.
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
CPCalibrationPlot(pValues, testSet, "blue")
}
