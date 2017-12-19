#' A Conformal Prediction  R Package for Classification
#'
#' The conformalClassification package implements Transductive Conformal Prediction (TCP) and
#' Inductive Conformal Prediction (ICP) for classification problems.
#'
#' Currently, the pakcage is built upon random forests method, where voting of random forests for each
#' class is considered as a conformity scores for each data point. Mainly the package generates
#' conformal prediction errors (p-values) for classification problems, it also provides various diagnostic
#' measures such as deviation from alidity, error rate, efficiency, observed fuzziness and calibration plots.
#' In future releases, we plan to extend package to use other
#' machine learning algorithms, (i.e. support vector machine) for model fitting.
#'
#' @docType package
#' @name conformalClassification
"_PACKAGE"
