# conformalClassification

## Maintained by Niharika Gauraha

The conformalClassification package implements Transductive Conformal Prediction (TCP) and Inductive Conformal Prediction (ICP) for classification problems. Conformal Prediction (CP) is a framework that complements the predictions of machine learning algorithms with reliable measures of confidence. TCP gives results with higher validity than ICP, however ICP is computationally faster than TCP. The package  conformalClassification is built upon the random forest method, where votes of the random forest for each class are considered as the conformity scores for each data point. Although the main aim of the conformalClassification package is to generate CP errors (p-values) for classification problems, the package also implements various diagnostic measures such as deviation from validity, error rate, efficiency, observed fuzziness and calibration plots. In future releases, we plan to extend  the package to use other machine learning algorithms, (e.g. support vector machines) for model fitting.

The package "conformalClassification" can be installed as an R package, which contains the software tools, and the files "conformalClassification.pdf" and "CPBackground.pdf".

The document file "conformalClassification.pdf" lists the functions and its usage supported by the "conformalClassification" R package.

The document file "CPBackground.pdf" briefly explains the theory of conformal predictions in Classification problems.

The directory "examples" contains R code to explain the usage of the "conformalClassification" R package.

