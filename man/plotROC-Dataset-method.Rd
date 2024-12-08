% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{plotROC,Dataset-method}
\alias{plotROC,Dataset-method}
\title{Plot ROC Curve}
\usage{
\S4method{plotROC}{Dataset}(obj, numeric_attr)
}
\arguments{
\item{obj}{A \code{\linkS4class{Dataset}} object containing the dataset.}

\item{numeric_attr}{A character string specifying the name of the numeric attribute
to be used for calculating rates.}
}
\value{
A ggplot object representing the ROC curve.
}
\description{
This method generates a Receiver Operating Characteristic (ROC) curve
for a given \code{\linkS4class{Dataset}} object based on a specified numeric attribute.
It computes the True Positive Rate (TPR) and False Positive Rate (FPR)
using the \code{\link{calculateRates}} function and visualizes the results.
}
\examples{
dataset <- createDataset(nombre = "Example", atributos = list(
 atributoDataset(name = "NumericAttr", vector = sample(1:200, 200)),
 atributoDataset(name = "FactorAttr", vector = sample(c(FALSE, TRUE), size = 200, replace=TRUE))
))
plotROC(dataset,"NumericAttr")
}
