% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dataset.R
\name{standarize,Dataset-method}
\alias{standarize,Dataset-method}
\title{Standardize Attributes for Dataset Objects}
\usage{
\S4method{standarize}{Dataset}(obj)
}
\arguments{
\item{obj}{An object of class \code{\linkS4class{Dataset}} containing a list
of attributes to be standardized.}
}
\value{
An object of class \code{\linkS4class{Dataset}} with standardized
numeric and integer attributes, while other attributes remain the same
}
\description{
This method standardizes the numeric and integer attributes of an object
of class \code{\linkS4class{Dataset}}
}
\details{
The method iterates over each attribute in the dataset and calls
the \code{\link{standarize}} method from the \code{\linkS4class{AtributoDataset}}
class for numeric and integer attributes.
}
\examples{
dataset <- createDataset(nombre = "Example", atributos = list(
  atributoDataset("Months", c(1, 2, 3)),
  atributoDataset("FactorAttr", factor(c("A", "B", "A")))))
standarize(dataset)
}
