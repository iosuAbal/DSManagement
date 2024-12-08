% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{plotCorrelation,Dataset-method}
\alias{plotCorrelation,Dataset-method}
\title{Plot Correlation and Mutual Information Heatmaps}
\usage{
\S4method{plotCorrelation}{Dataset}(obj)
}
\arguments{
\item{obj}{An object of class \code{\linkS4class{Dataset}} containing attributes
for which the heatmaps will be calculated.}
}
\value{
A \code{ggplot} object containing two independent heatmaps: one for
        correlation values and another for mutual information values.
}
\description{
This method internally uses \code{\link{correlation}}
}
\details{
This method generates heatmaps for both correlation and mutual information
matrices from a given \code{\linkS4class{Dataset}} object. It creates two
separate heatmaps displayed side by side without shared axes.
}
\examples{
myDataset <- createDataset(nombre = "Example", atributos = list(
 atributoDataset(name = "N1", vector = c(1, 2, 3, 4)),
 atributoDataset(name = "F1", vector = c(FALSE,FALSE,FALSE, FALSE)),
 atributoDataset(name = "F2", vector = c(FALSE,FALSE,TRUE, FALSE)),
 atributoDataset(name = "N2", vector = c(28,5,19, 26)),
 atributoDataset(name = "N3", vector = c(4,5,6, 25))
))
plotCorrelation(myDataset)

}
