% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dataset.R
\name{discretizeEW,Dataset-method}
\alias{discretizeEW,Dataset-method}
\title{Discretize Dataset Using Equal Width}
\usage{
\S4method{discretizeEW}{Dataset}(obj, n_intervals)
}
\arguments{
\item{obj}{An object of class \code{\linkS4class{Dataset}} to be discretized.}

\item{n_intervals}{An integer specifying the number of intervals to create for discretization.}
}
\value{
A new \code{\linkS4class{Dataset}} object containing all the attributes discretized
}
\description{
This method discretizes the numeric or integer attributes of a \code{\linkS4class{Dataset}} object into specified intervals using equal width
Internally calls discretizeEW for each numeric/integer attribute
}
\examples{
# Create a Dataset object with numeric attributes
vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(6, 7, 8, 9, 10)
atributo1 <- atributoDataset("Attribute 1", vector1)
atributo2 <- atributoDataset("Attribute 2", vector2)
dataset <- createDataset("Example Dataset", list(atributo1, atributo2))

# Discretize the Dataset object into 3 intervals for each numeric attribute
discretized_dataset <- discretizeEW(dataset, n_intervals = 3)

print(discretized_dataset)


}
