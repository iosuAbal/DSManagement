% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dataset.R
\docType{class}
\name{AtributoDataset-class}
\alias{AtributoDataset-class}
\title{AtributoDataset Class}
\description{
This class represents a dataset attribute, containing its name, type, values, and the cuts after discretizing it.
}
\section{Slots}{

\describe{
\item{\code{nombre}}{A character string representing the name of the attribute.}

\item{\code{tipo}}{A character string indicating the type of the attribute (e.g., "numeric", "integer", "factor", etc.).}

\item{\code{valores}}{A vector of any type containing the values of the attribute.}

\item{\code{cortes}}{A vector or any type that may contain information about cuts or discretization applied to the values.}
}}

