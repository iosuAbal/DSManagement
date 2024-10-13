## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(DSManagement) 
library(ggplot2)
library(reshape2)
library(patchwork)


## -----------------------------------------------------------------------------
numeric_attr <- atributoDataset("Numeric Attribute", seq(1:20))
logical_attr <- atributoDataset("Logical Attribute", sample(c(FALSE, TRUE), size = 20, replace = TRUE))

logical_attr

## -----------------------------------------------------------------------------
my_dataset <- createDataset("My Dataset", list(numeric_attr, logical_attr))
my_dataset

## -----------------------------------------------------------------------------
dsetCSV <- datasetFromCSV("../data/fichero.csv")
dsetCSV

## -----------------------------------------------------------------------------
discretizeEW(numeric_attr, 3)

## ----eval=TRUE----------------------------------------------------------------
discretizeEF(dsetCSV, 2)

## -----------------------------------------------------------------------------
calculateMetrics(my_dataset)

## -----------------------------------------------------------------------------
atributo <- atributoDataset(name = "Attribute1", vector = c(1, 2, 3, 4, 5))
normalize(atributo)

## -----------------------------------------------------------------------------
dataset <- createDataset(nombre = "Example", atributos = list( 
  atributoDataset(name = "Numeric1", vector = c(1, 1.5, 3,2, 5)),
  atributoDataset(name = "Numeric2", vector = c(21, 31, 60, 76)),
  atributoDataset(name = "FactorAttr", vector = factor(c("A", "B", "A", "A")))))
standarize(dataset)

## -----------------------------------------------------------------------------
dataset <- createDataset(nombre = "Example", atributos = list(
  atributoDataset(name = "NumericAttr", vector = c(1, 2, 3, 4)),
  atributoDataset(name = "FactorAttr", vector = factor(c("rojo","verde","verde", "verde"))),
  atributoDataset(name = "FactorAttr2", vector = factor(c("rojo","verde","azul","amarillo"))),
  atributoDataset(name = "LogicalAttr", vector = (c(FALSE, TRUE, FALSE, TRUE)))


))
newDs <- filterMetrics(dataset,"Entropy","<",1)
newDs

## -----------------------------------------------------------------------------
dataset <- createDataset(nombre = "Example", atributos = list(
  atributoDataset(name = "NumericAttr", vector = c(1, 2, 3, 4)),
  atributoDataset(name = "FactorAttr", vector = c(TRUE,TRUE,FALSE, FALSE)),
  atributoDataset(name = "FactorAttr2", vector = c(FALSE,FALSE,TRUE, FALSE)),
  atributoDataset(name = "NumericAttr2", vector = c(28, 5, 19, 24)),
  atributoDataset(name = "NumericAttr3", vector = c(4, 5, 6, 7))


))
correlation(dataset)

## -----------------------------------------------------------------------------
dataset <- createDataset(nombre = "Example", atributos = list(
  atributoDataset(name = "NumericAttr", vector = sample(1:200, 200)),
  atributoDataset(name = "FactorAttr", vector = sample(c(FALSE, TRUE), size = 200, replace=T))

))
plotROC(dataset,"NumericAttr")

## -----------------------------------------------------------------------------
calculateMetrics(dataset)$NumericAttr['AUC']

## ----fig.width=10, fig.height=4-----------------------------------------------
dataset <- createDataset(nombre = "Example", atributos = list(
  atributoDataset(name = "N1", vector = c(1, 2, 3, 4)),
  atributoDataset(name = "F1", vector = c(FALSE,FALSE,FALSE, FALSE)),
  atributoDataset(name = "F2", vector = c(FALSE,FALSE,TRUE, FALSE)),
  atributoDataset(name = "N2", vector = c(28,5,19, 26)),
  atributoDataset(name = "N3", vector = c(4,5,6, 25))


))
plotCorrelation(dataset)

