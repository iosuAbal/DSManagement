library(ggplot2)
library(reshape2)
library(patchwork)


setGeneric(name="plotROC", def=function(obj, numeric_attr) standardGeneric("plotROC"))
#' Plot ROC Curve
#'
#' This method generates a Receiver Operating Characteristic (ROC) curve
#' for a given \code{\linkS4class{Dataset}} object based on a specified numeric attribute.
#' It computes the True Positive Rate (TPR) and False Positive Rate (FPR)
#' using the \code{\link{calculateRates}} function and visualizes the results.
#'
#' @param obj A \code{\linkS4class{Dataset}} object containing the dataset.
#' @param numeric_attr A character string specifying the name of the numeric attribute
#'                     to be used for calculating rates.
#'
#' @return A ggplot object representing the ROC curve.
#' @examples
#' dataset <- createDataset(nombre = "Example", atributos = list(
#'  atributoDataset(name = "NumericAttr", vector = sample(1:200, 200)),
#'  atributoDataset(name = "FactorAttr", vector = sample(c(FALSE, TRUE), size = 200, replace=TRUE))
#' ))
#' plotROC(dataset,"NumericAttr")
setMethod(f="plotROC", signature="Dataset",
          definition=function(obj, numeric_attr) {
          ratesMatrix <- calculateRates(obj,numeric_attr)

          ratesdf <- data.frame(TPR=ratesMatrix[,1] , FPR=ratesMatrix[,2])
          ggplot(ratesdf, aes(x=FPR, y=TPR)) +
          geom_area(fill = "blue", alpha = 0.3)+
          geom_line(linewidth=0.5) +
          geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "red", linewidth = 0.7,linetype = 2) +
          labs(title = "Area Under Curve",
               x = "FPR (False Positive Rate)",
               y = "TPR (True Positive Rate)") +
          theme_minimal()
})


setGeneric(name="plotCorrelation", def=function(obj) standardGeneric("plotCorrelation"))
#' Plot Correlation and Mutual Information Heatmaps
#'
#' This method generates heatmaps for both correlation and mutual information
#' matrices from a given \code{\linkS4class{Dataset}} object. It creates two
#' separate heatmaps displayed side by side without shared axes.
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} containing attributes
#'             for which the heatmaps will be calculated.
#'
#'
#' @return A \code{ggplot} object containing two independent heatmaps: one for
#'         correlation values and another for mutual information values.
#' @description This method internally uses \code{\link{correlation}}
#'
#' @examples
#' myDataset <- createDataset(nombre = "Example", atributos = list(
#'  atributoDataset(name = "N1", vector = c(1, 2, 3, 4)),
#'  atributoDataset(name = "F1", vector = c(FALSE,FALSE,FALSE, FALSE)),
#'  atributoDataset(name = "F2", vector = c(FALSE,FALSE,TRUE, FALSE)),
#'  atributoDataset(name = "N2", vector = c(28,5,19, 26)),
#'  atributoDataset(name = "N3", vector = c(4,5,6, 25))
#'))
#' plotCorrelation(myDataset)
#'
setMethod(f="plotCorrelation", signature="Dataset",
          definition=function(obj) {
            matrixes <- correlation(obj)
            matrixCor <- matrixes[[1]]
            matrixInf <- matrixes[[2]]


            dfCor <- melt(matrixCor)
            names(dfCor) <- c("Fila", "Columna", "Valor")
            dfCor$Tipo <- "Correlación"

            # Calcular el heatmap para la matriz de información mutua
            dfInf <- melt(matrixInf)
            names(dfInf) <- c("Fila", "Columna", "Valor")
            dfInf$Tipo <- "Información Mutua"

            # Heatmap para la matriz de correlación (-1 a 1)
            p1 <- ggplot(dfCor, aes(x = Fila, y = Columna, fill = Valor)) +
              geom_tile(color = "white") +
              scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0.0, limits = c(-1, 1)) +
              labs(title = "Heatmap de Correlación", x = "Columnas", y = "Filas") +
              theme_minimal() +
              theme(panel.grid = element_blank())

            # Heatmap para la matriz de información mutua (0 a 1)
            p2 <- ggplot(dfInf, aes(x = Fila, y = Columna, fill = Valor)) +
              geom_tile(color = "white") +
              scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 1)) +
              labs(title = "Heatmap de Información Mutua", x = "Columnas", y = "Filas") +
              theme_minimal() +
              theme(panel.grid = element_blank())

            # Combinar los dos gráficos
            p1 + p2


})


