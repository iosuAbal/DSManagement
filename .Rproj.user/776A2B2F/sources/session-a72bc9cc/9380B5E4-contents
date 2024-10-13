#' AtributoDataset Class
#'
#' This class represents a dataset attribute, containing its name, type, values, and the cuts after discretizing it.
#'
#' @slot nombre A character string representing the name of the attribute.
#' @slot tipo A character string indicating the type of the attribute (e.g., "numeric", "integer", "factor", etc.).
#' @slot valores A vector of any type containing the values of the attribute.
#' @slot cortes A vector or any type that may contain information about cuts or discretization applied to the values.
#'
setClass("AtributoDataset",
         slots = c(nombre = "character",
                   tipo = "character",
                   valores = "ANY",
                   cortes = "ANY")
)


#' Basic constructor of an AtributoDataset Object
#'
#' @description This function creates an instance of the s \code{\linkS4class{AtributoDataset}} class.
#'
#' @param name A character string representing the name of the attribute.
#' @param vector A vector of values for the attribute. The type of this vector determines the 'tipo' of the attribute.
#' @param cortes An optional parameter that can contain information about cuts or discretization applied to the values. Default is NULL.
#'
#' @return An object of class \code{\linkS4class{AtributoDataset}} containing the name, type, values, and cuts of the attribute.
#'
atributoDataset <- function (name, vector,cortes=NULL) {
  tipo <- if (is.numeric(vector)) {
    if (all(as.integer(vector) == vector)) {
      "integer"
    } else {
      "numeric"
    }
  } else if (is.factor(vector)) {
    "factor"
  } else if (is.logical(vector)) {
    "logical"
  } else {
    "character"
  }
  object <- new("AtributoDataset", nombre=name, tipo=tipo, valores=vector, cortes=cortes)
  return(object)
}



validarAtributo <- function(object) {
  # Verificar que el nombre no esté vacío
  if (nchar(object@nombre) == 0) {
    stop("El nombre del atributo no puede estar vacío.")
  }
  if (nchar(object@tipo) == 0) {
    stop("El tipo del atributo no puede estar vacío.")
  }
  if (length(object@valores) == 0) {
    stop("Los valores del atributo no pueden estar vacíos.")
  }
  return(TRUE)
}

setValidity(Class="AtributoDataset", method=validarAtributo)



#' Dataset Class
#'
#' This class represents a dataset containing a name and a list of attributes.
#'
#' @slot nombre A character string representing the name of the dataset.
#' @slot atributos A list of objects of class AtributoDataset, representing the attributes of the dataset.
#'
setClass("Dataset",
         slots = c(nombre = "character",
                   atributos = "list")  # Lista de objetos AtributoDataset
)

#' Create a Dataset Object
#'
#' @description This function creates an instance of the Dataset class.
#'
#' @param nombre A character string representing the name of the dataset.
#' @param atributos A list of objects of class AtributoDataset representing the attributes of the dataset.
#'
#' @return An object of class Dataset containing the name and attributes.
#'
createDataset <- function (nombre, atributos) {
  object <- new("Dataset", nombre=nombre, atributos=atributos)
  return(object)
}
#no hace falta documentar
validarDataset <- function(object) {
  #La clase debe ser de tipo character
  if (!is.character(object@nombre)) {
    stop("El nombre debe ser de tipo character.")
  }

  # Atributos deben ser objetos de la clase AtributoDataset
  if (!all(sapply(object@atributos, function(x) "AtributoDataset" %in% class(x)))) {
    stop("Todos los elementos de 'atributos' deben ser de la clase 'AtributoDataset'.")
  }

  # Verificar que los nombres de los atributos sean únicos
  nombres_atributos <- sapply(object@atributos, function(x) x@nombre)
  if (anyDuplicated(nombres_atributos)) {
    stop("Los nombres de los atributos deben ser únicos.")
  }

  if(length(object@atributos)==0){
    stop("El dataset debe tener una lista de atributos")
  }

  return(TRUE)
}
setValidity(Class="Dataset", method=validarDataset)

#' Create a Dataset Object from a CSV File
#'
#' This function reads a CSV file and creates an instance of the \code{\linkS4class{Dataset}} class.
#'
#' @param file_path A character string representing the path to the CSV file.
#' @param sep A character string specifying the separator used in the CSV file (default is a comma).
#'
#' @return An object of class \code{\linkS4class{Dataset}} containing the name derived from the CSV filename and the attributes represented as \code{\linkS4class{AtributoDataset}} objects.
#'
datasetFromCSV <- function(file_path, sep=",") {
  # Leer el archivo CSV en un data.frame
  df <- read.csv(file_path, sep=sep)

  # Crear una lista de objetos AtributoDataset para cada columna del data.frame
  atributos <- lapply(names(df), function(col_name) {
    vector <- df[[col_name]]

    # Crear un objeto AtributoDataset para cada columna
    atributo <- atributoDataset(col_name, vector)

    return(atributo)
  })

  # Crear el objeto Dataset con la lista de atributos y la clase (si se proporciona)
  dataset <- new("Dataset", nombre=basename(file_path), atributos = atributos)

  return(dataset)
}


# Ejercicio1 --------------------------------------------------------------


setGeneric(name="discretizeEW", def=function(obj, n_intervals) standardGeneric("discretizeEW"))
setGeneric(name="discretizeEF", def=function(obj, n_intervals) standardGeneric("discretizeEF"))


#' Discretize AtributoDataset Using Equal Width
#'
#' This method discretizes the numeric or integer values of an \code{\linkS4class{AtributoDataset}} object into specified intervals using equal width.
#' Equal width means all intervals have the same length
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}} to be discretized.
#' @param n_intervals An integer specifying the number of intervals to create for discretization.
#'
#' @return A new \code{AtributoDataset} object containing the discretized values and cut points.
#' @examples
#' # Create an AtributoDataset object
#' vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' atributo <- atributoDataset("Test", vector)
#'
#' # Discretize the AtributoDataset object into 3 intervals
#' discretized_atributo <- discretizeEW(atributo, 3)
#'
#' # View the discretized values and cut points
#' print(discretized_atributo)
#'
setMethod(f="discretizeEW", signature="AtributoDataset",
          definition=function(obj, n_intervals) {
            if (obj@tipo != "numeric" && obj@tipo != "integer") {
              stop("El atributo no es numérico o entero, no se puede discretizar.")
            }
            # Aplicar tu función discretizeEW a los valores del objeto
            discretized_values <- discretizeEW_base(obj@valores, n_intervals)

            # Retornar un nuevo objeto AtributoDataset con los valores discretizados
            return(atributoDataset(paste(obj@nombre,"- Discretized(EW)"),discretized_values$vector, discretized_values$cut.points))
          })

#' Discretize Dataset Using Equal Width
#'
#' This method discretizes the numeric or integer attributes of a \code{\linkS4class{Dataset}} object into specified intervals using equal width
#' Internally calls discretizeEW for each numeric/integer attribute
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} to be discretized.
#' @param n_intervals An integer specifying the number of intervals to create for discretization.
#'
#' @return A new \code{\linkS4class{Dataset}} object containing all the attributes discretized
#'
#' @examples
#' # Create a Dataset object with numeric attributes
#' vector1 <- c(1, 2, 3, 4, 5)
#' vector2 <- c(6, 7, 8, 9, 10)
#' atributo1 <- atributoDataset("Attribute 1", vector1)
#' atributo2 <- atributoDataset("Attribute 2", vector2)
#' dataset <- createDataset("Example Dataset", list(atributo1, atributo2))
#'
#' # Discretize the Dataset object into 3 intervals for each numeric attribute
#' discretized_dataset <- discretizeEW(dataset, n_intervals = 3)
#'
#' print(discretized_dataset)
#'
#'
setMethod(f="discretizeEW", signature="Dataset",
          definition=function(obj, n_intervals) {
            atributos_discretized <- lapply(obj@atributos, FUN = function(atributo) {
              if (atributo@tipo == "numeric" || atributo@tipo == "integer") {
                return(discretizeEW(atributo, n_intervals))  # Llamar a la implementación de discretize en AtributoDataset
              }
              return(atributo)
            })

            return(createDataset(nombre = obj@nombre, atributos = atributos_discretized))
          })


#' Discretize AtributoDataset Using Equal Frequency
#'
#' This method discretizes the numeric or integer values of an \code{\linkS4class{AtributoDataset}} object into specified intervals using equal frequency
#' Thanks to this discretizing technique all intervals have the same amount of items +-1
#' In the case that the vector contains duplicated values, sometimes it will be imposible to divide all intervals perfectly.
#' So, if the cut point is a duplicated value in the vector, we have decided to add those duplicated values into the lower interval
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}} to be discretized.
#' @param n_intervals An integer specifying the number of intervals to create for discretization.
#'
#' @return A new \code{\linkS4class{AtributoDataset}} object containing the discretized values and cut points.
#'
#' @examples
#' # Create an AtributoDataset object
#' vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' atributo <- atributoDataset("Example Attribute", vector)
#'
#' # Discretize the AtributoDataset object into 3 intervals
#' discretized_atributo <- discretizeEF(atributo, n_intervals = 3)
#'
setMethod(f="discretizeEF", signature="AtributoDataset",
          definition=function(obj, n_intervals) {
            if (obj@tipo != "numeric" && obj@tipo != "integer") {
              stop("El atributo no es numérico o entero, no se puede discretizar.")
            }
            discretized_values <- discretizeEF_base(obj@valores, n_intervals)

            return(atributoDataset(paste(obj@nombre,"- Discretized(EF)"),discretized_values$vector, discretized_values$cut.points))
          })

#' Discretize Dataset Using Equal Frequency
#'
#' This method discretizes the numeric or integer attributes of a \code{\linkS4class{Dataset}} object into specified intervals using equal frequency
#' This method internally calls the implementation of \code{discretizeEF} for \code{AtributoDataset}
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} to be discretized.
#' @param n_intervals An integer specifying the number of intervals to create for discretization.
#'
#' @return A new \code{\linkS4class{Dataset}} object where all its values are equally discretized.
#'
#' @examples
#' vector1 <- c(1, 2, 3, 4, 5)
#' vector2 <- c(6, 7, 8, 9, 10)
#' atributo1 <- atributoDataset("Attribute 1", vector1)
#' atributo2 <- atributoDataset("Attribute 2", vector2)
#' dataset <- createDataset("Example Dataset", list(atributo1, atributo2))
#' discretized_dataset <- discretizeEF(dataset, n_intervals = 3)
#'
setMethod(f="discretizeEF", signature="Dataset",
          definition=function(obj,n_intervals) {
            atributos_discretized <- lapply(obj@atributos, FUN = function(atributo) {
              if (atributo@tipo == "numeric" || atributo@tipo == "integer") {
                return(discretizeEF(atributo,n_intervals))  # Llamar a la implementación de discretize en AtributoDataset
              }
              return(atributo)
            })

            return(createDataset(nombre = obj@nombre, atributos = atributos_discretized))
          })



# ejercicio2 --------------------------------------------------------------

#TPR
setGeneric(name="calculateRates", def=function(obj,numeric_attr_name) standardGeneric("calculateRates"))
#' Calculate True Positive Rate (TPR) and False Positive Rate (FPR)
#' @description This method calculates the TPR and FPR for each cut point in AttributeDataset values.
#' @param obj An object of class \code{\linkS4class{Dataset}}.
#' @param numeric_attr_name A string which indicates the name of the numeric attribute
#' to be used for ordering the dataset.
#'
#' @return A matrix where each row contains the TPR and FPR for different cut points.
#'
#' @examples
#' numeric_attr <- atributoDataset("Numeric Attribute", c(1, 2, 3, 4, 5))
#' logical_attr <- atributoDataset("Logical Attribute", c(TRUE, FALSE, TRUE, FALSE, TRUE))
#' my_dataset <- createDataset("My Dataset", list(numeric_attr, logical_attr))
#' calculateRates(my_dataset, "Numeric Attribute")
setMethod(f="calculateRates", signature="Dataset",
          definition=function(obj, numeric_attr_name) {
            # Obtener el atributo numérico especificado
            numeric_attr <- obj@atributos[[which(sapply(obj@atributos, function(attr) attr@nombre == numeric_attr_name))]]

            if (is.null(numeric_attr) || (numeric_attr@tipo != "numeric" && numeric_attr@tipo != "integer")) {
              stop("El atributo especificado no es numérico o entero o no se ha encontrado.")
            }
            if(!any(sapply(obj@atributos, function(attr) attr@tipo == "logical"))){
              stop("No existe ningun atributo lógico con el clasificar el AUC")
            }

            sorted_indices <- order(numeric_attr@valores)

            # cogemos el primer atributo lógico que haya
            logical_attr <- Filter(function(attr) attr@tipo == "logical", obj@atributos)[[1]]

            valores_logicos <- logical_attr@valores[sorted_indices]
            n <- length(valores_logicos)
            predicciones <- rep(FALSE, n)

            # habra tantos puntos de corte como valores en el dataframe
            results <- sapply(1:n, function(cut_point) {
              predicciones <- c(rep(FALSE, cut_point), rep(TRUE, n - cut_point))

              TP <- sum(predicciones == TRUE & valores_logicos == TRUE)
              TN <- sum(predicciones == FALSE & valores_logicos == FALSE)
              FP <- sum(predicciones == TRUE & valores_logicos == FALSE)
              FN <- sum(predicciones == FALSE & valores_logicos == TRUE)

              TPR <- TP / (TP + FN)
              FPR <- FP / (FP + TN)

              return(c(TPR, FPR))
            })

            results_matrix <- t(results)
            colnames(results_matrix) <- c("TPR", "FPR")
            return(results_matrix)
          })




#varianza
setGeneric(name="variance", def=function(obj) standardGeneric("variance"))
#' Calculate Variance of Numeric Attributes
#'
#' This method calculates the variance of the numeric values stored in an
#' object of class \code{\linkS4class{AtributoDataset}}. If the attribute
#' is not numeric or integer, it will throw an error
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}}.
#'
#' @return A numeric value representing the variance of the attribute's values.
#'
#' @examples
#' # Creating a numeric attribute
#' numeric_attr <- atributoDataset("Numeric Attribute", c(1, 2, 3, 4, 5))
#' variance(numeric_attr)
#'
setMethod(f="variance", signature="AtributoDataset",
          definition=function(obj) {
            if (obj@tipo != "numeric" && obj@tipo != "integer") {
              stop("El atributo no es numérico o entero, no se puede calcular la varianza")
            }
            return(variance_base(obj@valores))

          })

#entropy
setGeneric(name="entropy", def=function(obj) standardGeneric("entropy"))

#' Calculate Entropy for AtributoDataset Objects
#'
#' This method calculates the entropy of the values contained in an
#' object of class \code{\linkS4class{AtributoDataset}}. Entropy is a measure
#' of the uncertainty or randomness in the data.
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}}
#' containing the values (logical or factor) for which the entropy is to be calculated.
#' If values are numeric, it will throw an error
#'
#' @return A numeric value representing the entropy of the values in the
#' specified \code{\linkS4class{AtributoDataset}} object.
#'
#' @examples
#' # Create an instance of AtributoDataset
#' atributo <- atributoDataset("example", factor(c("blue","green","blue")))
#'
#' entropy(atributo)
setMethod(f="entropy", signature="AtributoDataset",
          definition=function(obj) {
            entropia <- entropy_base(obj@valores)
            return(entropia)
          })


#calculate metrics
setGeneric(name="calculateMetrics", def=function(obj) standardGeneric("calculateMetrics"))

#' Calculate Metrics for Dataset Objects
#'
#' This method calculates various metrics for each attribute in an object of
#' class \code{\linkS4class{Dataset}}. The metrics calculated include variance,
#' area under the curve (AUC), and entropy, depending on the type of each
#' attribute.
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} containing
#' a list of attributes for which the metrics have to be calculated.
#'
#' @return A list of named vectors, where each vector contains the calculated
#' metrics (Variance, AUC, and Entropy) for each attribute in the
#' \code{\linkS4class{Dataset}} object.
#'
#' @details
#' - For numeric and integer attributes, the variance and AUC are calculated.
#' - For factor and logical attributes, entropy is calculated.
#'
#' Note: The AUC is calculated based on the trapezoidal rule.
#'
#' @examples
#' numeric_attr <- atributoDataset("Numeric Attribute", c(1, 2, 3, 4, 5))
#' logical_attr <- atributoDataset("Logical Attribute", c(TRUE, FALSE, TRUE, FALSE, TRUE))
#' dataset <- createDataset("My Dataset", list(numeric_attr, logical_attr))
#' calculateMetrics(dataset)
#'
setMethod(f="calculateMetrics", signature="Dataset",
          definition=function(obj) {

            calcAUC <- TRUE
            if(!any(sapply(obj@atributos, function(attr) attr@tipo == "logical"))){
              warning("No existe ningun atributo lógico con el clasificar el AUC")
              calcAUC <- FALSE
            }
            resultados <- lapply(obj@atributos,FUN = function(atr){
              resultado <- c(Variance = NA, AUC = NA, Entropy = NA)

              if (atr@tipo == "numeric" || atr@tipo == "integer") {
                #varianza
                resultado["Variance"] <- variance(atr)
                if(calcAUC){
                  rates <- calculateRates(obj,atr@nombre)
                  TPR <- rates[, "TPR"]
                  FPR <- rates[, "FPR"]
                  sorted_indices <- order(FPR)
                  FPR <- FPR[sorted_indices]
                  TPR <- TPR[sorted_indices]
                  # Regla del trapecio
                  auc <- sum(diff(FPR) * (TPR[-length(TPR)] + TPR[-1]) / 2)
                  resultado["AUC"] <- auc
                }
              }
              else if(atr@tipo == "factor" || atr@tipo == "logical"){
                resultado["Entropy"] <- entropy(atr)
              }
              names(resultado) <- c("Variance", "AUC", "Entropy")
              return(resultado)
            })
            return(setNames(resultados, sapply(obj@atributos, function(atr) atr@nombre)))
          })



# ejercicio3 ---------------------------------------------------------------

#normalize
setGeneric(name="normalize", def=function(obj, cut.points) standardGeneric("normalize"))

#' Normalize Values for AtributoDataset Objects
#'
#' This method normalizes the values contained in an object of class
#' \code{\linkS4class{AtributoDataset}}. Normalization is performed only
#' for numeric or integer attributes.
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}}
#' containing the values to be normalized.
#'
#' @return An object of class \code{\linkS4class{AtributoDataset}} with the
#' normalized values and a modified name indicating normalization.
#'
#' @details
#' If the attribute type is not numeric or integer, an error is thrown.
#'
#' @examples
#' atributo <- atributoDataset(name = "Attribute1", vector = c(1, 2, 3, 4, 5))
#' normalize(atributo)
setMethod(f="normalize", signature="AtributoDataset",
          definition=function(obj) {
            if (obj@tipo != "numeric" && obj@tipo != "integer") {
              stop("El atributo no es numérico o entero, no se puede normalizar")
            }
            normalized_values <- normalize_base(obj@valores)

            return(atributoDataset(paste(obj@nombre,"- Normalized"), normalized_values))
          })

#' Normalize Attributes for Dataset Objects
#'
#' This method normalizes the numeric and integer attributes contained in an
#' object of class \code{\linkS4class{Dataset}}
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} containing a list
#' of attributes to be normalized.
#'
#' @return An object of class \code{\linkS4class{Dataset}} with the normalized
#' numeric and integer attributes, while other attributes remain the same
#'
#' @examples
#' dataset <- createDataset(nombre = "Example", atributos = list(
#'   atributoDataset(name = "Years", vector = c(1, 2, 3)),
#'   atributoDataset(name = "FactorAttr", vector = factor(c("A", "B", "A")))))
#' normalize(dataset)
setMethod(f="normalize", signature="Dataset",
          definition=function(obj) {
            normalized_atributos <- lapply(obj@atributos, FUN = function(atributo) {
              if (atributo@tipo == "numeric" || atributo@tipo == "integer") {
                return(normalize(atributo))  # Llamar a la implementación de normalize en AtributoDataset
              }
              return(atributo)
            })

            return(createDataset(nombre = obj@nombre, atributos = normalized_atributos))
          })


#standarize
setGeneric(name="standarize", def=function(obj) standardGeneric("standarize"))

#' Standardize Values for AtributoDataset Objects
#'
#' This method standardizes the values in an object of class
#' \code{\linkS4class{AtributoDataset}}. It can only be applied to numeric
#' or integer attributes.
#'
#' @param obj An object of class \code{\linkS4class{AtributoDataset}}
#' containing values to standardize.
#'
#' @return An object of class \code{\linkS4class{AtributoDataset}} with
#' standardized values.
#'
#' @details If the attribute type is not numeric or integer, an error is thrown
#'
#' @examples
#' atributo <- atributoDataset(name = "Attribute1", vector = c(1, 2, 3, 4, 5))
#' standarize(atributo)
setMethod(f="standarize", signature="AtributoDataset",
          definition=function(obj) {
            if (obj@tipo != "numeric" && obj@tipo != "integer") {
              stop("El atributo no es numérico o entero, no se puede estandarizar")
            }
            standarized_values <- standarize_base(obj@valores)

            return(atributoDataset(paste(obj@nombre,"- Standarized"), standarized_values))
          })


#' Standardize Attributes for Dataset Objects
#'
#' This method standardizes the numeric and integer attributes of an object
#' of class \code{\linkS4class{Dataset}}
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} containing a list
#' of attributes to be standardized.
#'
#' @return An object of class \code{\linkS4class{Dataset}} with standardized
#' numeric and integer attributes, while other attributes remain the same
#'
#' @details The method iterates over each attribute in the dataset and calls
#' the \code{\link{standarize}} method from the \code{\linkS4class{AtributoDataset}}
#' class for numeric and integer attributes.
#'
#' @examples
#' dataset <- createDataset(nombre = "Example", atributos = list(
#'   atributoDataset("Months", c(1, 2, 3)),
#'   atributoDataset("FactorAttr", factor(c("A", "B", "A")))))
#' standarize(dataset)
setMethod(f="standarize", signature="Dataset",
          definition=function(obj) {
            standarized_atributos <- lapply(obj@atributos, FUN = function(atributo) {
              if (atributo@tipo == "numeric" || atributo@tipo == "integer") {
                return(standarize(atributo))  # Llamar a la implementación de standarize en AtributoDataset
              }
              return(atributo)
            })

            return(createDataset(nombre = obj@nombre, atributos = standarized_atributos))
          })


# ejercicio4 --------------------------------------------------------------
setGeneric(name="filterMetrics", def=function(obj, metric_name, operator ,value) standardGeneric("filterMetrics"))

#' Filter the attributes of a \code{\linkS4class{Dataset}} object based on specific metrics.
#'
#' This method allows filtering the attributes of a \code{Dataset} object according to the value
#' of a specified metric using a given operator. Attributes that do not meet the specified
#' condition will be removed from the object.
#'
#' @param obj An object of class \code{\linkS4class{Dataset}} containing the attributes to be filtered.
#' @param metric_name A \code{character} string specifying the name of the metric to be used for filtering.
#' Only 'Variance' 'AUC' and 'Entropy' are allowed
#' @param operator A \code{character} string representing the comparison operator to be used.
#'        Must be one of: \code{">"}, \code{"<"}, \code{">="}, \code{"<="}, \code{"=="}, \code{"!="}.
#' @param value A numeric value to compare the attribute metrics against.
#'
#' @return An object of class \code{\linkS4class{Dataset}} containing only the attributes that passed the filter.
#'
#' @examples
#' dataset <- createDataset(nombre = "Example", atributos = list(
#'   atributoDataset("Months", c(1, 2, 3)),
#'   atributoDataset("FactorAttr", factor(c("A", "B", "A")))))
#' filtered_dataset <- filterMetrics(dataset, metric_name = "AUC", operator = ">", value = 0.5)
setMethod(f = "filterMetrics", signature = "Dataset",
          definition = function(obj, metric_name, operator, value) {
            metrics <- calculateMetrics(obj)
            filtered_metrics <- sapply(metrics, FUN = function(variable) {
              if (metric_name %in% names(variable)) {
                metric_value <- variable[[metric_name]]
                if(!is.na(metric_value)){
                  switch(operator,
                         ">" = return(metric_value > value),
                         "<" = return(metric_value < value),
                         ">=" = return(metric_value >= value),
                         "<=" = return(metric_value <= value),
                         "==" = return(metric_value == value),
                         "!=" = return(metric_value != value),
                         stop("Operador no válido")
                    )
                } else{
                    return(FALSE)
                }

              } else{
                stop("metric_name: ", metric_name," no válido")
              }
            })
            atributosValidos <- obj@atributos[filtered_metrics]
            obj@atributos <- atributosValidos
            return(obj)
  })



# ejercicio5 --------------------------------------------------------------
setGeneric(name="correlation", def=function(obj) standardGeneric("correlation"))

#' Calculate Correlation and Mutual Information for a \code{\linkS4class{Dataset}}
#'
#' This method calculates the correlation for numeric attributes and mutual information for categorical attributes
#' within a \code{\linkS4class{Dataset}} object. It returns two matrices: one for correlations and another for mutual information.
#'
#' @param obj A \code{\linkS4class{Dataset}} object containing attributes to be analyzed.
#'
#' @return A list containing two matrices:
#' \itemize{
#'   \item \code{correlations}: A symmetric matrix of correlations for numeric attributes
#'   \item \code{mutual_information}: A symmetric matrix of mutual information values for categorical attributes
#' }
#' @description This method iterates trough all the atributes of a dataset and calculates the correlation (or mutual Information) for each pair.
#' For those calculations uses the 'correlation' internal function.
#'
#' @examples
#' dataset <- createDataset(nombre = "Example", atributos = list(
#'   atributoDataset("Numeric1", c(1, 2, 3)),
#'   atributoDataset("Numeric2", c(4, 4, 6))))
#' result <- correlation(dataset)
#' correlation_matrix <- result$correlations
#' mutual_information_matrix <- result$mutual_information
#'
setMethod(f="correlation", signature="Dataset",
          definition=function(obj) {
            nombres <- sapply(obj@atributos, function(attr) attr@nombre)
            valores <- lapply(obj@atributos, FUN = function(atributo1) {
              lapply(obj@atributos, FUN = function(atributo2) {

              if ((atributo1@tipo == "numeric" || atributo1@tipo == "integer") &&
                  (atributo2@tipo == "numeric" || atributo2@tipo == "integer")) {
                return(correlation_base(atributo1@valores, atributo2@valores))

                }else if((atributo1@tipo == "factor" || atributo1@tipo == "logical") &&
                  (atributo2@tipo == "factor" || atributo2@tipo == "logical")){
                return(mutualInformation_base(atributo1@valores, atributo2@valores))

                  }else{
                return(NA)
              }
            })
          })
          # Cogemos todos los resultados y los metemos en una matriz
          result <- matrix(unlist(valores),
                                   nrow = length(obj@atributos),
                                   ncol = length(obj@atributos),
                                   byrow = TRUE)
          rownames(result) <- colnames(result) <- nombres

          # Separar la matriz en correlaciones e información mutua
          numeric_indices <- which(sapply(obj@atributos, function(attr) attr@tipo == "numeric" || attr@tipo == "integer"))
          categorical_indices <- which(sapply(obj@atributos, function(attr) attr@tipo == "factor" || attr@tipo == "logical"))

          correl_matrix <- result[numeric_indices, numeric_indices, drop = FALSE]
          infomutua_matrix <- result[categorical_indices, categorical_indices, drop = FALSE]

          return(list(correlations = correl_matrix, mutual_information = infomutua_matrix))

})

