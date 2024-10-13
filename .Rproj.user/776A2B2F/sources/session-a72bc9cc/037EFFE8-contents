#Funciones basicas, que seran llamadas desde dataset.r
#no estan documentadas de la misma manera ya que no apareceran en la ayuda

divideVectorByBreaks_base <- function(x, breaks, printWarning=FALSE){
  result_vector <- sapply(x, function(val) {
    if(val <= breaks[1]){
      paste0("(-Inf, ", breaks[1], "]")
    } else if(val >= breaks[length(breaks)]){
      paste0("[", breaks[length(breaks)], ", Inf)")
    } else{
      #caso general, no esta en los bordes. Mirar el break más cercano sin pasarse
      valid_breaks <- breaks[breaks <= val]
      closest_index <- which.max(valid_breaks)
      paste0("(", breaks[closest_index], ", ", breaks[closest_index + 1], ")")
    }
  })

  # Crear los levels por si no se usan todos en el vector
  levels <- c(
    paste0("(-Inf, ", breaks[1], "]"),
    if (length(breaks) > 1) {
      paste0("(", breaks[-length(breaks)], ", ", breaks[-1], ")")
    },
    paste0("[", breaks[length(breaks)], ", Inf)")
  )

  result <- list()
  result$vector <- factor(result_vector)
  result$cut.points <- breaks
  cantidades <- table(result_vector)
  if(max(cantidades)-min(cantidades)>=2 && printWarning==TRUE){
    warning("No se ha podido dividir exactamente en intervalos de igual frecuencia debido a valores duplicados en el vector")
  }
  return(result)
}

#discretizar igual anchura
discretizeEW_base <- function (x, num.bins) {
  isNumeric <- class(x)=="numeric" | class(x) == "integer"
  if(!isNumeric){
    stop("Error. Input vector must be numeric!")
  } else {
    data <- data.frame(original = x, index = seq_along(x))
    x <- data$original
    #calcular anchura de cada intervalo
    intervalWidth = (max(x)-min(x))/num.bins
    breaks <- round(seq(min(x)+intervalWidth, max(x)-intervalWidth, by=intervalWidth),digits=3)

    result <- divideVectorByBreaks_base(x,breaks)
    data$result <- result$vector
    #devolverlo en el orden original
    data <- data[order(data$index), ]
    result$vector <- data$result
    return(result)
  }
}

#discretizar por igual frecuencia
discretizeEF_base <- function(x, num.bins){
  formatoCorrecto <- class(x)=="numeric" | class(x) == "integer"
  if(!formatoCorrecto){
    stop("Error. Input vector must be numeric!")
  } else {
    data <- data.frame(original = x, index = seq_along(x))
    x <- sort(x)
    n <- length(x)
    #calcular cuantos elementos por intervalo
    intervalSize <- ceiling(n / num.bins)#redondeado
    # Indices para los breaks
    cut_indices <- seq(intervalSize, n, by = intervalSize)
    # Obtener los puntos de corte
    breaks <- sort(x[cut_indices])
    if (length(breaks) > num.bins - 1) {
      breaks <- breaks[1:(num.bins - 1)]
    }
    result <- divideVectorByBreaks_base(data$original,breaks, printWarning=TRUE)
    data$result <- result$vector
    #devolverlo en el orden original
    data <- data[order(data$index), ]
    result$vector <- data$result
    return(result)
  }
}


#entropia de un vector
entropy_base <- function(x){
  formatoCorrecto <- is.factor(x) || is.logical(x)
  if(!formatoCorrecto){
    stop("Error. El vector debe ser de tipo factor o logico!")
  } else {
    #contar cuantas veces aparece cada level
    ocurrencias <- table(x)
    probabilidades <- ocurrencias / length(x)
    probabilidades <- probabilidades[probabilidades>0]
    resultado <- sum(-probabilidades * log2(probabilidades))
    return (round(resultado, digits=3))
  }
}

#normalizar vector
normalize_base <- function(v){
  return ((v-min(v))/(max(v)-min(v)))
}

#standarizar vector
standarize_base <- function(v){
  return ((v - mean(v))/ sd(v))
}

#varianza de un vector
variance_base <- function(v){
  return(sum((v - mean(v))^2) / (length(v) - 1))
}


#calculo de tpr y fpr
calculateRates_base <- function(df) {
  if (is.numeric(df[,1]) && is.logical(df[,2])) {
    df <- df[order(df[,1]),]
    n <- nrow(df)
    df$prediction <- rep(FALSE, nrow(df))
    #para cada fila del df
    results <- sapply(1:n, function(cut.point) {
      #predecir todos los valores en false hasta el punto de corte y luego true
      df$prediction <- c(rep(FALSE, cut.point), rep(TRUE, n - cut.point))

      TP <- sum(df$prediction == TRUE & df[,2] == TRUE)
      TN <- sum(df$prediction == FALSE & df[,2] == FALSE)
      FP <- sum(df$prediction == TRUE & df[,2] == FALSE)
      FN <- sum(df$prediction == FALSE & df[,2] == TRUE)
      #true positive rate y false positive rate
      TPR <- TP / (TP + FN)
      FPR <- FP / (FP + TN)

      return(c(TPR, FPR))
    }
    )
    return(t(results))
  } else {
    stop("Formato incorrecto: La primera columna debe ser numerica, y la segunda lógica")
  }
}

correlation_base <- function(v1, v2) {
  n <- length(v1)
  v1dev <- v1 - mean(v1)
  v2dev <- v2 - mean(v2)
  #formula de correlacion de pearson
  correlation <- sum(v1dev * v2dev) / sqrt(sum(v1dev^2) * sum(v2dev^2))
  return(correlation)
}


mutualInformation_base <- function(v1,v2){
  joint_entropy <- entropy_base(cbind(v1, v2))  # H(X,Y)
  entropy_v1 <- entropy_base(v1)
  entropy_v2 <- entropy_base(v2)

  # de la formula I(X, Y) = H(X) + H(Y) - H(X,Y)
  return(entropy_v1 + entropy_v2 - joint_entropy)
}

#Extra
#discretizar sabiendo los puntos de corte
discretize_base <- function(x, cut.points){
  formatoCorrecto <- class(x)=="numeric" | class(x) == "integer"
  if(!formatoCorrecto){
    stop("Error. El vector debe ser numerico!")
  } else {
    print(x)
    print(cut.points)
    result <- divideVectorByBreaks_base(x, cut.points)
    return(result)
  }
}

