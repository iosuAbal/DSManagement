################################################################################
# Funciones internas del paquete implementado #
# *****************************************************************************#
# Autor: Iosu Abal#
# Fecha: 28/10/2024 #
################################################################################

#Funciones basicas, que seran llamadas desde dataset.r
#no estan documentadas de la misma manera ya que no apareceran en la ayuda.

divideVectorByBreaks_base <- function(x, breaks, intervalosPosibles, printWarning=FALSE) {

  # Usamos sapply para iterar sobre cada valor de x (es mas eficiente)
  discretized <- sapply(x, function(value) {
    closest_i <- -1  # Inicializamos closest_i

    # Encontramos el primer punto de corte donde value < breaks[j]
    interval <- intervalosPosibles[1]  # Por defecto, asignamos el primer intervalo
    for (j in seq_along(breaks)) {
      if (value < breaks[j]) {
        if (j == 1) {
          interval <- intervalosPosibles[1]  # Menor que el primer break
        } else if (value == breaks[j-1]) {
          interval <- intervalosPosibles[j]  # Igual al break anterior
        } else {
          interval <- intervalosPosibles[j]  # Dentro del intervalo
        }
        break
      }
      closest_i <- j
    }

    # Si el valor es mayor que el último punto de corte, asignamos el último intervalo
    if (closest_i == length(breaks)) {
      interval <- intervalosPosibles[length(intervalosPosibles)]
    }

    return(interval)
  })

  # Calculamos las cantidades de cada intervalo
  cantidades <- table(factor(discretized, levels = intervalosPosibles))
  # Verificamos si las frecuencias de los intervalos no son iguales
  frecuencias <- as.numeric(cantidades)
  if ((max(frecuencias) - min(frecuencias)) >= 2 && printWarning) {
    warning("No se ha podido dividir exactamente en intervalos de igual frecuencia debido a valores duplicados en el vector")
  }

  result <- list()
  result$vector <- factor(discretized, levels = intervalosPosibles)  # Aseguramos que se utilicen los levels correctos
  result$cut.points <- breaks

  return(result)
}

#obtener lista de posibles intervalos
obtenerPosiblesIntervalos_base <- function(cut_points) {
  intervalosDiscretizados <- list()

  # Agregar el primer intervalo con -Inf
  intervalosDiscretizados[[1]] <- paste0("(-Inf, ", cut_points[1], ")")

  # Si hay más de un punto de corte, agregamos los intervalos intermedios
  if (length(cut_points) > 1) {
    intermedios <- sapply(1:(length(cut_points) - 1), function(i) {
      paste0("[", cut_points[i], ", ", cut_points[i + 1], ")")
    }, USE.NAMES = FALSE)

    intervalosDiscretizados <- c(intervalosDiscretizados, as.list(intermedios))
  }

  # Agregar el último intervalo con Inf
  intervalosDiscretizados[[length(intervalosDiscretizados) + 1]] <- paste0("[", cut_points[length(cut_points)], ", Inf)")

  return(intervalosDiscretizados)
}





# DISCRETIZAR IGUAL ANCHURA -----------------------------------------------
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

    intervalos <- obtenerPosiblesIntervalos_base(breaks)
    result <- divideVectorByBreaks_base(x,breaks,intervalos)
    data$result <- result$vector
    #devolverlo en el orden original
    data <- data[order(data$index), ]
    result$vector <- data$result
    return(result)
  }
}



# DISCRETIZAR IGUAL FRECUENCIA -----------------------------------------------
discretizeEF_base <- function(x, num_bins) {
  # Comprobaciones iniciales
  n <- length(x)
  if (num_bins > n) {
    stop("Error! The number of bins must be lower than the length of the vector.")
  } else if (num_bins == 1) {
    stop("Error! The number of bins must be greater than 1.")
  }

  # Ordenamos el vector
  original_indices <- order(x)
  x_sorted <- sort(x)

  elementos_por_bin <- n / num_bins
  cut_points <- numeric()  # Para almacenar los puntos de corte

  for (i in seq_len(num_bins - 1)) {
    cut_index <- floor((i) * elementos_por_bin)

    # Solo añadimos el punto de corte si es un índice válido
    if (cut_index < n) {
      if (x_sorted[cut_index + 1] %in% cut_points) {  # Verificamos duplicados
        cut_points <- c(cut_points, x_sorted[cut_index + 1] + 0.01)  # Añadir una constante
      } else {
        cut_points <- c(cut_points, x_sorted[cut_index + 1])  # Añadir el punto de corte
      }
    }
  }

  # Verificamos que tengamos todos los puntos de corte necesarios
  if (length(cut_points) < num_bins - 1) {
    cut_points <- c(cut_points, x_sorted[n])  # Añadir el último valor como punto de corte
  }

  intervalosDiscretizados <- obtenerPosiblesIntervalos_base(cut_points)
  x_discretized <- divideVectorByBreaks_base(x_sorted, cut_points, intervalosDiscretizados, printWarning = TRUE)

  # Volver al orden original: utilizamos el índice original para volver a ordenar
  x_discretized_ordered <- x_discretized$vector[original_indices]

  return(list(vector = x_discretized_ordered, cut.points = cut_points))
}

# ENTROPIA -----------------------------------------------
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

# NORMALIZACION -----------------------------------------------
normalize_base <- function(v){
  return ((v-min(v))/(max(v)-min(v)))
}

# ESTANDARIZACION -----------------------------------------------
standarize_base <- function(v){
  return ((v - mean(v))/ sd(v))
}

# VARIANZA -----------------------------------------------
variance_base <- function(v){
  return(sum((v - mean(v))^2) / (length(v) - 1))
}


# CORRELACION -----------------------------------------------
correlation_base <- function(v1, v2) {
  n <- length(v1)
  v1dev <- v1 - mean(v1)
  v2dev <- v2 - mean(v2)
  #formula de correlacion de pearson
  correlation <- sum(v1dev * v2dev) / sqrt(sum(v1dev^2) * sum(v2dev^2))
  return(correlation)
}

# INFORMACION MUTUA -----------------------------------------------
mutualInformation_base <- function(v1,v2){
  joint_entropy <- entropy_base(cbind(v1, v2))  # H(X,Y)
  entropy_v1 <- entropy_base(v1)
  entropy_v2 <- entropy_base(v2)

  # de la formula I(X, Y) = H(X) + H(Y) - H(X,Y)
  return(entropy_v1 + entropy_v2 - joint_entropy)
}

#Extra
# DISCRETIZAR SABIENDO PUNTOS DE CORTE -----------------------------------------------
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

