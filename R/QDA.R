#' DiscriminanteCuadratico
#'
#' @param x (matriz) con un marco de datos que contiene las variables predictoras
#' @param grupo (vector) indica a que grupo pertenece cada observacion
#'
#' @return una lista con los elementos media, covarianza, prior y log.prior.
#' @export
#'
#' @examples
#' \dontrun{
#' # Crear un marco de datos con variables predictoras
#' x <- data.frame(
#'  x1 = c(1, 2, 3, 4, 5, 6),
#'  x2 = c(6, 5, 4, 3, 2, 1),
#'  x3 = c(1, 2, 1, 2, 1, 2)
#'  )
#'
#'  # Crear un vector que indica a qué grupo pertenece cada observación
#'  grupo <- c('A', 'A', 'B', 'B', 'C', 'C')
#'
#'  # Ejecutar la función qda_function con los datos creados
#'  resultado<-qda(x, grupo)
#'
#'  # Ver el resultado
#'  print(resultado)
#'  }
qda <- function(x, grupo) {
  # Calcular las medias por grupo
  mediaq <- by(x, grupo, colMeans)

  # Calcular la matriz de covarianza por grupo
  covarianza <- by(x, grupo, cov)

  # Calcular el número de observaciones por grupo
  n <- table(grupo)

  # Calcular las probabilidades a priori por grupo
  prior <- n / sum(n)

  # Calcular el logaritmo de las probabilidades a priori por grupo
  log.prior <- log(prior)

  # Crear una lista para almacenar los resultados
  result <- list(mediaq = mediaq, covarianza = covarianza, prior = prior, log.prior = log.prior)

  # Asignar la clase 'qda' al resultado
  class(result) <- 'qda'

  # Devolver el resultado
  return(result)
}
