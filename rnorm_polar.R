#' Title rnorm_polarTrig
#'
#' @return n: Duas vezes o número de vetores da Normal(0,1) que desejamos gerar
#'            (padrão: n = 1)
#' @export
#'
#' @examples > set.seed(30)
#'           > rnorm_polarTrig()
#'           [1]  0.3101949 -0.5247283
rnorm_polarTrig <- function(n = 1) {
  u1 <- runif_congruencial()
  r2 <- -2 * log(u1)
  u2 <- runif_congruencial()
  theta <- 2 * pi * u2
  x <- sqrt(r2) * cos(theta)
  y <- sqrt(r2) * sin(theta)
  resultado <- c(x, y)
  return(resultado)
}

#' Title rnorm_polarDireto 
#'
#' @param n: Duas vezes o número de vetores da Normal(0,1) que desejamos gerar
#'        (padrão: n = 1)
#'
#' @return resultado: vetor de tamanho 2*n da Normal(0,1) usando o Método Polar 
#'                    sem utilizar funções trigonométricas
#' @export
#'
#' @examples > set.seed(30)
#'           > rnorm_polarDireto()
#'           [1] 0.3461371 0.3508665
rnorm_polarDireto <- function(n = 1) {
  resultado <- c()
  i = 1
  while (i <= n) {
    u1 <- runif_congruencial()
    u2 <- runif_congruencial()
    v1 <- 2 * u1 - 1
    v2 <- 2 * u2 - 1
    u <- v1 ^ 2 + v2 ^ 2
    if (u > 1) {
      next()
    }
    i <- i + 1
    x <- sqrt(-2 * log(u) / u) * v1
    y <- sqrt(-2 * log(u) / u) * v2
    resultado <- append(resultado, c(x, y))
  }
  return(resultado)
}