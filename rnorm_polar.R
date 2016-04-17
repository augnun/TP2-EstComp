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
  resultado <- c()
  i <- 1
  while (i <= n) {
    u1 <- runif_congruencial()
    r2 <- -2 * log(u1)
    u2 <- runif_congruencial()
    theta <- 2 * pi * u2
    x <- sqrt(r2) * cos(theta)
    y <- sqrt(r2) * sin(theta)
    resultado <- append(resultado, c(x, y))
    i <- i + 1
  }
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
#'           [1] 0.7975119 0.5323805
rnorm_polarDireto <- function(n=1){
  X <- matrix(0, ncol = 2, nrow = n)
  for (i in 1:n){
    U <- runif(2)
    V <- 2 * U - 1
    R2 <- sum(V^2)
    while (R2 > 1){
      U <- runif(2)
      V <- 2 * U - 1
      R2 <- sum(V^2)
    }
    Y <- sqrt(-2 * log(R2) / R2)
    X[i, ] <- Y * V
  }
  as.vector(X)
}