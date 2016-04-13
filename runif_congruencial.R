#' Title runif_congruencial: gerador congruencial para variável aleatória U(0,1)
#'
#' @param n: tamanho do vetor aleatório da U(0,1) desejado
#'           padrão: 1
#'
#' @return x: vetor aleatório iid da U(0,1) de tamanho n
#' @export
#'
#' @examples> set.seed(2016)
#'            > runif_congruencial(1)
#'            [1] 0.7043008
#'
#'            > set.seed(2016)
#'            > runif_congruencial(10)
#'            [1] 0.7043008 0.6616736 0.1355620 0.4696484 0.6340929 0.2228151 0.4471911 0.4617350
#'            [9] 0.4915651 0.3476999
runif_congruencial <- function(n = 1) {
  a <- 62089911
  m <- (2 ^ 31) - 1
  x <- c(0)
  y <- c(0)
  x[1] <- 0
  y[1] <- sample(.Random.seed[.Random.seed > 0], 1)
  if (n == 1) {
    y_0 <- y[1]
    y[1] <- (a * y_0) %% m
    x[1] = y[1] / m
  }
  else{
    y_0 <- y[1]
    y[1] <- (a * y_0) %% m
    x[1] = y[1] / m
    for (i in 2:n) {
      y[i] <- (a * y[i - 1]) %% m
      x[i] <- y[i] / m
    }
  }
  return(x)
}