#' Title pnorm_MC
#'
#' @param x: vetor de quantis da N(0,1) para o qual desejamos os percentis
#' @param n: tamanho da amostra da U(0,1) usada na Integração de Monte Carlo
#'
#' @return vetor de percentis [P(0<Z<x)] da N(0,1) dados os quantis em x
#' @export
#'
#' @examples > set.seed(30)
#'           > pnorm_MC(0)
#'           [1] 0.5
#'           > set.seed(30)
#'           > pnorm_MC(c(0,1.64,1.96,3.99))
#'           [1] 0.5000000 0.9493068 0.9747366 0.9994916
pnorm_MC <- function(x, n = 10^6){
  t <- runif(n)
  fda <- numeric(length(x))
  for(i in 1:length(x)){
    g <- x[i]*exp(-0.5*(x[i]*t)^2)
    fda[i] <- mean(g)/sqrt(2*pi) + 0.5
  }
  return(fda)
}