#' Title pnorm_polar
#'
#' @param x vetor de quantis da distribuição
#' @param n tamanho do vetor. Padrão: n = 10^5
#'
#' @return uma lista com a densidade acumulada empírica e a diferença entre a 
#'          dens. acumulada encontrada pelo método e pnorm() do R
#' @export
#'
#' @examples
pnorm_polar <- function(x,n=10^5){
  resultado <- list(length(x))
  fda <- numeric(length(x))
  t <- rnorm_polarDireto(n)
  mean(ifelse(t<x,1,0))
  for(i in 1:length(x)){
    fda[i] <- mean(ifelse(t<x[i],1,0))
    resultado[i] <- paste(format(fda[i],digits = 6),"\newline(",format(fda[i]-pnorm(x),digits=3),")&",sep="")
  }
  return(resultado)
}