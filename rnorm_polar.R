rnorm_polar <- function(){
  u_1 <- runif_congruencial()
  r2 <- -2*log(u_1)
  u_2 <- runif_congruencial()
  theta <- 2*pi*u_2
  x <- sqrt(r2)*cos(theta)
  y <- sqrt(r2)*sin(theta)
  resultado <- c(x,y)
  return(resultado)
}