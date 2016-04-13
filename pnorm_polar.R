pnorm_polar <- function(x,n=10^5){
  t <- rnorm_polarDireto(n)
  sum(as.integer(as.logical(t[t <= pnorm(x)])))/(2*n)
}