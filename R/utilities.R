

if_null <- function(x = NULL, val_if_null = NULL){
  if (is.null(x)){
    val_if_null
  } else {
    x
  }
}

mass <- function(x, min = NULL, max = NULL){
  if (!all(x %% 1 == 0)){
    stop("x must be integer conformable")
  }
  xin <- na.omit(x)
  N <- length(xin)
  min <- if_null(min, min(xin))
  max <- if_null(max, max(xin))
  x <- seq.int(min, max)
  nx <- length(x)
  yraw <- rep(NA, nx)
  for(i in 1:nx){
    yraw[i] <- length(xin[xin == x[i]])
  }
  y <- yraw / N
  out <- list(x = x, y = y, n = N)
  class(out) <- c("mass", "list")
  out
}


messageq <- function(txt = NULL, quiet = FALSE){
  if (!quiet){
    message(txt)
  }
}