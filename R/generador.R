#---------------------------------------------------------------------------------------------------------
# Generadores congruenciales 
#---------------------------------------------------------------------------------------------------------
mcm <- function(a, m, x0, n) {
  if (is.integer(a) & is.integer(m) & a > 0 & m > 0) {
    res <- numeric(n + 1)
    res[1] <- x0
    for (i in 2:(n + 1)) res[i] <- (a * res[i - 1]) %% m
    return(res[-1] / m)
  } else {
    cat("Los valores de a y m no cumplen las condiciones iniciales\n")
  }
}

gcm <- function(a, m, c, x0, n) {
  if (is.integer(a) & is.integer(m) & a > 0 & m > 0) {
    res <- numeric(n + 1)
    res[1] <- x0
    for (i in 2:(n + 1)) res[i] <- (a * res[i - 1] + c) %% m
    return(res[-1] / m)
  } else {
    cat("Los valores de a y m no cumplen las condiciones iniciales\n")
  }
}

#---------------------------------------------------------------------------------------------------------
# Transformada inversa discreta (mti) 
#---------------------------------------------------------------------------------------------------------
mti <- function(probs, labels) {
  U <- runif(1)
  pos <- which(U < cumsum(probs))[1]
  return(labels[pos])
}

#---------------------------------------------------------------------------------------------------------
# Poisson 
#---------------------------------------------------------------------------------------------------------
dp <- function(lambda) {
  U <- runif(1)
  i <- 0
  p <- exp(-lambda)
  Fx <- p
  while (U >= Fx) {
    p <- (lambda * p) / (i + 1)
    Fx <- Fx + p
    i <- i + 1
  }
  return(i)
}

#---------------------------------------------------------------------------------------------------------
# Binomial
#---------------------------------------------------------------------------------------------------------
db <- function(n, p) {
  U <- runif(1)
  c_val <- p / (1 - p)
  i <- 0
  pr <- (1 - p)^n
  Fx <- pr
  while (U >= Fx) {
    pr <- (c_val * (n - i) / (i + 1)) * pr
    Fx <- Fx + pr
    i <- i + 1
  }
  return(i)
}
  
#---------------------------------------------------------------------------------------------------------
# Geométrica
#---------------------------------------------------------------------------------------------------------
dg <- function(q) {
  return(floor(log(runif(1)) / log(q)) + 1)
}


#---------------------------------------------------------------------------------------------------------
# Prueba de rachas
#---------------------------------------------------------------------------------------------------------
runs_test <- function(x) {
  n <- length(x)
  mediana <- median(x)
  binario <- as.integer(x > mediana)
  cambios <- sum(binario[-1] != binario[-n])
  R <- cambios + 1
  n1 <- sum(binario == 1)
  n2 <- sum(binario == 0)
  mu <- 2 * n1 * n2 / (n1 + n2) + 1
  sigma2 <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) /
            ((n1 + n2)^2 * (n1 + n2 - 1))
  z <- (R - mu) / sqrt(sigma2)
  p_valor <- 2 * (1 - pnorm(abs(z)))
  return(list(R = R, Z = z, p_valor = p_valor))
}
