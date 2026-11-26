# ==============================================================
# Análisis teórico de los generadores 
# ==============================================================

# --------------------------------------------------------------
# Máximo común divisor (algoritmo de Euclides)
# --------------------------------------------------------------
mcd <- function(a, b) {
  while (b != 0) {
    r <- a %% b
    a <- b
    b <- r
  }
  a
}


# --------------------------------------------------------------
# Factores primos de m
# --------------------------------------------------------------
factores_primos <- function(m) {
  factores <- c()
  p <- 2
  while (p * p <= m) {
    if (m %% p == 0) {
      factores <- c(factores, p)
      while (m %% p == 0) m <- m / p
    }
    p <- p + 1
  }
  if (m > 1) factores <- c(factores, m)
  factores
}


# --------------------------------------------------------------
# Teorema de Hull-Dobell: el MCL tiene periodo completo (= m)
# si y solo si:
#   1. c y m son primos relativos           mcd(c, m) = 1
#   2. a - 1 es divisible por todo primo p que divide a m
#   3. si m es múltiplo de 4, a - 1 también lo es
# --------------------------------------------------------------
hull_dobell <- function(a, c, m) {
  primos <- factores_primos(m)

  cond1 <- mcd(c, m) == 1
  cond2 <- TRUE
  for (p in primos) {
    if ((a - 1) %% p != 0) cond2 <- FALSE
  }
  cond3 <- if (m %% 4 == 0) (a - 1) %% 4 == 0 else TRUE

  data.frame(
    Condicion = c("1. mcd(c, m) = 1",
                  "2. (a − 1) divisible por todos los primos de m",
                  "3. Si 4 | m, entonces 4 | (a − 1)"),
    Detalle   = c(paste0("mcd(", c, ", ", m, ") = ", mcd(c, m)),
                  paste0("Primos de m: ", paste(primos, collapse = ", "),
                         "  |  a − 1 = ", a - 1),
                  if (m %% 4 == 0) paste0("m es múltiplo de 4; (a − 1) mod 4 = ", (a - 1) %% 4)
                  else "m no es múltiplo de 4 (no aplica)"),
    Cumple    = c(cond1, cond2, cond3)
  )
}

# --------------------------------------------------------------
# Prueba Chi-cuadrado de uniformidad con k intervalos iguales
#   H0: los números siguen una distribución U(0, 1)
# --------------------------------------------------------------
chi_cuadrado_uniforme <- function(u, k = 10, alfa = 0.05) {
  N <- length(u)
  observadas <- numeric(k)
  for (j in 1:k) {
    observadas[j] <- sum(u >= (j - 1) / k & u < j / k)
  }
  esperada <- N / k
  chi2     <- sum((observadas - esperada)^2 / esperada)
  critico  <- qchisq(1 - alfa, df = k - 1)
  p_valor  <- 1 - pchisq(chi2, df = k - 1)

  tabla <- data.frame(
    Intervalo  = paste0("[", round((1:k - 1) / k, 2), ", ", round((1:k) / k, 2), ")"),
    Observada  = as.integer(observadas),
    Esperada   = esperada,
    Aporte     = round((observadas - esperada)^2 / esperada, 4)
  )

  list(tabla = tabla, chi2 = chi2, critico = critico, p_valor = p_valor,
       gl = k - 1, rechaza = chi2 > critico)
}

