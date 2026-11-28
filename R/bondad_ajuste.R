# ==============================================================
# Pruebas de bondad de ajuste e independencia
# Responsable: Erika
# Los estadísticos se calculan "a mano" con funciones básicas.
# ==============================================================


# --------------------------------------------------------------
# Prueba de Kolmogorov-Smirnov
#   H0: los datos siguen la distribución F
#   D+ = máx( i/n − F(x_(i)) ),  D− = máx( F(x_(i)) − (i−1)/n ),  D = máx(D+, D−)
#   Valor p con la distribución asintótica de Kolmogorov
#   (corrección de Stephens: λ = (√n + 0.12 + 0.11/√n) · D)
# --------------------------------------------------------------
prueba_ks <- function(x, cdf, alfa = 0.05) {
  n  <- length(x)
  xs <- sort(x)
  Fx <- cdf(xs)
  i  <- 1:n
  d_mas   <- max(i / n - Fx)
  d_menos <- max(Fx - (i - 1) / n)
  D <- max(d_mas, d_menos)
  j <- which.max(pmax(i / n - Fx, Fx - (i - 1) / n))

  ajuste <- sqrt(n) + 0.12 + 0.11 / sqrt(n)
  lambda <- ajuste * D
  k <- 1:100
  p_valor <- 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * lambda^2))
  p_valor <- min(max(p_valor, 0), 1)
  critico <- sqrt(-0.5 * log(alfa / 2)) / ajuste

  list(D = D, d_mas = d_mas, d_menos = d_menos, critico = critico, p_valor = p_valor,
       rechaza = D > critico, x_max = xs[j], xs = xs, Fn = i / n, Fx = Fx)
}


# --------------------------------------------------------------
# Prueba de rachas por encima y por debajo de la mediana
#   (Wald-Wolfowitz). H0: la secuencia es aleatoria (independiente)
#   E[R] = 2 n1 n2 / (n1 + n2) + 1
#   V[R] = 2 n1 n2 (2 n1 n2 − n1 − n2) / ((n1 + n2)² (n1 + n2 − 1))
# --------------------------------------------------------------
prueba_rachas_mediana <- function(x, alfa = 0.05) {
  mediana <- median(x)
  s <- x[x != mediana] > mediana          # TRUE = arriba, FALSE = abajo (se excluyen empates)
  n1 <- sum(s); n2 <- sum(!s)
  if (n1 < 2 || n2 < 2) stop("No hay suficientes valores arriba y abajo de la mediana para la prueba de rachas.")
  R  <- 1 + sum(s[-1] != s[-length(s)])
  media_R <- 2 * n1 * n2 / (n1 + n2) + 1
  var_R   <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / ((n1 + n2)^2 * (n1 + n2 - 1))
  Z <- (R - media_R) / sqrt(var_R)
  # longitudes de cada racha
  cortes <- c(which(s[-1] != s[-length(s)]), length(s))
  longitudes <- diff(c(0, cortes))
  list(R = R, n1 = n1, n2 = n2, esperado = media_R, varianza = var_R, Z = Z,
       critico = qnorm(1 - alfa / 2), p_valor = 2 * (1 - pnorm(abs(Z))),
       rechaza = abs(Z) > qnorm(1 - alfa / 2), mediana = mediana, longitudes = longitudes)
}


# --------------------------------------------------------------
# Prueba de rachas ascendentes y descendentes
#   Se cuenta cuántas veces cambia el sentido (sube / baja) la secuencia.
#   E[R] = (2n − 1) / 3,   V[R] = (16n − 29) / 90
# --------------------------------------------------------------
prueba_rachas_ascendentes <- function(x, alfa = 0.05) {
  n <- length(x)
  sentido <- sign(diff(x))
  sentido <- sentido[sentido != 0]
  if (length(sentido) < 2) stop("La secuencia es constante: no se puede aplicar la prueba de rachas.")
  R <- 1 + sum(sentido[-1] != sentido[-length(sentido)])
  media_R <- (2 * n - 1) / 3
  var_R   <- (16 * n - 29) / 90
  Z <- (R - media_R) / sqrt(var_R)
  list(R = R, esperado = media_R, varianza = var_R, Z = Z,
       critico = qnorm(1 - alfa / 2), p_valor = 2 * (1 - pnorm(abs(Z))),
       rechaza = abs(Z) > qnorm(1 - alfa / 2))
}


# --------------------------------------------------------------
# Autocorrelación muestral r_k para k = 1, ..., max_lag
# --------------------------------------------------------------
autocorrelacion <- function(x, max_lag = 20) {
  n <- length(x); m <- mean(x)
  den <- sum((x - m)^2)
  r <- numeric(max_lag)
  for (k in 1:max_lag) r[k] <- sum((x[1:(n - k)] - m) * (x[(1 + k):n] - m)) / den
  data.frame(lag = 1:max_lag, r = r)
}


# --------------------------------------------------------------
# Lectura de archivos subidos por el usuario (.csv o .xlsx)
# Devuelve un data.frame solo con las columnas numéricas.
# --------------------------------------------------------------
leer_archivo_datos <- function(ruta, nombre) {
  ext <- tolower(tools::file_ext(nombre))
  if (ext == "csv" || ext == "txt") {
    # CSV en formato latino (separador ";" y decimales con coma):
    # se reconoce si aparece ";" o si el encabezado no tiene comas pero los datos sí
    lineas <- readLines(ruta, n = 6, warn = FALSE)
    latino <- any(grepl(";", lineas)) ||
              (!grepl(",", lineas[1]) && any(grepl(",", lineas[-1])))
    if (latino) {
      datos <- read.csv2(ruta, stringsAsFactors = FALSE)
    } else {
      datos <- read.csv(ruta, stringsAsFactors = FALSE)
    }
  } else if (ext %in% c("xlsx", "xls")) {
    datos <- as.data.frame(readxl::read_excel(ruta))
  } else {
    stop("Formato no admitido. Suba un archivo .csv o .xlsx.")
  }
  numericas <- sapply(datos, is.numeric)
  if (!any(numericas)) stop("El archivo no tiene columnas numéricas.")
  datos[, numericas, drop = FALSE]
}
