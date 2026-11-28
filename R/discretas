# ==============================================================
# Generación de variables aleatorias discretas
# Método de la Transformación Inversa Discreta (Ross, cap. 4)
# Responsable: Erika
#
# Usa crear_fuente_uniforme() de R/generadores_continuas.R
# ==============================================================


# --------------------------------------------------------------
# Funciones de masa de probabilidad (calculadas con recursiones)
# --------------------------------------------------------------

# Binomial(n, p):  p(0) = (1 − p)^n,   p(k+1) = (n − k)/(k + 1) · p/(1 − p) · p(k)
fmp_binomial <- function(n, p) {
  if (n < 1 || n != round(n)) stop("n debe ser un entero mayor o igual a 1.")
  if (p <= 0 || p >= 1) stop("p debe estar entre 0 y 1 (sin incluirlos).")
  prob <- numeric(n + 1)
  prob[1] <- (1 - p)^n
  for (k in 0:(n - 1)) prob[k + 2] <- (n - k) / (k + 1) * p / (1 - p) * prob[k + 1]
  list(valores = 0:n, probs = prob)
}

# Poisson(λ):  p(0) = e^(−λ),   p(k+1) = λ/(k + 1) · p(k)
# (se corta cuando la probabilidad acumulada llega a 1 − 10^-12)
fmp_poisson <- function(lambda) {
  if (lambda <= 0) stop("λ debe ser mayor que 0.")
  if (lambda > 500) stop("Use λ ≤ 500.")
  prob <- exp(-lambda); k <- 0
  while (sum(prob) < 1 - 1e-12 && k < 2000) {
    prob <- c(prob, lambda / (k + 1) * prob[k + 1])
    k <- k + 1
  }
  list(valores = 0:k, probs = prob)
}

# Geométrica(p): número de ensayos hasta el primer éxito
#   p(k) = p · (1 − p)^(k − 1),  k = 1, 2, ...
fmp_geometrica <- function(p) {
  if (p <= 0 || p >= 1) stop("p debe estar entre 0 y 1 (sin incluirlos).")
  prob <- p; k <- 1
  while (sum(prob) < 1 - 1e-12 && k < 5000) {
    prob <- c(prob, prob[k] * (1 - p))
    k <- k + 1
  }
  list(valores = 1:k, probs = prob)
}

# Convierte "0.1, 0.2, 1/4" en números (acepta fracciones simples)
leer_lista_numeros <- function(texto, nombre) {
  partes <- trimws(strsplit(texto, "[,;]")[[1]])
  partes <- partes[partes != ""]
  if (length(partes) == 0) stop(paste("Ingrese al menos un valor en", nombre, "."))
  numeros <- numeric(length(partes))
  for (i in seq_along(partes)) {
    t <- partes[i]
    if (!grepl("^-?[0-9]*\\.?[0-9]+(/[0-9]*\\.?[0-9]+)?$", t))
      stop(paste0("En ", nombre, ", \"", t, "\" no es un número válido. Use decimales con punto o fracciones como 1/6."))
    if (grepl("/", t)) {
      ab <- as.numeric(strsplit(t, "/")[[1]])
      numeros[i] <- ab[1] / ab[2]
    } else {
      numeros[i] <- as.numeric(t)
    }
  }
  numeros
}

# Distribución personalizada: valida que sea una ley de probabilidad
fmp_personalizada <- function(texto_valores, texto_probs) {
  x <- leer_lista_numeros(texto_valores, "los valores")
  p <- leer_lista_numeros(texto_probs, "las probabilidades")
  if (length(x) != length(p))
    stop(paste0("Hay ", length(x), " valores y ", length(p), " probabilidades: deben ser la misma cantidad."))
  if (any(duplicated(x))) stop("Hay valores repetidos en X.")
  if (any(p < 0)) stop("Las probabilidades no pueden ser negativas.")
  if (abs(sum(p) - 1) > 1e-6)
    stop(paste0("Las probabilidades suman ", round(sum(p), 6), ", pero deben sumar exactamente 1 (Σpᵢ = 1)."))
  orden <- order(x)
  list(valores = x[orden], probs = p[orden])
}


# --------------------------------------------------------------
# Transformación Inversa Discreta
#   Se genera U y se busca el primer j tal que U < F(x_j);
#   entonces X = x_j.  La cantidad de comparaciones es j.
#   Si ordenar = TRUE se revisan primero los valores más probables
#   (mejora de Ross: menos comparaciones en promedio).
# --------------------------------------------------------------
transformada_inversa_discreta <- function(valores, probs, N, fuente, ordenar = FALSE) {
  if (ordenar) {
    o <- order(probs, decreasing = TRUE)
    valores <- valores[o]; probs <- probs[o]
  }
  Facum <- cumsum(probs)
  Facum[length(Facum)] <- 1                       # evita problemas de redondeo al final

  u <- fuente(N)
  x <- numeric(N)
  comparaciones <- numeric(N)
  for (i in 1:N) {
    j <- 1
    while (u[i] >= Facum[j]) j <- j + 1        # búsqueda secuencial
    x[i] <- valores[j]
    comparaciones[i] <- j
  }
  list(x = x, u = u, comparaciones = comparaciones,
       orden_busqueda = valores, F_busqueda = Facum,
       tabla = data.frame(i = 1:N, u = u, comparaciones = comparaciones, x = x))
}


# --------------------------------------------------------------
# Prueba Chi-cuadrado de bondad de ajuste para discretas.
# Se agrupan valores vecinos hasta que cada grupo tenga E ≥ 5.
# --------------------------------------------------------------
chi_cuadrado_discreta <- function(x, valores, probs, alfa = 0.05) {
  N <- length(x)
  obs <- numeric(length(valores))
  for (j in seq_along(valores)) obs[j] <- sum(x == valores[j])
  esp <- N * probs

  grupos_o <- c(); grupos_e <- c(); etiquetas <- c()
  acum_o <- 0; acum_e <- 0; inicio <- 1
  for (j in seq_along(valores)) {
    acum_o <- acum_o + obs[j]; acum_e <- acum_e + esp[j]
    if (acum_e >= 5 || j == length(valores)) {
      if (acum_e < 5 && length(grupos_e) > 0) {          # el último grupo se une al anterior
        k <- length(grupos_e)
        grupos_o[k] <- grupos_o[k] + acum_o; grupos_e[k] <- grupos_e[k] + acum_e
        etiquetas[k] <- paste0(strsplit(etiquetas[k], " – ")[[1]][1], " – ", valores[j])
      } else {
        grupos_o <- c(grupos_o, acum_o); grupos_e <- c(grupos_e, acum_e)
        etiquetas <- c(etiquetas, if (inicio == j) as.character(valores[j]) else paste0(valores[inicio], " – ", valores[j]))
      }
      acum_o <- 0; acum_e <- 0; inicio <- j + 1
    }
  }
  k <- length(grupos_e)
  if (k < 2) return(NULL)
  chi2 <- sum((grupos_o - grupos_e)^2 / grupos_e)
  gl <- k - 1
  list(tabla = data.frame(Valores = etiquetas, Observada = as.integer(grupos_o), Esperada = round(grupos_e, 2),
                          Aporte = round((grupos_o - grupos_e)^2 / grupos_e, 4)),
       chi2 = chi2, gl = gl, critico = qchisq(1 - alfa, gl), p_valor = 1 - pchisq(chi2, gl),
       rechaza = chi2 > qchisq(1 - alfa, gl))
}

