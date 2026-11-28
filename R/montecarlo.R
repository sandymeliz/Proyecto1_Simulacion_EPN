# ==============================================================
# Integración por el Método de Montecarlo
# Responsable: Erika
#
# Usa crear_fuente_uniforme() y crear_funcion_usuario()
# de R/generadores_continuas.R (uniformes del MCL propio).
# ==============================================================


# --------------------------------------------------------------
# Método 1: Media muestral
#   I = ∫ f(x) dx ≈ (b − a) · promedio de f(X_i),  X_i ~ U(a, b)
# --------------------------------------------------------------
mc_media_muestral <- function(f, a, b, N, fuente) {
  u  <- fuente(N)
  x  <- a + (b - a) * u
  fx <- f(x)
  z  <- (b - a) * fx                 # cada z_i es una estimación de I
  list(metodo = "Media muestral", x = x, fx = fx, z = z,
       estimacion = mean(z), ee = sd(z) / sqrt(N), usadas = N,
       tabla = data.frame(i = 1:N, u = u, x = x, f_x = fx,
                          estimacion_acumulada = cumsum(z) / (1:N)))
}


# --------------------------------------------------------------
# Método 2: Acierto y error (lanzar puntos al rectángulo)
#   Rectángulo [a, b] × [y_min, y_max] que contiene a la curva.
#   Puntos sobre el eje y debajo de f cuentan +1,
#   puntos bajo el eje y encima de f cuentan −1 (área negativa).
#   I ≈ Área del rectángulo · (aciertos positivos − aciertos negativos) / N
# --------------------------------------------------------------
mc_acierto_error <- function(f, a, b, N, fuente) {
  malla <- seq(a, b, length.out = 2001)
  fm <- f(malla)
  y_max <- max(0, max(fm)) * 1.02
  y_min <- min(0, min(fm)) * 1.02
  area  <- (b - a) * (y_max - y_min)

  u1 <- fuente(N); u2 <- fuente(N)
  x  <- a + (b - a) * u1
  y  <- y_min + (y_max - y_min) * u2
  fx <- f(x)

  positivo <- y > 0 & y <= fx
  negativo <- y < 0 & y >= fx
  z <- area * (positivo - negativo)

  resultado <- ifelse(positivo, "Acierto (+)", ifelse(negativo, "Acierto (−)", "Fallo"))
  list(metodo = "Acierto y error", x = x, y = y, fx = fx, z = z,
       aciertos = positivo | negativo, signo = positivo - negativo,
       y_min = y_min, y_max = y_max, area = area,
       estimacion = mean(z), ee = sd(z) / sqrt(N), usadas = 2 * N,
       tabla = data.frame(i = 1:N, u1 = u1, u2 = u2, x = x, y = y, f_x = fx,
                          resultado = resultado, estimacion_acumulada = cumsum(z) / (1:N)))
}


# --------------------------------------------------------------
# Valor de referencia con integración numérica (cuadratura de R)
# --------------------------------------------------------------
integral_referencia <- function(f, a, b) {
  r <- tryCatch(integrate(f, a, b, subdivisions = 1000L), error = function(e) NULL)
  if (is.null(r)) return(list(valor = NA, error = NA))
  list(valor = r$value, error = r$abs.error)
}


# --------------------------------------------------------------
# Convergencia: estimación y error estándar con los primeros n puntos
# (se toman ~600 valores de n repartidos en escala logarítmica)
# --------------------------------------------------------------
convergencia_mc <- function(z) {
  N <- length(z)
  n <- unique(round(exp(seq(0, log(N), length.out = 600))))
  suma  <- cumsum(z)
  suma2 <- cumsum(z^2)
  est <- suma[n] / n
  var_n <- pmax(suma2[n] / n - est^2, 0) * n / pmax(n - 1, 1)
  ee  <- sqrt(var_n / n)
  data.frame(n = n, estimacion = est, ee = ee,
             inferior = est - 1.96 * ee, superior = est + 1.96 * ee)
}
