# ==============================================================
# Generadores de números pseudoaleatorios
# ==============================================================

# --------------------------------------------------------------
# Función auxiliar: busca el primer valor que se repite
# --------------------------------------------------------------
buscar_ciclo <- function(x) {
  if (length(x) < 2) return(list(periodo = NA, aviso = NULL))
  for (i in 2:length(x)) {
    anterior <- which(x[1:(i - 1)] == x[i])
    if (length(anterior) > 0) {
      return(list(periodo = i - anterior[1],
                  aviso = paste0("Se detectó un ciclo: x", i - 1, " = x", anterior[1] - 1,
                                 ". Periodo = ", i - anterior[1],
                                 "; desde ahí los valores se repiten.")))
    }
  }
  list(periodo = NA, aviso = NULL)
}


# --------------------------------------------------------------
# 1. Método Congruencial Lineal
#    x(n+1) = (a * x(n) + c) mod m      u(n) = x(n) / m
# --------------------------------------------------------------
gen_mcl <- function(x0, a, c, m, N) {
  if (a * m >= 2^53) stop("a·m es demasiado grande para calcular con precisión.")

  x <- numeric(N + 1)
  x[1] <- x0
  for (i in 1:N) {
    x[i + 1] <- (a * x[i] + c) %% m
  }
  u <- x[2:(N + 1)] / m

  tabla <- data.frame(n          = 1:N,
                      x_anterior = x[1:N],
                      ax_mas_c   = a * x[1:N] + c,
                      x_n        = x[2:(N + 1)],
                      u_n        = u)

  ciclo <- buscar_ciclo(x)
  list(tabla = tabla, u = u, aviso = ciclo$aviso, periodo = ciclo$periodo)
}


# --------------------------------------------------------------
# 2. Método de Lehmer
#    x(i) * c  ->  k cifras izquierda | n cifras derecha
#    x(i+1) = derecha - izquierda       u(i) = x(i) / 10^n
# --------------------------------------------------------------
gen_lehmer <- function(x0, c, n, N) {
  if (x0 >= 10^n) stop(paste("La semilla debe tener como máximo", n, "cifras."))

  x <- numeric(N)
  producto <- izquierda <- derecha <- numeric(N)
  x[1] <- x0
  aviso_negativo <- NULL
  total <- N

  for (i in 1:N) {
    producto[i]  <- x[i] * c
    izquierda[i] <- floor(producto[i] / 10^n)
    derecha[i]   <- producto[i] %% 10^n
    siguiente    <- derecha[i] - izquierda[i]

    if (i < N) {
      if (siguiente < 0) {
        aviso_negativo <- paste0("En la iteración ", i - 1, " se obtuvo un número negativo (",
                                 derecha[i], " - ", izquierda[i], " = ", siguiente,
                                 "). El método se detiene: es uno de sus inconvenientes.")
        total <- i
        break
      }
      x[i + 1] <- siguiente
    }
  }

  x <- x[1:total]
  u <- x / 10^n

  tabla <- data.frame(i          = 0:(total - 1),
                      x_i        = x,
                      x_por_c    = producto[1:total],
                      izquierda  = izquierda[1:total],
                      derecha    = derecha[1:total],
                      x_siguiente = derecha[1:total] - izquierda[1:total],
                      u_i        = u)

  ciclo <- buscar_ciclo(x)
  aviso <- if (!is.null(aviso_negativo)) aviso_negativo else ciclo$aviso
  list(tabla = tabla, u = u, aviso = aviso, periodo = ciclo$periodo)
}


# --------------------------------------------------------------
# 3. Método de los Cuadrados Medios
#    x(i+1) = n cifras centrales de x(i)^2     u(i) = x(i) / 10^n
# --------------------------------------------------------------
gen_cuadrados_medios <- function(x0, n, N) {
  if (n %% 2 != 0) stop("El número de cifras n debe ser par.")
  if (x0 >= 10^n) stop(paste("La semilla debe tener como máximo", n, "cifras."))

  x <- numeric(N)
  cuadrado <- numeric(N)
  x[1] <- x0
  for (i in 1:N) {
    cuadrado[i] <- x[i]^2
    # cifras centrales: se quitan n/2 cifras de la derecha y se toman n
    if (i < N) x[i + 1] <- floor(cuadrado[i] / 10^(n / 2)) %% 10^n
  }
  u <- x / 10^n

  tabla <- data.frame(i         = 0:(N - 1),
                      x_i       = x,
                      x_i_cuad  = cuadrado,
                      centrales = c(x[-1], floor(cuadrado[N] / 10^(n / 2)) %% 10^n),
                      u_i       = u)

  ciclo <- buscar_ciclo(x)
  list(tabla = tabla, u = u, aviso = ciclo$aviso, periodo = ciclo$periodo)
}
