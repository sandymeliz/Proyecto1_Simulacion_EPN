# ==============================================================
# Generación de variables aleatorias continuas
# ==============================================================


# --------------------------------------------------------------
# Fuente de números uniformes U(0, 1)
# --------------------------------------------------------------
crear_fuente_uniforme <- function(semilla, tipo = "mcl") {
  a_mcl <- 1664525
  c_mcl <- 1013904223
  m_mcl <- 2^32
  x <- semilla %% m_mcl
  if (tipo == "runif") set.seed(semilla)

  function(k) {
    if (tipo == "runif") return(runif(k))
    u <- numeric(k)
    for (i in 1:k) {
      x <<- (a_mcl * x + c_mcl) %% m_mcl
      u[i] <- (x + 0.5) / m_mcl
    }
    u
  }
}

# --------------------------------------------------------------
# Regla del trapecio: área bajo los puntos (x, y)
# --------------------------------------------------------------
trapecio <- function(x, y) {
  n <- length(x)
  sum((y[-1] + y[-n]) / 2 * (x[-1] - x[-n]))
}

# Interpolación lineal: valor de y en x0 a partir de una malla (xs, ys)
interpolar <- function(xs, ys, x0) {
  n <- length(xs)
  j <- findInterval(x0, xs, all.inside = TRUE)
  t <- (x0 - xs[j]) / (xs[j + 1] - xs[j])
  ys[j] + t * (ys[j + 1] - ys[j])
}

# ==============================================================
# 1. UNIFORME (a, b) — Transformación inversa
#    F(x) = (x − a) / (b − a)  →  X = a + (b − a) · U
# ==============================================================
sim_uniforme <- function(N, a, b, fuente) {
  if (b <= a) stop("El límite b debe ser mayor que a.")
  u <- fuente(N)
  x <- a + (b - a) * u
  list(x = x, usadas = N,
       tabla = data.frame(i = 1:N, u = u, x = x))
}

# ==============================================================
# 2. EXPONENCIAL (λ) — Transformación inversa
#    F(x) = 1 − e^(−λx)  →  X = −ln(U) / λ
# ==============================================================
sim_exponencial <- function(N, lambda, fuente) {
  if (lambda <= 0) stop("La tasa λ debe ser mayor que 0.")
  u <- fuente(N)
  x <- -log(u) / lambda
  list(x = x, usadas = N,
       tabla = data.frame(i = 1:N, u = u, x = x))
}

# ==============================================================
# 3a. NORMAL (μ, σ) — Box-Muller
#     Z1 = sqrt(−2 ln U1) · cos(2π U2)
#     Z2 = sqrt(−2 ln U1) · sin(2π U2)
#     X  = μ + σ · Z
# ==============================================================
sim_normal_box_muller <- function(N, mu, sigma, fuente) {
  if (sigma <= 0) stop("La desviación estándar σ debe ser mayor que 0.")
  pares <- ceiling(N / 2)
  u1 <- fuente(pares)
  u2 <- fuente(pares)
  R     <- sqrt(-2 * log(u1))
  theta <- 2 * pi * u2
  z1 <- R * cos(theta)
  z2 <- R * sin(theta)
  z  <- c(rbind(z1, z2))[1:N]
  uu1 <- rep(u1, each = 2)[1:N]
  uu2 <- rep(u2, each = 2)[1:N]
  x  <- mu + sigma * z

  list(x = x, usadas = 2 * pares, pares = data.frame(z1 = z1, z2 = z2),
       tabla = data.frame(i = 1:N, u1 = uu1, u2 = uu2, z = z, x = x))
}


# ==============================================================
# 3b. NORMAL (μ, σ) — Aceptación y Rechazo
#     1. Y = −ln(U1)
#     2. Si U2 ≤ exp(−(Y − 1)² / 2) se acepta: |Z| = Y
#     3. Con U3 se elige el signo: Z = Y si U3 ≤ 0.5, si no Z = −Y
#     Constante c = sqrt(2e/π) ≈ 1.3155
# ==============================================================
sim_normal_rechazo <- function(N, mu, sigma, fuente) {
  if (sigma <= 0) stop("La desviación estándar σ debe ser mayor que 0.")
  c_ross <- sqrt(2 * exp(1) / pi)

  z <- c(); y_ok <- c(); u2_ok <- c(); u3_ok <- c()
  cand_y <- c(); cand_v <- c(); cand_ok <- c()
  usadas <- 0

  while (length(z) < N) {
    k  <- ceiling((N - length(z)) * 1.4) + 10    # candidatos en este lote
    u1 <- fuente(k); u2 <- fuente(k); u3 <- fuente(k)
    usadas <- usadas + 3 * k

    y  <- -log(u1)
    acepta <- u2 <= exp(-(y - 1)^2 / 2)
    signo  <- ifelse(u3 <= 0.5, 1, -1)

    z     <- c(z, (signo * y)[acepta])
    y_ok  <- c(y_ok, y[acepta]); u2_ok <- c(u2_ok, u2[acepta]); u3_ok <- c(u3_ok, u3[acepta])

    # para el gráfico: altura del punto = U2 · c · g(Y), con g(y) = e^(−y)
    cand_y  <- c(cand_y, y)
    cand_v  <- c(cand_v, u2 * c_ross * exp(-y))
    cand_ok <- c(cand_ok, acepta)
  }

  z <- z[1:N]
  x <- mu + sigma * z
  list(x = x, usadas = usadas,
       tabla = data.frame(i = 1:N, y = y_ok[1:N], u2 = u2_ok[1:N], u3 = u3_ok[1:N], z = z, x = x),
       rechazo = list(
         cand  = data.frame(y = cand_y, v = cand_v, acepta = cand_ok),
         f     = function(t) 2 / sqrt(2 * pi) * exp(-t^2 / 2),    # mitad de la normal
         env   = function(t) c_ross * exp(-t),                    # c · g(t)
         rango = c(0, 5),
         tasa  = mean(cand_ok), tasa_teo = 1 / c_ross,
         nombre_f = "f(y) = 2/√(2π) · e^(−y²/2)", nombre_env = "c · g(y) = 1.3155 · e^(−y)"))
}


