
# ==============================================================
# Generación de variables aleatorias continuas
# Responsable: Sandy
#
# Todas las variables se construyen a partir de números uniformes
# producidos por un Método Congruencial Lineal propio
# (los mismos principios de la Pestaña 1).
# ==============================================================


# --------------------------------------------------------------
# Fuente de números uniformes U(0, 1)
#   MCL con parámetros de Numerical Recipes:
#   x(n+1) = (1664525 · x(n) + 1013904223) mod 2^32
#   Cumple Hull-Dobell → periodo completo 2^32.
#   Se usa u = (x + 0.5) / m para que u nunca sea 0 ni 1
#   (evita log(0) en los métodos que usan logaritmos).
#
# Devuelve una función: cada vez que se llama con k entrega
# los k siguientes números de la secuencia.
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

  # se intercalan: z1[1], z2[1], z1[2], z2[2], ...
  z  <- c(rbind(z1, z2))[1:N]
  uu1 <- rep(u1, each = 2)[1:N]
  uu2 <- rep(u2, each = 2)[1:N]
  x  <- mu + sigma * z

  list(x = x, usadas = 2 * pares, pares = data.frame(z1 = z1, z2 = z2),
       tabla = data.frame(i = 1:N, u1 = uu1, u2 = uu2, z = z, x = x))
}


# ==============================================================
# 3b. NORMAL (μ, σ) — Aceptación y Rechazo (Ross, cap. 5)
#     1. Y = −ln(U1)  (exponencial de tasa 1)
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


# ==============================================================
# 4. DENSIDAD PERSONALIZADA f(x) en [a, b]
# ==============================================================

# Convierte el texto escrito por el usuario en una función de x.
# Por seguridad solo se permiten números, x, operaciones y funciones matemáticas.
crear_funcion_usuario <- function(texto) {
  permitidas <- c("x", "exp", "log", "sqrt", "sin", "cos", "tan", "abs", "pi")
  if (grepl("[^0-9A-Za-z_+*/^(). -]", texto))
    stop("La función tiene caracteres no permitidos. Use solo números, x, + − * / ^ ( ) y funciones como exp, log, sqrt, sin, cos.")
  palabras <- regmatches(texto, gregexpr("[A-Za-z_][A-Za-z_0-9]*", texto))[[1]]
  invalidas <- palabras[!(palabras %in% permitidas)]
  if (length(invalidas) > 0)
    stop(paste0("No se reconoce: ", paste(unique(invalidas), collapse = ", "),
                ". Funciones permitidas: exp, log, sqrt, sin, cos, tan, abs, pi."))
  expr <- parse(text = texto)
  function(x) {
    y <- eval(expr, list(x = x))
    if (length(y) == 1) y <- rep(y, length(x))
    y
  }
}

# Revisa que f sea una densidad válida y prepara la malla para F(x)
preparar_densidad <- function(f, a, b) {
  if (!(b > a)) stop("El límite b debe ser mayor que a.")
  xs <- seq(a, b, length.out = 2001)
  fx <- f(xs)
  if (any(!is.finite(fx))) stop("f(x) no está definida en todo el intervalo [a, b] (da infinito o NaN).")
  if (any(fx < 0)) stop("f(x) toma valores negativos: una densidad debe ser f(x) ≥ 0.")

  area <- trapecio(xs, fx)
  if (area <= 0) stop("El área bajo f(x) es 0.")
  aviso <- NULL
  if (abs(area - 1) > 0.01) {
    aviso <- paste0("El área bajo f(x) en [", a, ", ", b, "] es ", round(area, 4),
                    " (debería ser 1). Se normalizó dividiendo f(x) para ", round(area, 4), ".")
  }
  fx <- fx / area

  # F(x) acumulando trapecios
  Fx <- c(0, cumsum((fx[-1] + fx[-length(fx)]) / 2 * diff(xs)))
  Fx <- Fx / Fx[length(Fx)]

  list(xs = xs, fx = fx, Fx = Fx, area = area, aviso = aviso,
       f_norm = function(t) ifelse(t < a | t > b, 0, f(t) / area))
}

# 4a. Transformación inversa numérica: X = F^(-1)(U)
sim_personalizada_inversa <- function(N, f, a, b, fuente) {
  d <- preparar_densidad(f, a, b)
  u <- fuente(N)
  x <- interpolar(d$Fx, d$xs, u)       # se busca x tal que F(x) = u
  list(x = x, usadas = N, densidad = d, aviso = d$aviso,
       tabla = data.frame(i = 1:N, u = u, x = x))
}

# 4b. Aceptación y rechazo con propuesta uniforme en [a, b]
#     Y = a + (b − a)·U1,  V = M·U2   → se acepta si V ≤ f(Y)
sim_personalizada_rechazo <- function(N, f, a, b, fuente) {
  d <- preparar_densidad(f, a, b)
  M <- max(d$fx) * 1.02                 # techo un poco por encima del máximo
  x <- c(); v_ok <- c()
  cand_y <- c(); cand_v <- c(); cand_ok <- c()
  usadas <- 0

  while (length(x) < N) {
    k  <- ceiling((N - length(x)) * M * (b - a) * 1.2) + 10
    u1 <- fuente(k); u2 <- fuente(k)
    usadas <- usadas + 2 * k
    y <- a + (b - a) * u1
    v <- M * u2
    acepta <- v <= d$f_norm(y)
    x    <- c(x, y[acepta]); v_ok <- c(v_ok, v[acepta])
    cand_y <- c(cand_y, y); cand_v <- c(cand_v, v); cand_ok <- c(cand_ok, acepta)
  }
  x <- x[1:N]
  c_const <- M * (b - a)
  list(x = x, usadas = usadas, densidad = d, aviso = d$aviso,
       tabla = data.frame(i = 1:N, y = x, v = v_ok[1:N], f_y = d$f_norm(x)),
       rechazo = list(
         cand  = data.frame(y = cand_y, v = cand_v, acepta = cand_ok),
         f     = d$f_norm,
         env   = function(t) rep(M, length(t)),
         rango = c(a, b),
         tasa  = mean(cand_ok), tasa_teo = 1 / c_const,
         nombre_f = "f(x)", nombre_env = paste0("M = máx f(x) = ", round(M, 4))))
}
