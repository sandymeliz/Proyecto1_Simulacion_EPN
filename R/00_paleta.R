# ==============================================================
# Paleta de colores del aplicativo
# ==============================================================

PALETA <- list(
  # Rainbow
  rojo        = "#F65353",
  coral       = "#FF8A8A",
  durazno     = "#FF9E62",
  naranja     = "#FF9F1C",
  amarillo    = "#F6CC45",
  lima        = "#AED67E",
  menta       = "#72AA91",
  petroleo    = "#5C93AA",
  celeste     = "#81B1CD",
  azul        = "#03468F",
  indigo      = "#5C6BAA",
  violeta     = "#795CAA",
  orquidea    = "#A55CAA",
  rosa        = "#FFA9BA",
  # Moody
  salvia      = "#829D88",
  pizarra     = "#355C7D",
  noche       = "#35387D",
  # Pastel
  p_celeste   = "#A8C7D6",
  p_lavanda   = "#A8AFD6",
  p_lila      = "#D4A8D6",
  p_rosa      = "#FFD7D6",
  # Pastel 2 (paleta minimalista)
  agua        = "#CFF4F3",
  lila        = "#EFD6FF",
  chicle      = "#FFC0D7",
  vainilla    = "#FFF9C2",
  pistacho    = "#CEEBD1"
)

# Color con transparencia (alfa entre 0 y 1) para gráficos
transparente <- function(hex, alfa) {
  rgb <- col2rgb(hex)
  paste0("rgba(", rgb[1], ",", rgb[2], ",", rgb[3], ",", alfa, ")")
}

# Estilos del selector en forma de tarjetas (radioButtons con clase `clase`).
# `colores` es una lista con el color de cada opción, por ejemplo list(binom = "#5C93AA").
css_tarjetas_opcion <- function(clase, colores) {
  reglas <- paste0("
.", clase, " .shiny-options-group { display: flex; gap: 14px; flex-wrap: wrap; }
.", clase, " .radio-inline { flex: 1; min-width: 200px; margin: 0 !important; padding: 0 !important; }
.", clase, " input[type=radio] { display: none; }
.", clase, " .radio-inline > span { display: block; }
.", clase, " label:has(input:checked) .p1-op { box-shadow: 0 8px 18px rgba(53,56,125,.18); transform: translateY(-2px); }
")
  for (v in names(colores)) {
    reglas <- paste0(reglas, ".", clase, " label:has(input[value=", v, "]:checked) .p1-op { border-color: ",
                     colores[[v]], "; background: ", colores[[v]], "22; }\n")
  }
  reglas
}

# Tarjeta de opción con icono, título y lema
opcion_tarjeta <- function(titulo, lema, icono, color) {
  div(class = "p1-op",
      div(class = "p1-ico", style = paste0("background:", transparente(color, 0.16), "; color:", color, ";"), icon(icono)),
      div(div(class = "p1-tit", titulo), div(class = "p1-lema", lema)))
}


# Nombres legibles para las columnas de las tablas
NOMBRES_COLUMNAS <- c(
  n = "n", i = "i", u = "U", x = "x", y = "y", v = "V", z = "z",
  x_anterior = "x(n−1)", ax_mas_c = "a·x(n−1) + c", x_n = "x(n)", u_n = "u(n)",
  x_i = "x(i)", x_por_c = "x(i)·c", izquierda = "Izquierda", derecha = "Derecha",
  x_siguiente = "x(i+1)", u_i = "u(i)", x_i_cuad = "x(i)²", centrales = "Cifras centrales",
  u1 = "U₁", u2 = "U₂", u3 = "U₃", f_x = "f(x)", f_y = "f(y)",
  estimacion_acumulada = "Estimación acumulada", resultado = "Resultado",
  comparaciones = "Comparaciones"
)
nombres_columnas <- function(nombres) {
  ifelse(nombres %in% names(NOMBRES_COLUMNAS), NOMBRES_COLUMNAS[nombres], gsub("_", " ", nombres))
}
