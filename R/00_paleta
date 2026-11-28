# ==============================================================
# Paleta de colores del aplicativo (compartida por todas las pestañas)
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
  p_rosa      = "#FFD7D6"
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
      div(class = "p1-ico", style = paste0("background:", color, ";"), icon(icono)),
      div(div(class = "p1-tit", titulo), div(class = "p1-lema", lema)))
}
