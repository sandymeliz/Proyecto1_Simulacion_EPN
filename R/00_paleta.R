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
  p_rosa      = "#FFD7D6"
)

# Color con transparencia (alfa entre 0 y 1) para gráficos
transparente <- function(hex, alfa) {
  rgb <- col2rgb(hex)
  paste0("rgba(", rgb[1], ",", rgb[2], ",", rgb[3], ",", alfa, ")")
}
