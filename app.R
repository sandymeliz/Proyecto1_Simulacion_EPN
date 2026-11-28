# ==============================================================
# Trabajo Grupal N.º 1 - Simulación (EPN, 2026-B)
# Aplicativo interactivo en R Shiny
# ==============================================================

library(shiny)

# Textos de todas las tablas (DT) en español
options(DT.options = list(language = list(
  search = "Buscar:", lengthMenu = "Mostrar _MENU_ filas",
  info = "Mostrando _START_ a _END_ de _TOTAL_ filas", infoEmpty = "Sin datos",
  infoFiltered = "(filtrado de _MAX_ filas)", zeroRecords = "No se encontraron resultados",
  paginate = list(first = "Primero", last = "Último", `next` = "Siguiente", previous = "Anterior")
)))

# --------------------------------------------------------------
# Estilos del menú superior y del pie de página
# --------------------------------------------------------------
CSS_MINIMAL <- paste0("
body { background: #fcfcff; font-size: 14px; }

/* Encabezado de cada pestaña: compacto y pastel */
.p1-cabecera { padding: 14px 22px !important; border-radius: 14px !important; margin-bottom: 14px !important;
               background: ", PALETA$agua, " !important; }
.p2-cabecera { background: ", PALETA$lila, " !important; }
.p3-cabecera { background: ", PALETA$vainilla, " !important; }
.p4-cabecera { background: ", PALETA$chicle, "99 !important; }
.p5-cabecera { background: ", PALETA$pistacho, " !important; }
.p1-cabecera h2 { font-size: 21px !important; font-weight: 700 !important; }
.p1-cabecera h2 .fa, .p1-cabecera h2 svg { font-size: 17px; opacity: .75; margin-right: 4px; }
.p1-cabecera p { font-size: 13.5px !important; margin-top: 2px !important; }

/* Títulos numerados */
.p1-paso-num { width: 20px !important; height: 20px !important; line-height: 20px !important; font-size: 11px !important; }
.p1 h4 { font-size: 15.5px; }

/* Tarjetas de selección: más bajas y livianas */
.p1-op { padding: 9px 14px !important; border-width: 1.5px !important; border-radius: 12px !important; gap: 10px !important; }
.p1-op:hover { transform: none !important; box-shadow: 0 2px 8px rgba(53,56,125,.07) !important; }
.p1-op .p1-ico { width: 30px !important; height: 30px !important; font-size: 13px !important; border-radius: 9px !important; }
.p1-op .p1-tit { font-size: 14.5px !important; font-weight: 600 !important; }
.p1-op .p1-lema { font-size: 12px !important; }
label:has(input:checked) .p1-op { box-shadow: none !important; transform: none !important; }

/* Paneles sin sombras */
.p1-panel { box-shadow: none !important; border: 1px solid #eef0f6 !important; border-radius: 14px !important; padding: 14px 16px !important; }
.p1-info { padding: 12px 18px !important; border-radius: 14px !important; border-left-width: 4px !important; }
.p1-info h4 { font-size: 15px !important; }
.p1-formula { font-size: 16px !important; padding: 6px 10px !important; }
.p1-chip { font-size: 11.5px !important; padding: 3px 10px !important; }

/* Explicaciones: discretas pero visibles */
.p1-que { background: ", PALETA$vainilla, "99 !important; border-left: 3px solid ", PALETA$amarillo, " !important;
          font-size: 12.5px !important; padding: 7px 12px !important; border-radius: 8px !important; }
.p1-ayuda { font-size: 11.5px !important; }

/* Botones */
.p1-boton { font-size: 15px !important; padding: 8px !important; border-radius: 10px !important; box-shadow: none !important;
            color: ", PALETA$noche, " !important;
            background: linear-gradient(90deg, ", PALETA$chicle, ", ", PALETA$lila, ") !important; }
.p1 .p1-panel a.p1-desc, .p1 .p1-panel a.p1-desc:hover, .p1 .p1-panel a.p1-desc .fa { color: ", PALETA$noche, " !important; }
.p1-desc { padding: 7px 12px !important; font-size: 13.5px !important; border-radius: 10px !important; }
.p1 .p1-panel a.p1-desc:nth-of-type(1) { background: ", PALETA$pistacho, " !important; }
.p1 .p1-panel a.p1-desc:nth-of-type(2) { background: ", PALETA$agua, " !important; }
.p1 .p1-panel a.p1-desc:nth-of-type(3) { background: ", PALETA$lila, " !important; }

/* Pestañas internas */
.p1 .nav-tabs > li > a { font-size: 13.5px; padding: 8px 12px; }
.p1 .nav-tabs > li.active > a { border-top: 3px solid ", PALETA$chicle, " !important; }
.p1 .nav-pills > li.active > a { background: ", PALETA$lila, " !important; color: ", PALETA$noche, " !important; }

/* Pie de página */
.pie { margin-top: 28px; padding: 14px 28px; font-size: 12.5px; color: ", PALETA$noche, ";
       background: linear-gradient(90deg, ", PALETA$agua, ", ", PALETA$lila, ", ", PALETA$chicle, ");
       display: flex; justify-content: space-between; flex-wrap: wrap; gap: 6px; }
.pie b { font-weight: 700; }
")

# Menú superior: barra blanca con una línea pastel abajo
CSS_MENU_B <- paste0("
.navbar.navbar-default { background: white; border: none; border-radius: 0; min-height: 58px; margin-bottom: 16px;
  position: sticky; top: 0; z-index: 1000; box-shadow: 0 1px 0 #eef0f6;
  border-bottom: 4px solid transparent;
  border-image: linear-gradient(90deg, ", PALETA$agua, ", ", PALETA$lila, ", ", PALETA$chicle, ", ", PALETA$vainilla, ", ", PALETA$pistacho, ") 1; }
")
CSS_MENU_COMUN <- paste0("
.navbar-default .navbar-brand { height: 58px; padding: 9px 16px; display: flex; align-items: center; }
.marca { display: flex; align-items: center; gap: 10px; }
.marca-icono { width: 32px; height: 32px; border-radius: 9px; display: flex; align-items: center; justify-content: center;
               font-size: 15px; color: ", PALETA$violeta, "; background: white; border: 1.5px solid ", PALETA$lila, "; }
.marca-titulo { color: ", PALETA$noche, "; font-weight: 700; font-size: 16px; line-height: 1.1; }
.marca-sub { color: #8a8ea8; font-size: 10px; font-weight: 600; letter-spacing: 1px; text-transform: uppercase; }
.navbar-default .navbar-nav { margin-top: 10px; }
.navbar-default .navbar-nav > li > a { color: ", PALETA$pizarra, "; font-weight: 600; font-size: 13.5px;
  border-radius: 20px; margin: 0 2px; padding: 7px 13px; }
.navbar-default .navbar-nav > li > a:hover, .navbar-default .navbar-nav > li > a:focus { color: ", PALETA$noche, "; background: rgba(255,255,255,.55); }
.navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus { color: ", PALETA$noche, "; background: white; box-shadow: 0 1px 4px rgba(53,56,125,.12); }
.num-pestana { display: inline-block; width: 18px; height: 18px; border-radius: 50%; text-align: center; line-height: 18px;
               font-size: 10.5px; font-weight: 700; margin-right: 5px; background: rgba(53,56,125,.10); color: ", PALETA$noche, "; }
.navbar-nav > .active .num-pestana { background: ", PALETA$chicle, "; }
.navbar-nav > li > a .fa, .navbar-nav > li > a svg { font-size: 12px; margin-right: 3px; opacity: .7; }
.navbar-default .navbar-toggle { margin-top: 12px; }
")
# Menú elegido: barra blanca con línea pastel (opción B)
CSS_MENU <- paste0(CSS_MINIMAL, CSS_MENU_COMUN, CSS_MENU_B, "
.navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus { background: ", PALETA$chicle, "88 !important; box-shadow: none !important; }
.navbar-default .navbar-nav > li > a:hover { background: ", PALETA$lila, "88 !important; }

/* Minimalismo: sin iconos junto a los textos (se conservan en tarjetas de selección y de resumen) */
.navbar-nav i[class*='fa'], .p1-cabecera i[class*='fa'], .p1 h4 i[class*='fa'], .p1 h5 i[class*='fa'],
.p1 .nav-tabs i[class*='fa'], .p1 .nav-pills i[class*='fa'], .p1-que i[class*='fa'], .p1-ayuda i[class*='fa'],
.p1-chip i[class*='fa'], .p1-boton i[class*='fa'], .p1-desc i[class*='fa'], .pie i[class*='fa'],
.p1 .alert i[class*='fa'], .p1 a.action-button i[class*='fa'], .p1 .shiny-download-link i[class*='fa'] { display: none !important; }
")

# Título de cada pestaña: número + icono + nombre
pestana <- function(numero, icono, texto) {
  tagList(span(class = "num-pestana", numero), icon(icono), texto)
}


# ==============================================================
# INTERFAZ
# ==============================================================
ui <- navbarPage(
  title = div(class = "marca",
              div(class = "marca-icono", icon("dice-d20")),
              div(div(class = "marca-titulo", "Simulación Estocástica"),
                  div(class = "marca-sub", "EPN · Trabajo Grupal 1 · 2026-B"))),
  windowTitle = "Simulación Estocástica – EPN",
  collapsible = TRUE,
  header = tags$head(tags$style(HTML(CSS_MENU))),

  tabPanel(pestana(1, "dice",        "Pseudoaleatorios"), mod_p1_pseudoaleatorios_ui("p1")),
  tabPanel(pestana(2, "chart-area",  "Montecarlo"),       mod_p2_montecarlo_ui("p2")),
  tabPanel(pestana(3, "chart-simple","Discretas"),        mod_p3_discretas_ui("p3")),
  tabPanel(pestana(4, "wave-square", "Continuas"),        mod_p4_continuas_ui("p4")),
  tabPanel(pestana(5, "clipboard-check", "Bondad de ajuste"), mod_p5_bondad_ajuste_ui("p5")),

  footer = div(class = "pie",
               span(icon("graduation-cap"), " Escuela Politécnica Nacional · Simulación · Trabajo Grupal N.º 1"),
               span(icon("user-pen"), " Autoras: ", tags$b("Sandy González"), " y ", tags$b("Erika Haro")))
)


# ==============================================================
# SERVIDOR
# ==============================================================
server <- function(input, output, session) {
  mod_p1_pseudoaleatorios_server("p1")
  mod_p2_montecarlo_server("p2")
  mod_p3_discretas_server("p3")
  mod_p4_continuas_server("p4")
  mod_p5_bondad_ajuste_server("p5")
}

shinyApp(ui, server)
