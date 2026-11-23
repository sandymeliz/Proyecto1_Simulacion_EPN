# ==============================================================
# Pestaña 4: Variables Aleatorias Continuas
# Responsable: Cris
# ==============================================================

mod_p4_continuas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Variables Aleatorias Continuas"),
    p("Módulo en construcción.")
  )
}

mod_p4_continuas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del módulo aquí
  })
}
