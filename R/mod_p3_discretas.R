# ==============================================================
# Pestaña 3: Variables Aleatorias Discretas
# Responsable: Compañera
# ==============================================================

mod_p3_discretas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Variables Aleatorias Discretas"),
    p("Módulo en construcción.")
  )
}

mod_p3_discretas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del módulo aquí
  })
}
