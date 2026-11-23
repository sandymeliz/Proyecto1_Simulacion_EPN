# ==============================================================
# Pestaña 2: Integración por el Método de Montecarlo
# Responsable: Erika Haro
# ==============================================================

mod_p2_montecarlo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Integración por el Método de Montecarlo"),
    p("Módulo en construcción.")
  )
}

mod_p2_montecarlo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del módulo aquí
  })
}
