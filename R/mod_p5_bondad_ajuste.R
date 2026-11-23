# ==============================================================
# Pestaña 5: Pruebas de Bondad de Ajuste
# Responsable: Compañera
# ==============================================================

mod_p5_bondad_ajuste_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Pruebas de Bondad de Ajuste"),
    p("Módulo en construcción.")
  )
}

mod_p5_bondad_ajuste_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del módulo aquí
  })
}
