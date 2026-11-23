# ==============================================================
# Pestaña 1: Generación de Números Pseudoaleatorios
# Responsable: Cris
# ==============================================================

mod_p1_pseudoaleatorios_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Generación de Números Pseudoaleatorios"),
    p("Módulo en construcción.")
  )
}

mod_p1_pseudoaleatorios_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del módulo aquí
  })
}
