# ==============================================================
# Trabajo Grupal N.º 1 - Simulación (EPN, 2026-B)
# Aplicativo interactivo en R Shiny
# Los módulos de cada pestaña están en la carpeta R/ y Shiny
# los carga automáticamente.
# ==============================================================

library(shiny)

ui <- navbarPage(
  title = "Simulación Estocástica",
  tabPanel("1. Pseudoaleatorios", mod_p1_pseudoaleatorios_ui("p1")),
  tabPanel("2. Montecarlo",       mod_p2_montecarlo_ui("p2")),
  tabPanel("3. V.A. Discretas",   mod_p3_discretas_ui("p3")),
  tabPanel("4. V.A. Continuas",   mod_p4_continuas_ui("p4")),
  tabPanel("5. Bondad de Ajuste", mod_p5_bondad_ajuste_ui("p5"))
)

server <- function(input, output, session) {
  mod_p1_pseudoaleatorios_server("p1")
  mod_p2_montecarlo_server("p2")
  mod_p3_discretas_server("p3")
  mod_p4_continuas_server("p4")
  mod_p5_bondad_ajuste_server("p5")
}

shinyApp(ui, server)
