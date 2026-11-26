# ==============================================================
# Pestaña 5: Pruebas de Bondad de Ajuste
# Responsable: Erika Haro
# ==============================================================

mod_p5_bondad_ajuste_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Fuente de datos"),
          radioButtons(ns("fuente"), "Origen:",
                       choices = c("Generar con MCL" = "mcl",
                                   "Subir archivo" = "archivo")),
          conditionalPanel(
            condition = "input.fuente == 'mcl'", ns = ns,
            numericInput(ns("a_m"), "a:", value = 31),
            numericInput(ns("m_m"), "m:", value = 67),
            numericInput(ns("x0"), "x0 (semilla):", value = 12),
            numericInput(ns("N"), "N:", value = 1000)
          ),
          conditionalPanel(
            condition = "input.fuente == 'archivo'", ns = ns,
            fileInput(ns("file"), "Subir .csv o .xlsx:",
                      accept = c(".csv", ".xlsx"))
          ),
          numericInput(ns("alfa"), "Nivel de significancia α:", value = 0.05, min = 0, max = 1),
          actionButton(ns("run"), "Ejecutar pruebas", class = "btn-primary")
        )
      ),
      column(8,
        h4("Resultados de las pruebas"),
        tableOutput(ns("tabla_pruebas")),
        plotOutput(ns("grafico_hist"), height = "300px"),
        plotOutput(ns("grafico_qq"), height = "300px"),
        plotOutput(ns("grafico_rachas"), height = "250px")
      )
    )       
  )
}

mod_p5_bondad_ajuste_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    datos <- eventReactive(input$run, {
      if (input$fuente == "mcl") {
        runif_vals <- mcm(a = as.integer(input$a_m),
                          m = as.integer(input$m_m),
                          x0 = input$x0, n = input$N)
      } else {
        req(input$file)
        ruta <- input$file$datapath
        if (grepl("\\.csv$", input$file$name)) {
          df <- read.csv(ruta)
        } else {
          if (!requireNamespace("openxlsx", quietly = TRUE))
            install.packages("openxlsx")
          df <- openxlsx::read.xlsx(ruta)
        }
        runif_vals <- as.numeric(df[[1]])
      }
      runif_vals
    })
    
    #-------------------------------------------------------------------------------------------
    # Tabla con resultados de KS y Rachas
    #-------------------------------------------------------------------------------------------
    output$tabla_pruebas <- renderTable({
      x <- datos()
      req(length(x) > 10)

      # Kolmogorov-Smirnov vs U(0,1)
      ks <- suppressWarnings(ks.test(x, "punif"))
      # Prueba de rachas
      rt <- runs_test(x)

      alfa <- input$alfa

      df_res <- data.frame(
        Prueba = c("Kolmogorov-Smirnov", "Rachas (Runs Test)"),
        Estadistico = c(ks$statistic, rt$Z),
        p_valor = c(ks$p.value, rt$p_valor),
        Decision = c(
          ifelse(ks$p.value < alfa, "Rechazar H0 (no uniforme)", "No rechazar H0 (uniforme)"),
          ifelse(rt$p_valor < alfa, "Rechazar H0 (dependiente)", "No rechazar H0 (independiente)")
        )
      )
      df_res
    })
    
    #-------------------------------------------------------------------------------------------
    # Histograma con densidad teórica
    #-------------------------------------------------------------------------------------------
    output$grafico_hist <- renderPlot({
      x <- datos()
      hist(x, breaks = 30, freq = FALSE, col = "lightblue",
           main = "Histograma vs. densidad U(0,1)",
           xlab = "Valores", ylim = c(0, 1.5))
      curve(ifelse(x > 0, 1, 0), from = 0, to = 1, add = TRUE,
            col = "red", lwd = 2)
    })
    
    #-------------------------------------------------------------------------------------------
    # Q-Q plot contra U(0,1)
    #-------------------------------------------------------------------------------------------
    output$grafico_qq <- renderPlot({
      x <- datos()
      qqplot(runif(length(x)), x,
             main = "Q-Q Plot vs. Uniforme",
             xlab = "Cuantiles teóricos U(0,1)",
             ylab = "Cuantiles muestrales")
      abline(0, 1, col = "red", lwd = 2)
    })
    
    #-------------------------------------------------------------------------------------------
    # Gráfico de rachas (secuencia binaria)
    #-------------------------------------------------------------------------------------------
    output$grafico_rachas <- renderPlot({
      x <- datos()
      binario <- as.integer(x > median(x))
      plot(binario, type = "l", col = "steelblue", lwd = 1,
           main = "Secuencia binaria (arriba/abajo de la mediana)",
           xlab = "Índice", ylab = "0 / 1")
    }) 
  })
}
