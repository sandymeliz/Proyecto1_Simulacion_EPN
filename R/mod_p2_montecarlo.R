# ==============================================================
# Pestaña 2: Integración por el Método de Montecarlo
# Responsable: Erika Haro
# ==============================================================
mod_p2_montecarlo_ui <- function(id) 
{
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Parámetros de la integral"),
          textInput(ns("fx"), "Función f(x) (sintaxis R):", value = "exp(-x^2)"),
          selectInput(ns("tipo"), "Tipo de rango:",
                      choices = c("Finito [a, b]" = "finito",
                                  "Semi-infinito [0, ∞)" = "semi",
                                  "Infinito (-∞, ∞)" = "infinito")),
          numericInput(ns("a"), "Límite inferior a:", value = 0),
          numericInput(ns("b"), "Límite superior b:", value = 1),
          numericInput(ns("N"), "Número de simulaciones N:", value = 10000, min = 100),
          actionButton(ns("run"), "Calcular", class = "btn-primary")
        )
      ),
      column(8,
        verbatimTextOutput(ns("resultado")),
        plotOutput(ns("grafico_geom"), height = "300px"),
        plotOutput(ns("grafico_conv"), height = "300px"),
        downloadButton(ns("descargar"), "Descargar resultados (.csv)")
      )
    )
  )
}


mod_p2_montecarlo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #---------------------------------------------------------------------------------------------------------
    # Reactive que calcula la estimación MC
    #---------------------------------------------------------------------------------------------------------
    calculo <- eventReactive(input$run, {
      req(input$fx)
      fx <- input$fx
      N <- input$N
      a <- input$a
      b <- input$b
      tipo <- input$tipo
      
      #---------------------------------------------------------------------------------------------------------
      # Simulación según el tipo de rango
      #---------------------------------------------------------------------------------------------------------
      if (tipo == "finito") {
        y <- runif(N)
        x <- a + (b - a) * y
        vals <- eval(parse(text = fx), list(x = x)) * (b - a)
      } else if (tipo == "semi") {
        y <- runif(N)
        x <- 1 / y - 1
        vals <- eval(parse(text = fx), list(x = x)) / (y^2)
      } else {  # infinito (-∞, ∞) usando dos colas
        y <- runif(N)
        x_pos <- (1 / y) - 1
        x_neg <- 1 - (1 / y)
        v_pos <- eval(parse(text = fx), list(x = x_pos)) / (y^2)
        v_neg <- eval(parse(text = fx), list(x = x_neg)) / (y^2)
        vals <- v_pos + v_neg
      }
      
      #---------------------------------------------------------------------------------------------------------
      # Estimación, error estándar y referencia
      #---------------------------------------------------------------------------------------------------------
      estimacion <- mean(vals)
      error_est <- sd(vals) / sqrt(N)
      
      #---------------------------------------------------------------------------------------------------------
      # Valor de referencia con integrate()
      #---------------------------------------------------------------------------------------------------------
      f_ref <- function(x) {
        eval(parse(text = fx), list(x = x))
      }
      ref <- tryCatch({
        if (tipo == "finito")
          integrate(f_ref, lower = a, upper = b)$value
        else if (tipo == "semi")
          integrate(f_ref, lower = 0, upper = Inf)$value
        else
          integrate(f_ref, lower = -Inf, upper = Inf)$value
      }, error = function(e) NA)

      list(estimacion = estimacion, error_est = error_est,
           referencia = ref, vals = vals, fx = fx, a = a, b = b,
           tipo = tipo, N = N)
    })
      
    #---------------------------------------------------------------------------------------------------------
    # Texto de resultados
    #---------------------------------------------------------------------------------------------------------
    output$resultado <- renderPrint({
      r <- calculo()
      cat(sprintf("Estimación MC (N = %d): %.6f\n", r$N, r$estimacion))
      cat(sprintf("Error estándar:          %.6f\n", r$error_est))
      cat(sprintf("IC 95%%: [%.6f, %.6f]\n",
                  r$estimacion - 1.96 * r$error_est,
                  r$estimacion + 1.96 * r$error_est))
      if (!is.na(r$referencia)) {
        cat(sprintf("Valor de referencia:     %.6f\n", r$referencia))
        cat(sprintf("Error absoluto:          %.6f\n",
                    abs(r$estimacion - r$referencia)))
      }
    })
    
    #---------------------------------------------------------------------------------------------------------
    # Gráfico geométrico: puntos aleatorios bajo la curva
    #---------------------------------------------------------------------------------------------------------
    output$grafico_geom <- renderPlot({
      r <- calculo()
      if (r$tipo == "finito") {
        xs <- seq(r$a, r$b, length.out = 200)
        ys <- eval(parse(text = r$fx), list(x = xs))
        plot(xs, ys, type = "l", col = "red", lwd = 2,
             xlab = "x", ylab = "f(x)",
             main = "Representación geométrica")
        n_pts <- min(300, r$N)
        x_rand <- runif(n_pts, r$a, r$b)
        y_rand <- runif(n_pts, min(ys), max(ys))
        dentro <- y_rand <= eval(parse(text = r$fx), list(x = x_rand))
        points(x_rand[dentro], y_rand[dentro], col = "blue", pch = 16, cex = 0.5)
        points(x_rand[!dentro], y_rand[!dentro], col = "gray", pch = 16, cex = 0.5)
      } else {
        hist(r$vals, breaks = 40, col = "lightblue",
             main = "Distribución de las evaluaciones f(x)",
             xlab = "Valor simulado")
      }
    })
      
    #---------------------------------------------------------------------------------------------------------
    # Gráfico de convergencia
    #---------------------------------------------------------------------------------------------------------
    output$grafico_conv <- renderPlot({
      r <- calculo()
      seq_N <- seq(100, r$N, by = max(1, r$N %/% 200))
      estimaciones <- sapply(seq_N, function(k) mean(r$vals[1:k]))
      plot(seq_N, estimaciones, type = "l", col = "blue", lwd = 2,
           xlab = "Número de simulaciones N",
           ylab = "Estimación de la integral",
           main = "Convergencia del método de Montecarlo")
      if (!is.na(r$referencia)) {
        abline(h = r$referencia, col = "red", lwd = 2, lty = 2)
        legend("topright", legend = "Valor de referencia",
               col = "red", lwd = 2, lty = 2)
      }
    })
    
    #---------------------------------------------------------------------------------------------------------
    # Descarga de resultados
    #---------------------------------------------------------------------------------------------------------
    output$descargar <- downloadHandler(
      filename = function() paste0("montecarlo_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- calculo()
        df <- data.frame(
          estimacion = r$estimacion,
          error_estandar = r$error_est,
          referencia = r$referencia,
          N = r$N
        )
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
