# ==============================================================
# Pestaña 3: Variables Aleatorias Discretas
# Responsable: Erika Haro
# ==============================================================

mod_p3_discretas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Configuración"),
          selectInput(ns("dist"), "Distribución:",
                      choices = c("Binomial", "Poisson",
                                  "Geométrica", "Personalizada")),
          conditionalPanel(
            condition = "input.dist == 'Binomial'", ns = ns,
            numericInput(ns("n_bin"), "n (ensayos):", value = 10),
            numericInput(ns("p_bin"), "p (prob. éxito):", value = 0.3, min = 0, max = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Poisson'", ns = ns,
            numericInput(ns("lambda_p"), "λ (media):", value = 4)
          ),
          conditionalPanel(
            condition = "input.dist == 'Geométrica'", ns = ns,
            numericInput(ns("q_geo"), "q (prob. fracaso):", value = 0.37, min = 0, max = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Personalizada'", ns = ns,
            textInput(ns("vals"), "Valores X (separados por coma):",
                      value = "1,2,3,4"),
            textInput(ns("probs"), "Probabilidades P(X):",
                      value = "0.2,0.15,0.25,0.40"),
            verbatimTextOutput(ns("validacion"))
          ),
          numericInput(ns("N"), "Número de simulaciones:", value = 10000),
          actionButton(ns("run"), "Simular", class = "btn-primary"),
          downloadButton(ns("dl_xlsx"), "Descargar .xlsx"),
          downloadButton(ns("dl_txt"),  "Descargar .txt")
        )
      ),
      column(8,
        plotOutput(ns("grafico"), height = "400px"),
        h4("Resumen descriptivo"),
        tableOutput(ns("resumen"))
      )
    )
  )
}

mod_p3_discretas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Validación de la distribución personalizada
    observe({
      if (input$dist == "Personalizada") {
        vals <- as.numeric(strsplit(input$vals, ",")[[1]])
        probs <- as.numeric(strsplit(input$probs, ",")[[1]])
        if (length(vals) != length(probs)) {
          output$validacion <- renderPrint(cat("⚠ X y P deben tener la misma longitud\n"))
        } else if (abs(sum(probs) - 1) > 1e-6) {
          output$validacion <- renderPrint(cat("⚠ La suma de P debe ser 1 (actual:", sum(probs), ")\n"))
        } else {
          output$validacion <- renderPrint(cat("✓ Distribución válida\n"))
        }
      }
    })

    # Simulación
    sim <- eventReactive(input$run, {
      N <- input$N
      req(N > 0)

      if (input$dist == "Binomial") {
        res <- sapply(1:N, function(k) db(input$n_bin, input$p_bin))
        teorica <- data.frame(x = 0:input$n_bin,
                              p = dbinom(0:input$n_bin, input$n_bin, input$p_bin))
      } else if (input$dist == "Poisson") {
        res <- sapply(1:N, function(k) dp(input$lambda_p))
        k_max <- qpois(0.999, input$lambda_p)
        teorica <- data.frame(x = 0:k_max,
                              p = dpois(0:k_max, input$lambda_p))
      } else if (input$dist == "Geométrica") {
        res <- sapply(1:N, function(k) dg(input$q_geo))
        k_max <- qgeom(0.999, 1 - input$q_geo) + 1
        teorica <- data.frame(x = 1:k_max,
                              p = dgeom((1:k_max) - 1, 1 - input$q_geo))
      } else {  # Personalizada
        vals <- as.numeric(strsplit(input$vals, ",")[[1]])
        probs <- as.numeric(strsplit(input$probs, ",")[[1]])
        req(length(vals) == length(probs), abs(sum(probs) - 1) < 1e-6)
        res <- sapply(1:N, function(k) mti(probs, vals))
        teorica <- data.frame(x = vals, p = probs)
      }

      emp <- as.data.frame(table(factor(res, levels = teorica$x)))
      names(emp) <- c("x", "frecuencia")
      emp$p_empirica <- emp$frecuencia / N
      emp$p_teorica <- teorica$p
      list(res = res, tabla = emp)
    })

    # Gráfico de barras comparativo
    output$grafico <- renderPlot({
      s <- sim()
      t <- s$tabla
      mid <- (seq_along(t$x) - 0.5)
      barplot(t$p_empirica, names.arg = t$x, col = "lightblue",
              main = paste("Empírico vs. Teórico –", input$dist),
              xlab = "x", ylab = "P(X = x)", ylim = max(t$p_teorica, t$p_empirica) * 1.2)
      points(mid, t$p_teorica, pch = 19, col = "red", cex = 1.2)
      legend("topright", legend = c("Empírico", "Teórico"),
             fill = c("lightblue", "red"), border = "black")
    })

    # Resumen descriptivo
    output$resumen <- renderTable({
      s <- sim()
      res <- s$res
      data.frame(
        Estadístico = c("Media simulada", "Varianza simulada", "Media teórica",
                        "Varianza teórica"),
        Valor = c(mean(res), var(res),
                  sum(s$tabla$x * s$tabla$p_teorica),
                  sum((s$tabla$x - sum(s$tabla$x * s$tabla$p_teorica))^2 * s$tabla$p_teorica))
      )
    })

    # Descargas
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("discretas_", input$dist, ".xlsx"),
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly = TRUE))
          install.packages("openxlsx")
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Simulación")
        openxlsx::writeData(wb, "Simulación", sim()$tabla)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$dl_txt <- downloadHandler(
      filename = function() paste0("discretas_", input$dist, ".txt"),
      content = function(file) {
        write.table(sim()$tabla, file, sep = "\t", row.names = FALSE)
      }
    )
  })
}
