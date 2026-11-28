# ==============================================================
# Pestaña 2: Integración por el Método de Montecarlo
# Responsable: Erika Haro
# Usa: R/montecarlo.R, R/generadores_continuas.R, R/00_paleta.R
#      y los estilos compartidos de la Pestaña 1
# ==============================================================
library(plotly)
library(DT)
library(writexl)

METODOS_P2 <- list(
  media  = list(nombre = "Media muestral",  icono = "scale-balanced", color = PALETA$petroleo,
                lema = "Promedio de alturas × ancho del intervalo"),
  acierto = list(nombre = "Acierto y error", icono = "bullseye",      color = PALETA$orquidea,
                 lema = "Lanza puntos y cuenta los que caen bajo la curva")
)

EJEMPLOS_P2 <- list(
  ej1 = list(f = "x^2",            a = 0,  b = 1),
  ej2 = list(f = "sin(x)",         a = 0,  b = pi),
  ej3 = list(f = "4/(1+x^2)",      a = 0,  b = 1),
  ej4 = list(f = "sqrt(1-x^2)",    a = 0,  b = 1),
  ej5 = list(f = "exp(-x^2)",      a = -2, b = 2),
  ej6 = list(f = "x^3 - 2*x",      a = -1, b = 2)
)

CSS_P2 <- paste0(
  css_tarjetas_opcion("p2-met", list(media = PALETA$petroleo, acierto = PALETA$orquidea)), "
.p2-cabecera { background: linear-gradient(110deg, ", PALETA$p_celeste, " 0%, ", PALETA$celeste, "88 50%, ",
                 PALETA$p_lavanda, " 100%); }
.p2-input-f input { font-family: Cambria, Georgia, serif; font-size: 18px !important; }
")


# ==============================================================
# INTERFAZ
# ==============================================================
mod_p2_montecarlo_ui <- function(id) {
  ns <- NS(id)

  div(class = "p1",
    tags$head(tags$style(HTML(CSS_P1)), tags$style(HTML(CSS_P2))),

    div(class = "p1-cabecera p2-cabecera",
        h2(icon("chart-area"), " Aproximación de Integrales por Montecarlo"),
        p("Estima el área bajo una curva usando números aleatorios: cuantos más puntos, ",
          "más se acerca la estimación al valor exacto.")),

    paso_p1(1, "Elige el método de Montecarlo"),
    div(class = "p2-met",
        radioButtons(ns("metodo"), NULL, inline = TRUE, width = "100%",
                     choiceNames = list(
                       opcion_tarjeta(METODOS_P2$media$nombre, METODOS_P2$media$lema, METODOS_P2$media$icono, METODOS_P2$media$color),
                       opcion_tarjeta(METODOS_P2$acierto$nombre, METODOS_P2$acierto$lema, METODOS_P2$acierto$icono, METODOS_P2$acierto$color)),
                     choiceValues = c("media", "acierto"))),
    br(),

    fluidRow(
      column(3,
        div(class = "p1-panel",
          paso_p1(2, "Define la integral"),
          selectInput(ns("ejemplo"), "Ejemplos",
                      choices = c("Escribir mi propia función"          = "propia",
                                  "∫₀¹ x² dx = 1/3"                     = "ej1",
                                  "∫₀^π sen(x) dx = 2"                  = "ej2",
                                  "∫₀¹ 4/(1+x²) dx = π"                 = "ej3",
                                  "∫₀¹ √(1−x²) dx = π/4"                = "ej4",
                                  "∫₋₂² e^(−x²) dx ≈ 1.7642"           = "ej5",
                                  "∫₋₁² (x³ − 2x) dx = 0.75  (con área negativa)" = "ej6")),
          div(class = "p2-input-f", textInput(ns("f"), "Función f(x)", value = "x^2")),
          ayuda_p1("Use <b>x</b> como variable. Permitido: + − * / ^ ( ), exp, log, sqrt, sin, cos, tan, abs, pi."),
          fluidRow(column(6, numericInput(ns("a"), "Límite a", value = 0)),
                   column(6, numericInput(ns("b"), "Límite b", value = 1))),
          ayuda_p1("Se calcula ∫ f(x) dx desde a hasta b."),
          numericInput(ns("N"), "Número de simulaciones (N)", value = 5000, min = 10, max = 100000),
          ayuda_p1("Cantidad de puntos aleatorios (10 a 100 000). El error baja como 1/√N."),
          hr(style = "margin:10px 0;"),
          selectInput(ns("fuente"), "Fuente de números uniformes",
                      choices = c("MCL propio (Pestaña 1)" = "mcl", "runif de R (solo referencia)" = "runif")),
          numericInput(ns("semilla"), "Semilla", value = 2026, min = 1),
          ayuda_p1("Con la misma semilla se repiten los mismos puntos."),
          uiOutput(ns("boton"))
        ),
        div(class = "p1-panel",
          paso_p1(4, "Descarga los datos", PALETA$menta),
          downloadButton(ns("desc_xlsx"), HTML("<b>Excel</b> (.xlsx)"), class = "p1-desc",
                         style = paste0("background:", PALETA$menta, ";")),
          div(class = "p1-ayuda", style = "margin:2px 0 10px 0;", "Hojas: Puntos, Resumen y Comparación."),
          downloadButton(ns("desc_csv"), HTML("<b>CSV</b> (.csv)"), class = "p1-desc",
                         style = paste0("background:", PALETA$petroleo, ";")),
          downloadButton(ns("desc_txt"), HTML("<b>Texto</b> (.txt)"), class = "p1-desc",
                         style = paste0("background:", PALETA$indigo, ";"))
        )
      ),

      column(9,
        uiOutput(ns("info")),
        uiOutput(ns("avisos")),
        uiOutput(ns("tarjetas")),
        br(),
        div(class = "p1-panel",
          paso_p1(3, "Explora los resultados"),
          tabsetPanel(
            tabPanel(tagList(icon("shapes"), " Gráfico geométrico"),
                     uiOutput(ns("geo_texto")),
                     plotlyOutput(ns("g_geo"), height = "460px")),
            tabPanel(tagList(icon("chart-line"), " Convergencia"),
                     que_veo_p1("Evolución de la estimación al aumentar el número de simulaciones. La <b>banda</b> es el intervalo de confianza del 95% (± 1.96 · error estándar) y la <b>línea punteada</b> el valor de referencia. El eje horizontal está en escala logarítmica."),
                     plotlyOutput(ns("g_conv"), height = "440px")),
            tabPanel(tagList(icon("arrow-trend-down"), " Precisión del método"),
                     que_veo_p1("Error absoluto |estimación − referencia| frente a N, en escala log-log. La <b>línea punteada</b> es la ley teórica σ/√N: para reducir el error a la mitad se necesitan <b>4 veces más</b> simulaciones."),
                     plotlyOutput(ns("g_error"), height = "420px")),
            tabPanel(tagList(icon("code-compare"), " Comparar métodos"),
                     que_veo_p1("Se ejecutan los dos métodos con la misma semilla y el mismo N. Gana el de <b>menor error estándar</b>: necesita menos simulaciones para la misma precisión."),
                     DTOutput(ns("comp_tabla")), br(),
                     plotlyOutput(ns("g_comp"), height = "380px")),
            tabPanel(tagList(icon("table"), " Tabla de puntos"),
                     que_veo_p1("Primeros puntos simulados y la estimación acumulada."),
                     DTOutput(ns("tabla")))
          )
        )
      )
    )
  )
}


# ==============================================================
# SERVIDOR
# ==============================================================
mod_p2_montecarlo_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    estilo_p2 <- function(g) {
      g %>% layout(font = list(family = "Segoe UI, Helvetica, Arial", color = PALETA$noche),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   legend = list(orientation = "h", y = -0.15)) %>%
        config(displaylogo = FALSE, locale = "es")
    }

    observeEvent(input$ejemplo, {
      if (input$ejemplo != "propia") {
        e <- EJEMPLOS_P2[[input$ejemplo]]
        updateTextInput(session, "f", value = e$f)
        updateNumericInput(session, "a", value = round(e$a, 6))
        updateNumericInput(session, "b", value = round(e$b, 6))
      }
    })

    output$boton <- renderUI({
      m <- METODOS_P2[[input$metodo]]
      actionButton(session$ns("generar"), "Calcular integral", icon = icon("wand-magic-sparkles"),
                   class = "p1-boton", width = "100%",
                   style = paste0("background: linear-gradient(90deg, ", m$color, ", ", PALETA$noche, ");"))
    })

    # ---------- Cálculo ----------
    resultado <- eventReactive(input$generar, {
      req(input$N, input$semilla)
      tryCatch({
        if (input$N < 10 || input$N > 100000) stop("N debe estar entre 10 y 100 000.")
        if (!(input$b > input$a)) stop("El límite b debe ser mayor que a.")
        f <- crear_funcion_usuario(input$f)
        prueba <- f(seq(input$a, input$b, length.out = 2001))
        if (any(!is.finite(prueba))) stop("f(x) no está definida en todo [a, b] (da infinito o NaN).")

        ref <- integral_referencia(f, input$a, input$b)

        # los dos métodos con la misma semilla (para la comparación)
        media   <- mc_media_muestral(f, input$a, input$b, input$N, crear_fuente_uniforme(input$semilla, input$fuente))
        acierto <- mc_acierto_error(f, input$a, input$b, input$N, crear_fuente_uniforme(input$semilla, input$fuente))
        r <- if (input$metodo == "media") media else acierto

        list(r = r, media = media, acierto = acierto, ref = ref, f = f,
             a = input$a, b = input$b, f_txt = input$f, clave = input$metodo,
             color = METODOS_P2[[input$metodo]]$color, conv = convergencia_mc(r$z))
      }, error = function(e) list(error = conditionMessage(e)))
    }, ignoreNULL = FALSE)

    datos <- reactive({
      d <- resultado()
      validate(need(is.null(d$error), d$error))
      d
    })

    # ---------- Descripción ----------
    output$info <- renderUI({
      m <- METODOS_P2[[input$metodo]]
      if (input$metodo == "media") {
        idea <- "Se eligen N puntos X<sub>i</sub> al azar en [a, b] y se calcula la <b>altura promedio</b> de la función. El área es ese promedio por el ancho (b − a): es como cambiar la curva por un <b>rectángulo de igual área</b>."
        formula <- "Î = (b − a) · (1/N) Σ f(X<sub>i</sub>), &nbsp; X<sub>i</sub> = a + (b − a)U<sub>i</sub>"
      } else {
        idea <- "Se encierra la curva en un rectángulo y se lanzan N puntos al azar dentro de él. La fracción de puntos que cae <b>entre la curva y el eje x</b> multiplicada por el área del rectángulo aproxima la integral. Si f es negativa, esos puntos restan."
        formula <- "Î = Área<sub>rect</sub> · (aciertos<sub>+</sub> − aciertos<sub>−</sub>) / N"
      }
      div(class = "p1-info",
          style = paste0("background:", transparente(m$color, 0.13), "; border-left:6px solid ", m$color, ";"),
          fluidRow(
            column(7, h4(style = "margin-top:0; font-weight:700;",
                         icon(m$icono, style = paste0("color:", m$color, ";")), paste0(" ¿Cómo funciona? ", m$nombre)),
                   div(style = "font-size:14px; line-height:1.5;", HTML(idea))),
            column(5, div(class = "p1-formula", HTML(formula)),
                   div(class = "p1-chip", icon("circle-info", style = paste0("color:", PALETA$petroleo, ";")),
                       " Error estándar: EE = s / √N"))
          ))
    })

    output$avisos <- renderUI({
      d <- resultado()
      if (!is.null(d$error)) div(class = "alert alert-danger", icon("circle-xmark"), " ", d$error)
      else if (is.na(d$ref$valor)) div(class = "alert alert-warning", icon("triangle-exclamation"),
                                        " No se pudo calcular la integral de referencia (la integral podría ser divergente).")
    })

    # ---------- Tarjetas ----------
    output$tarjetas <- renderUI({
      d <- datos(); r <- d$r
      ref <- d$ref$valor
      err_abs <- abs(r$estimacion - ref)
      err_rel <- if (!is.na(ref) && ref != 0) 100 * err_abs / abs(ref) else NA
      dentro  <- !is.na(ref) && abs(r$estimacion - ref) <= 1.96 * r$ee
      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
          tarjeta_p1("calculator", "ESTIMACIÓN MONTECARLO", round(r$estimacion, 6), paste("con N =", length(r$z)), d$color),
          tarjeta_p1("plus-minus", "ERROR ESTÁNDAR", round(r$ee, 6),
                     paste0("IC 95%: [", round(r$estimacion - 1.96 * r$ee, 4), ", ", round(r$estimacion + 1.96 * r$ee, 4), "]"),
                     PALETA$celeste),
          tarjeta_p1("ruler", "REFERENCIA NUMÉRICA", if (is.na(ref)) "—" else round(ref, 6),
                     "integrate() de R (cuadratura)", PALETA$indigo),
          tarjeta_p1(if (dentro) "circle-check" else "triangle-exclamation", "ERROR ABSOLUTO",
                     if (is.na(ref)) "—" else num_p1(signif(err_abs, 4)),
                     if (is.na(err_rel)) "" else paste0(round(err_rel, 3), "% · ", if (dentro) "dentro del IC 95%" else "fuera del IC 95%"),
                     if (dentro) PALETA$menta else PALETA$naranja))
    })

    # ---------- Gráfico geométrico ----------
    output$geo_texto <- renderUI({
      d <- datos()
      if (d$clave == "media") {
        que_veo_p1(paste0("La <b>curva</b> es f(x). Los puntos marcan los valores f(X<sub>i</sub>) evaluados en posiciones aleatorias. ",
                          "El <b>rectángulo sombreado</b> tiene altura igual al promedio de esas alturas (",
                          round(mean(d$r$fx), 4), "): su área es la estimación de la integral."))
      } else {
        que_veo_p1(paste0("Puntos lanzados al rectángulo de área ", round(d$r$area, 4), ". <b style='color:", PALETA$menta,
                          "'>Verdes</b>: aciertos sobre el eje (suman). <b style='color:", PALETA$amarillo,
                          "'>Amarillos</b>: aciertos bajo el eje (restan). <b style='color:", PALETA$rojo,
                          "'>Rojos</b>: fallos. Se muestran hasta 4000 puntos."))
      }
    })

    output$g_geo <- renderPlotly({
      d <- datos(); r <- d$r
      xs <- seq(d$a, d$b, length.out = 500); fx <- d$f(xs)
      g <- plot_ly()
      if (d$clave == "media") {
        h <- mean(r$fx)
        ver <- 1:min(length(r$x), 1500)
        g <- g %>%
          add_trace(x = c(d$a, d$b, d$b, d$a, d$a), y = c(0, 0, h, h, 0), type = "scatter", mode = "lines",
                    fill = "toself", fillcolor = transparente(PALETA$petroleo, 0.15),
                    line = list(color = PALETA$petroleo, dash = "dash"), name = "Rectángulo de altura promedio") %>%
          add_trace(x = xs, y = fx, type = "scatter", mode = "lines", fill = "tozeroy",
                    fillcolor = transparente(PALETA$amarillo, 0.18),
                    line = list(color = PALETA$noche, width = 3), name = "f(x)") %>%
          add_markers(x = r$x[ver], y = r$fx[ver], name = "f(Xᵢ)",
                      marker = list(size = 5, color = transparente(PALETA$orquidea, 0.6))) %>%
          add_markers(x = r$x[ver], y = rep(0, length(ver)), name = "Xᵢ aleatorios",
                      marker = list(size = 6, symbol = "line-ns-open", color = PALETA$petroleo))
      } else {
        ver <- 1:min(length(r$x), 4000)
        sg <- r$signo[ver]
        g <- g %>%
          add_trace(x = c(d$a, d$b, d$b, d$a, d$a), y = c(r$y_min, r$y_min, r$y_max, r$y_max, r$y_min),
                    type = "scatter", mode = "lines", line = list(color = PALETA$noche, dash = "dash"), name = "Rectángulo")
        grupos <- list(list(s = 0, n = "Fallo", col = transparente(PALETA$rojo, 0.45)),
                       list(s = 1, n = "Acierto (+)", col = transparente(PALETA$menta, 0.7)),
                       list(s = -1, n = "Acierto (−)", col = transparente(PALETA$amarillo, 0.85)))
        for (gr in grupos) {
          sel <- sg == gr$s
          if (any(sel)) g <- g %>% add_markers(x = r$x[ver][sel], y = r$y[ver][sel], name = gr$n,
                                               marker = list(size = 4, color = gr$col))
        }
        g <- g %>%
          add_lines(x = xs, y = fx, name = "f(x)", line = list(color = PALETA$noche, width = 3.5))
      }
      g %>% layout(xaxis = list(title = "x"), yaxis = list(title = "f(x)")) %>% estilo_p2()
    })

    # ---------- Convergencia ----------
    output$g_conv <- renderPlotly({
      d <- datos(); cv <- d$conv
      g <- plot_ly() %>%
        add_trace(x = c(cv$n, rev(cv$n)), y = c(cv$superior, rev(cv$inferior)), type = "scatter", mode = "lines",
                  fill = "toself", fillcolor = transparente(d$color, 0.18), line = list(width = 0),
                  name = "IC 95%", hoverinfo = "skip") %>%
        add_lines(x = cv$n, y = cv$estimacion, name = "Estimación", line = list(color = d$color, width = 2.5))
      if (!is.na(d$ref$valor)) {
        g <- g %>% add_lines(x = range(cv$n), y = rep(d$ref$valor, 2), name = "Referencia",
                             line = list(color = PALETA$rojo, width = 2.5, dash = "dash"))
      }
      rango_y <- quantile(cv$estimacion[cv$n >= 30], c(0, 1), na.rm = TRUE)
      g %>% layout(xaxis = list(title = "Número de simulaciones N", type = "log"),
                   yaxis = list(title = "Estimación de la integral",
                                range = c(rango_y[1] - 0.3 * diff(rango_y) - 1e-9, rango_y[2] + 0.3 * diff(rango_y) + 1e-9))) %>%
        estilo_p2()
    })

    # ---------- Precisión ----------
    output$g_error <- renderPlotly({
      d <- datos(); cv <- d$conv
      validate(need(!is.na(d$ref$valor), "Se necesita el valor de referencia."))
      err <- pmax(abs(cv$estimacion - d$ref$valor), 1e-12)
      s <- sd(d$r$z)
      plot_ly() %>%
        add_lines(x = cv$n, y = err, name = "Error absoluto", line = list(color = d$color, width = 2)) %>%
        add_lines(x = cv$n, y = s / sqrt(cv$n), name = "σ / √N (teórico)",
                  line = list(color = PALETA$rojo, width = 2.5, dash = "dash")) %>%
        layout(xaxis = list(title = "N", type = "log"), yaxis = list(title = "Error", type = "log")) %>%
        estilo_p2()
    })

    # ---------- Comparar métodos ----------
    tabla_comp <- reactive({
      d <- datos(); ref <- d$ref$valor
      filas <- list()
      for (m in list(d$media, d$acierto)) {
        filas[[m$metodo]] <- data.frame(
          Método = m$metodo, Estimación = round(m$estimacion, 6), Error_estándar = round(m$ee, 6),
          IC_95 = paste0("[", round(m$estimacion - 1.96 * m$ee, 4), ", ", round(m$estimacion + 1.96 * m$ee, 4), "]"),
          Error_absoluto = if (is.na(ref)) NA else signif(abs(m$estimacion - ref), 4),
          Uniformes_usadas = m$usadas)
      }
      t <- do.call(rbind, filas)
      t$Eficiencia_relativa <- round(min(t$Error_estándar)^2 / t$Error_estándar^2, 3)
      t
    })

    output$comp_tabla <- renderDT({
      datatable(tabla_comp(), rownames = FALSE, options = list(dom = "t", ordering = FALSE),
                colnames = c("Método", "Estimación", "Error estándar", "IC 95%", "Error absoluto",
                             "Uniformes usadas", "Eficiencia relativa"))
    })

    output$g_comp <- renderPlotly({
      d <- datos()
      cm <- convergencia_mc(d$media$z); ca <- convergencia_mc(d$acierto$z)
      g <- plot_ly() %>%
        add_lines(x = cm$n, y = cm$estimacion, name = "Media muestral", line = list(color = PALETA$petroleo, width = 2.5)) %>%
        add_lines(x = ca$n, y = ca$estimacion, name = "Acierto y error", line = list(color = PALETA$orquidea, width = 2.5))
      if (!is.na(d$ref$valor)) g <- g %>% add_lines(x = range(cm$n), y = rep(d$ref$valor, 2), name = "Referencia",
                                                    line = list(color = PALETA$rojo, dash = "dash", width = 2.5))
      rango_y <- quantile(c(cm$estimacion[cm$n >= 30], ca$estimacion[ca$n >= 30]), c(0, 1))
      g %>% layout(xaxis = list(title = "N", type = "log"),
                   yaxis = list(title = "Estimación", range = c(rango_y[1] - 0.2 * diff(rango_y) - 1e-9, rango_y[2] + 0.2 * diff(rango_y) + 1e-9))) %>%
        estilo_p2()
    })

    # ---------- Tabla ----------
    output$tabla <- renderDT({
      t <- datos()$r$tabla
      datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatRound(columns = which(sapply(t, is.numeric))[-1], digits = 5)
    })

    # ---------- Descargas ----------
    resumen <- reactive({
      d <- datos(); r <- d$r
      data.frame(Medida = c("Integral", "Método", "N", "Estimación", "Error estándar", "IC 95% inferior",
                            "IC 95% superior", "Referencia (integrate)", "Error absoluto"),
                 Valor = c(paste0("∫ ", d$f_txt, " dx en [", d$a, ", ", round(d$b, 6), "]"), r$metodo, length(r$z),
                           round(r$estimacion, 8), round(r$ee, 8), round(r$estimacion - 1.96 * r$ee, 8),
                           round(r$estimacion + 1.96 * r$ee, 8), round(d$ref$valor, 8),
                           signif(abs(r$estimacion - d$ref$valor), 6)))
    })
    output$desc_xlsx <- downloadHandler(
      filename = function() "montecarlo.xlsx",
      content  = function(file) write_xlsx(list(Puntos = datos()$r$tabla, Resumen = resumen(), Comparacion = tabla_comp()), file))
    output$desc_csv <- downloadHandler(
      filename = function() "montecarlo.csv",
      content  = function(file) write.csv(datos()$r$tabla, file, row.names = FALSE))
    output$desc_txt <- downloadHandler(
      filename = function() "montecarlo.txt",
      content  = function(file) write.table(datos()$r$tabla, file, sep = "\t", row.names = FALSE, quote = FALSE))
  })
}

