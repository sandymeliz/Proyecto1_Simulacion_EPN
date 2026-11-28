# ==============================================================
# Pestaña 3: Variables Aleatorias Discretas
# Responsable: Erika Haro
# Usa: R/discretas.R, R/generadores_continuas.R, R/00_paleta.R
#      y los estilos compartidos de la Pestaña 1
# ==============================================================
library(plotly)
library(DT)
library(writexl)

DISCRETAS_P3 <- list(
  binom = list(nombre = "Binomial",      icono = "coins",       color = PALETA$petroleo,
               lema = "Éxitos en n ensayos independientes"),
  pois  = list(nombre = "Poisson",       icono = "phone-volume", color = PALETA$naranja,
               lema = "Eventos en un intervalo de tiempo"),
  geom  = list(nombre = "Geométrica",    icono = "rotate-right", color = PALETA$menta,
               lema = "Ensayos hasta el primer éxito"),
  pers  = list(nombre = "Personalizada", icono = "pen-ruler",    color = PALETA$orquidea,
               lema = "Tu propia ley de probabilidad")
)

EJEMPLOS_P3 <- list(
  dado    = list(x = "1, 2, 3, 4, 5, 6",  p = "1/6, 1/6, 1/6, 1/6, 1/6, 1/6"),
  ross    = list(x = "1, 2, 3, 4",        p = "0.20, 0.15, 0.25, 0.40"),
  monedas = list(x = "0, 1, 2, 3",        p = "1/8, 3/8, 3/8, 1/8"),
  ventas  = list(x = "0, 1, 2, 3, 4",     p = "0.10, 0.30, 0.35, 0.15, 0.10")
)

CSS_P3 <- paste0(
  css_tarjetas_opcion("p3-dist", list(binom = PALETA$petroleo, pois = PALETA$naranja,
                                      geom = PALETA$menta, pers = PALETA$orquidea)), "
.p3-cabecera { background: linear-gradient(110deg, ", PALETA$p_rosa, " 0%, #FFE2C8 50%, ", PALETA$p_celeste, " 100%); }
.p3-lista textarea, .p3-lista input { font-family: Consolas, monospace; font-size: 15px !important; }
")


# ==============================================================
# INTERFAZ
# ==============================================================
mod_p3_discretas_ui <- function(id) {
  ns <- NS(id)
  op <- function(k) { d <- DISCRETAS_P3[[k]]; opcion_tarjeta(d$nombre, d$lema, d$icono, d$color) }

  div(class = "p1",
    tags$head(tags$style(HTML(CSS_P1)), tags$style(HTML(CSS_P3))),

    div(class = "p1-cabecera p3-cabecera",
        h2(icon("chart-simple"), " Generación de Variables Aleatorias Discretas"),
        p("Simula variables que toman valores enteros o puntuales usando el ",
          tags$b("Método de la Transformación Inversa Discreta"), " con uniformes de nuestro propio generador.")),

    paso_p1(1, "Elige la distribución"),
    div(class = "p3-dist",
        radioButtons(ns("dist"), NULL, inline = TRUE, width = "100%",
                     choiceNames = list(op("binom"), op("pois"), op("geom"), op("pers")),
                     choiceValues = c("binom", "pois", "geom", "pers"))),
    br(),

    fluidRow(
      column(3,
        div(class = "p1-panel",
          paso_p1(2, "Configura los parámetros"),
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'binom'"),
            fluidRow(column(6, numericInput(ns("b_n"), "Ensayos n", value = 10, min = 1, max = 500, step = 1)),
                     column(6, numericInput(ns("b_p"), "Prob. éxito p", value = 0.3, min = 0.001, max = 0.999, step = 0.05))),
            ayuda_p1("X = número de éxitos en n ensayos, cada uno con probabilidad p de éxito.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'pois'"),
            numericInput(ns("p_lambda"), "Tasa λ", value = 4, min = 0.01, max = 500, step = 0.5),
            ayuda_p1("Promedio de eventos por intervalo (llamadas por hora, clientes por minuto...). Media = varianza = λ.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'geom'"),
            numericInput(ns("g_p"), "Prob. éxito p", value = 0.2, min = 0.001, max = 0.999, step = 0.05),
            ayuda_p1("X = número de ensayos hasta obtener el primer éxito (X = 1, 2, 3, ...). Media = 1/p.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'pers'"),
            selectInput(ns("ejemplo"), "Ejemplos",
                        choices = c("Escribir mi propia ley" = "propia", "Dado justo" = "dado",
                                    "Ejemplo de Ross (cap. 4)" = "ross", "Caras en 3 monedas" = "monedas",
                                    "Ventas diarias" = "ventas"), selected = "ross"),
            div(class = "p3-lista", textInput(ns("x_txt"), "Valores X = {x₁, ..., xₖ}", value = "1, 2, 3, 4")),
            ayuda_p1("Separe con comas. Ejemplo: <code>0, 1, 2, 5</code>"),
            div(class = "p3-lista", textInput(ns("p_txt"), "Probabilidades P(X = xᵢ)", value = "0.20, 0.15, 0.25, 0.40")),
            ayuda_p1("En el mismo orden. Se aceptan fracciones: <code>1/6</code>. <b>Deben sumar 1.</b>"),
            uiOutput(ns("suma_p")),
            actionLink(ns("normalizar"), "Ajustar para que sumen 1", icon = icon("scale-balanced"))
          ),
          hr(style = "margin:10px 0;"),
          checkboxInput(ns("ordenar"), "Buscar primero los valores más probables", value = FALSE),
          ayuda_p1("Mejora de Ross: ordenar por probabilidad reduce el número de comparaciones de la búsqueda."),
          selectInput(ns("fuente"), "Fuente de números uniformes",
                      choices = c("MCL propio (Pestaña 1)" = "mcl", "runif de R (solo referencia)" = "runif")),
          numericInput(ns("semilla"), "Semilla", value = 2026, min = 1),
          numericInput(ns("N"), "Cantidad de valores (N)", value = 2000, min = 10, max = 20000),
          ayuda_p1("Cuántas variables aleatorias se van a generar (10 a 20 000)."),
          uiOutput(ns("boton"))
        ),
        div(class = "p1-panel",
          paso_p1(4, "Descarga los datos", PALETA$menta),
          downloadButton(ns("desc_xlsx"), HTML("<b>Excel</b> (.xlsx)"), class = "p1-desc",
                         style = paste0("background:", PALETA$menta, ";")),
          div(class = "p1-ayuda", style = "margin:2px 0 10px 0;", "Hojas: Datos, Frecuencias y Resumen."),
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
            tabPanel(tagList(icon("chart-column"), " Empírico vs. teórico"),
                     que_veo_p1("Para cada valor x: la <b>barra de color</b> es la proporción observada en la simulación y la <b>barra gris</b> la probabilidad teórica P(X = x). Si la simulación es correcta, las parejas de barras casi coinciden."),
                     plotlyOutput(ns("g_barras"), height = "440px")),
            tabPanel(tagList(icon("stairs"), " Función de distribución"),
                     que_veo_p1("F(x) = P(X ≤ x) es una función <b>escalonada</b>: salta en cada valor posible. Se compara la empírica con la teórica."),
                     plotlyOutput(ns("g_cdf"), height = "420px")),
            tabPanel(tagList(icon("gears"), " ¿Cómo se generó?"),
                     que_veo_p1("Transformación inversa discreta: para cada U se avanza por la escalera F(x) hasta el primer escalón que <b>supera</b> a U. Ese escalón es el valor X. Abajo, los primeros 15 pasos."),
                     plotlyOutput(ns("g_como"), height = "400px"),
                     DTOutput(ns("tabla_pasos"))),
            tabPanel(tagList(icon("clipboard-list"), " Resumen descriptivo"),
                     fluidRow(
                       column(6, h4(icon("calculator"), " Medidas"), tableOutput(ns("resumen"))),
                       column(6, h4(icon("scale-balanced"), " Prueba Chi-cuadrado de ajuste"),
                              selectInput(ns("alfa"), "Significancia α", choices = c(0.01, 0.05, 0.10), selected = 0.05, width = "160px"),
                              uiOutput(ns("chi_texto")), tableOutput(ns("chi_tabla")))
                     ),
                     h4(icon("table-list"), " Tabla de frecuencias"),
                     DTOutput(ns("frecuencias"))),
            tabPanel(tagList(icon("table"), " Tabla de datos"),
                     que_veo_p1("Uniforme usada, número de comparaciones de la búsqueda y valor generado."),
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
mod_p3_discretas_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    estilo_p3 <- function(g) {
      g %>% layout(font = list(family = "Segoe UI, Helvetica, Arial", color = PALETA$noche),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   legend = list(orientation = "h", y = -0.15)) %>%
        config(displaylogo = FALSE, locale = "es")
    }

    # ---------- Ejemplos y ayudas de la personalizada ----------
    observeEvent(input$ejemplo, {
      if (input$ejemplo != "propia") {
        e <- EJEMPLOS_P3[[input$ejemplo]]
        updateTextInput(session, "x_txt", value = e$x)
        updateTextInput(session, "p_txt", value = e$p)
      }
    })

    output$suma_p <- renderUI({
      p <- tryCatch(leer_lista_numeros(input$p_txt, "las probabilidades"), error = function(e) NULL)
      if (is.null(p)) return(NULL)
      ok <- abs(sum(p) - 1) <= 1e-6
      div(style = paste0("font-size:13px; font-weight:600; margin:-4px 0 6px 0; color:", if (ok) PALETA$menta else PALETA$rojo, ";"),
          icon(if (ok) "circle-check" else "circle-xmark"), paste0(" Σpᵢ = ", round(sum(p), 6),
                                                                   if (ok) "  ✓" else "  (debe ser 1)"))
    })

    observeEvent(input$normalizar, {
      p <- tryCatch(leer_lista_numeros(input$p_txt, "las probabilidades"), error = function(e) NULL)
      if (!is.null(p) && sum(p) > 0 && all(p >= 0)) {
        updateTextInput(session, "p_txt", value = paste(round(p / sum(p), 6), collapse = ", "))
      }
    })

    output$boton <- renderUI({
      d <- DISCRETAS_P3[[input$dist]]
      actionButton(session$ns("generar"), "Generar valores", icon = icon("wand-magic-sparkles"),
                   class = "p1-boton", width = "100%",
                   style = paste0("background: linear-gradient(90deg, ", d$color, ", ", PALETA$noche, ");"))
    })

    # ---------- Simulación ----------
    resultado <- eventReactive(input$generar, {
      req(input$N, input$semilla)
      tryCatch({
        if (input$N < 10 || input$N > 20000) stop("N debe estar entre 10 y 20 000.")
        dist <- input$dist
        ley <- switch(dist,
                      binom = fmp_binomial(input$b_n, input$b_p),
                      pois  = fmp_poisson(input$p_lambda),
                      geom  = fmp_geometrica(input$g_p),
                      pers  = fmp_personalizada(input$x_txt, input$p_txt))
        param <- switch(dist,
                        binom = paste0("Binomial(n = ", input$b_n, ", p = ", input$b_p, ")"),
                        pois  = paste0("Poisson(λ = ", input$p_lambda, ")"),
                        geom  = paste0("Geométrica(p = ", input$g_p, ")"),
                        pers  = paste0("Personalizada con ", length(ley$valores), " valores"))
        fuente <- crear_fuente_uniforme(input$semilla, input$fuente)
        r <- transformada_inversa_discreta(ley$valores, ley$probs, input$N, fuente, input$ordenar)
        media <- sum(ley$valores * ley$probs)
        r$ley <- ley
        r$teo <- list(media = media, var = sum(ley$valores^2 * ley$probs) - media^2)
        r$comp_teo <- sum(seq_along(ley$probs) * (if (input$ordenar) sort(ley$probs, decreasing = TRUE) else ley$probs))
        r$dist <- dist; r$param <- param; r$color <- DISCRETAS_P3[[dist]]$color
        r$nombre <- DISCRETAS_P3[[dist]]$nombre
        r
      }, error = function(e) list(error = conditionMessage(e)))
    }, ignoreNULL = FALSE)

    datos <- reactive({
      r <- resultado()
      validate(need(is.null(r$error), r$error))
      r
    })

    # Valores que se muestran en los gráficos (se ocultan colas con probabilidad despreciable)
    visibles <- reactive({
      r <- datos(); v <- r$ley$valores; p <- r$ley$probs
      keep <- p > 1e-5 | v %in% unique(r$x)
      list(valores = v[keep], probs = p[keep])
    })

    # ---------- Descripción ----------
    output$info <- renderUI({
      d <- DISCRETAS_P3[[input$dist]]
      fmp <- switch(input$dist,
                    binom = "P(X = k) = C(n, k) p<sup>k</sup> (1 − p)<sup>n−k</sup>, &nbsp; k = 0, ..., n",
                    pois  = "P(X = k) = e<sup>−λ</sup> λ<sup>k</sup> / k!, &nbsp; k = 0, 1, 2, ...",
                    geom  = "P(X = k) = p (1 − p)<sup>k−1</sup>, &nbsp; k = 1, 2, ...",
                    pers  = "P(X = x<sub>i</sub>) = p<sub>i</sub>, &nbsp; Σ p<sub>i</sub> = 1")
      recursion <- switch(input$dist,
                    binom = "Las probabilidades se calculan con la recursión p(k+1) = (n−k)/(k+1) · p/(1−p) · p(k).",
                    pois  = "Las probabilidades se calculan con la recursión p(k+1) = λ/(k+1) · p(k), sin factoriales grandes.",
                    geom  = "Cada probabilidad es la anterior por (1 − p).",
                    pers  = "Las probabilidades las define el usuario y se valida que sumen 1.")
      div(class = "p1-info",
          style = paste0("background:", transparente(d$color, 0.13), "; border-left:6px solid ", d$color, ";"),
          fluidRow(
            column(7, h4(style = "margin-top:0; font-weight:700;",
                         icon(d$icono, style = paste0("color:", d$color, ";")),
                         paste0(" ¿Cómo funciona? ", d$nombre, " por transformación inversa")),
                   div(style = "font-size:14px; line-height:1.5;",
                       HTML(paste0("Se genera U ~ U(0,1) y se elige el <b>primer valor x<sub>j</sub></b> cuya probabilidad acumulada ",
                                   "F(x<sub>j</sub>) supera a U. Así cada valor sale con su probabilidad exacta, porque ocupa un tramo de ",
                                   "[0, 1) de longitud p<sub>j</sub>. ", recursion)))),
            column(5, div(class = "p1-formula", HTML(fmp)),
                   div(class = "p1-formula", style = "font-size:16px;",
                       HTML("X = x<sub>j</sub> &nbsp; si &nbsp; F(x<sub>j−1</sub>) ≤ U &lt; F(x<sub>j</sub>)")))
          ))
    })

    output$avisos <- renderUI({
      r <- resultado()
      if (!is.null(r$error)) div(class = "alert alert-danger", icon("circle-xmark"), " ", r$error)
    })

    # ---------- Tarjetas ----------
    output$tarjetas <- renderUI({
      r <- datos(); x <- r$x; N <- length(x)
      ok_media <- abs(mean(x) - r$teo$media) <= 2 * sqrt(r$teo$var / N) + 1e-12
      ok_var   <- r$teo$var == 0 || abs(var(x) - r$teo$var) / r$teo$var < 0.1
      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
          tarjeta_p1("list-ol", "VALORES GENERADOS", N, r$param, r$color),
          tarjeta_p1("bullseye", "MEDIA MUESTRAL", round(mean(x), 4), paste0("Teórica ", round(r$teo$media, 4)),
                     if (ok_media) PALETA$menta else PALETA$naranja),
          tarjeta_p1("arrows-left-right", "VARIANZA MUESTRAL", round(var(x), 4), paste0("Teórica ", round(r$teo$var, 4)),
                     if (ok_var) PALETA$menta else PALETA$naranja),
          tarjeta_p1("magnifying-glass", "COMPARACIONES PROMEDIO", round(mean(r$comparaciones), 3),
                     paste0("Teórico Σ j·pⱼ = ", round(r$comp_teo, 3)), PALETA$celeste))
    })

    # ---------- Barras empírico vs teórico ----------
    output$g_barras <- renderPlotly({
      r <- datos(); vis <- visibles()
      emp <- numeric(length(vis$valores))
      for (j in seq_along(vis$valores)) emp[j] <- mean(r$x == vis$valores[j])
      etiquetas <- as.character(vis$valores)
      plot_ly() %>%
        add_bars(x = etiquetas, y = emp, name = "Empírica (simulación)", marker = list(color = r$color),
                 hovertemplate = "x = %{x}<br>Empírica = %{y:.4f}<extra></extra>") %>%
        add_bars(x = etiquetas, y = vis$probs, name = "Teórica P(X = x)", marker = list(color = PALETA$p_lavanda),
                 hovertemplate = "x = %{x}<br>Teórica = %{y:.4f}<extra></extra>") %>%
        layout(barmode = "group", bargap = 0.25, title = paste(r$param),
               xaxis = list(title = "x", categoryorder = "array", categoryarray = etiquetas),
               yaxis = list(title = "Probabilidad")) %>%
        estilo_p3()
    })

    # ---------- CDF ----------
    output$g_cdf <- renderPlotly({
      r <- datos(); vis <- visibles()
      v <- vis$valores
      Fteo <- cumsum(r$ley$probs)[r$ley$valores %in% v]
      Femp <- numeric(length(v))
      for (j in seq_along(v)) Femp[j] <- mean(r$x <= v[j])
      paso <- if (length(v) > 1) min(diff(v)) else 1
      xx <- c(v[1] - paso, v, v[length(v)] + paso)
      plot_ly() %>%
        add_lines(x = xx, y = c(0, Femp, 1), name = "F empírica", line = list(shape = "hv", color = r$color, width = 3)) %>%
        add_lines(x = xx, y = c(0, Fteo, 1), name = "F teórica", line = list(shape = "hv", color = PALETA$rojo, width = 2.5, dash = "dash")) %>%
        add_markers(x = v, y = Fteo, name = "Saltos", marker = list(color = PALETA$rojo, size = 7), showlegend = FALSE) %>%
        layout(xaxis = list(title = "x"), yaxis = list(title = "F(x) = P(X ≤ x)", range = c(0, 1.05))) %>%
        estilo_p3()
    })

    # ---------- ¿Cómo se generó? ----------
    output$g_como <- renderPlotly({
      r <- datos(); vis <- visibles()
      v <- vis$valores
      Fteo <- cumsum(r$ley$probs)[r$ley$valores %in% v]
      paso <- if (length(v) > 1) min(diff(v)) else 1
      xx <- c(v[1] - paso, v, v[length(v)] + paso)
      k <- min(15, length(r$x))
      x0 <- v[1] - paso
      plot_ly() %>%
        add_lines(x = xx, y = c(0, Fteo, 1), name = "F(x)", line = list(shape = "hv", color = r$color, width = 3.5)) %>%
        add_segments(x = rep(x0, k), xend = r$x[1:k], y = r$u[1:k], yend = r$u[1:k], name = "U → escalón",
                     line = list(color = PALETA$celeste, width = 1.5)) %>%
        add_segments(x = r$x[1:k], xend = r$x[1:k], y = r$u[1:k], yend = rep(0, k), name = "X generado",
                     line = list(color = PALETA$rojo, width = 1.5, dash = "dot")) %>%
        add_markers(x = rep(x0, k), y = r$u[1:k], name = "Uniformes U", marker = list(size = 9, color = PALETA$celeste)) %>%
        add_markers(x = r$x[1:k], y = rep(0, k), name = "Valores X", marker = list(size = 11, color = PALETA$rojo)) %>%
        layout(xaxis = list(title = "x"), yaxis = list(title = "U  /  F(x)", range = c(-0.02, 1.05))) %>%
        estilo_p3()
    })

    output$tabla_pasos <- renderDT({
      r <- datos(); k <- min(15, length(r$x))
      Fb <- r$F_busqueda
      intervalo <- character(k)
      for (i in 1:k) {
        j <- r$comparaciones[i]
        intervalo[i] <- paste0("[", round(if (j == 1) 0 else Fb[j - 1], 4), ", ", round(Fb[j], 4), ")")
      }
      t <- data.frame(i = 1:k, U = round(r$u[1:k], 5), Tramo = intervalo,
                      Comparaciones = r$comparaciones[1:k], X = r$x[1:k])
      datatable(t, rownames = FALSE, options = list(dom = "t", pageLength = 15, ordering = FALSE),
                colnames = c("i", "U", "Tramo de F que contiene a U", "Comparaciones", "X generado"))
    })

    # ---------- Resumen descriptivo ----------
    tabla_frec <- reactive({
      r <- datos(); vis <- visibles()
      obs <- numeric(length(vis$valores))
      for (j in seq_along(vis$valores)) obs[j] <- sum(r$x == vis$valores[j])
      data.frame(x = vis$valores, Frecuencia = obs, Prob_empirica = round(obs / length(r$x), 5),
                 Prob_teorica = round(vis$probs, 5), Diferencia = round(obs / length(r$x) - vis$probs, 5))
    })

    output$frecuencias <- renderDT({
      datatable(tabla_frec(), rownames = FALSE, options = list(pageLength = 10, dom = "tp"),
                colnames = c("x", "Frecuencia", "Prob. empírica", "Prob. teórica", "Diferencia"))
    })

    tabla_resumen <- reactive({
      r <- datos(); x <- r$x
      vis <- visibles()
      moda_emp <- as.numeric(names(which.max(table(x))))
      data.frame(Medida = c("Media", "Varianza", "Desviación estándar", "Mediana", "Moda", "Mínimo", "Máximo"),
                 Simulación = c(round(mean(x), 4), round(var(x), 4), round(sd(x), 4), median(x), moda_emp, min(x), max(x)),
                 Teórica = c(round(r$teo$media, 4), round(r$teo$var, 4), round(sqrt(r$teo$var), 4),
                             r$ley$valores[which(cumsum(r$ley$probs) >= 0.5)[1]],
                             r$ley$valores[which.max(r$ley$probs)], min(vis$valores), max(vis$valores)))
    })
    output$resumen <- renderTable({
      t <- tabla_resumen()
      t$Simulación <- format(t$Simulación, drop0trailing = TRUE)
      t$Teórica    <- format(t$Teórica, drop0trailing = TRUE)
      t
    }, striped = TRUE, hover = TRUE, width = "100%", align = "lrr")

    chi <- reactive({
      r <- datos()
      chi_cuadrado_discreta(r$x, r$ley$valores, r$ley$probs, as.numeric(input$alfa))
    })
    output$chi_texto <- renderUI({
      ch <- chi()
      if (is.null(ch)) return(div(class = "alert alert-info", "Se necesitan al menos 2 grupos con frecuencia esperada ≥ 5."))
      div(class = if (ch$rechaza) "alert alert-danger" else "alert alert-success",
          HTML(paste0(formula_p1("χ<sup>2</sup> = ", round(ch$chi2, 4), " &nbsp; vs. &nbsp; χ<sup>2</sup><sub>", ch$gl, "; ",
                                 1 - as.numeric(input$alfa), "</sub> = ", round(ch$critico, 4)),
                      "<b>Valor p ", if (ch$p_valor < 1e-4) "< 0.0001" else paste("=", round(ch$p_valor, 4)), "</b> → ",
                      if (ch$rechaza) "Se <b>rechaza</b> H₀: la muestra no sigue la distribución."
                      else "<b>No se rechaza</b> H₀: la muestra es compatible con la distribución.",
                      "<br><small>Los valores con frecuencia esperada &lt; 5 se agrupan con sus vecinos.</small>")))
    })
    output$chi_tabla <- renderTable({ ch <- chi(); if (!is.null(ch)) ch$tabla }, striped = TRUE, width = "100%")

    # ---------- Tabla de datos ----------
    output$tabla <- renderDT({
      datatable(datos()$tabla, rownames = FALSE, options = list(pageLength = 10)) %>%
        formatRound(columns = "u", digits = 6)
    })

    # ---------- Descargas ----------
    output$desc_xlsx <- downloadHandler(
      filename = function() paste0("discretas_", datos()$dist, ".xlsx"),
      content  = function(file) write_xlsx(list(Datos = datos()$tabla, Frecuencias = tabla_frec(),
                                                Resumen = rbind(data.frame(Medida = "Distribución", Simulación = datos()$param, Teórica = ""),
                                                                tabla_resumen())), file))
    output$desc_csv <- downloadHandler(
      filename = function() paste0("discretas_", datos()$dist, ".csv"),
      content  = function(file) write.csv(datos()$tabla, file, row.names = FALSE))
    output$desc_txt <- downloadHandler(
      filename = function() paste0("discretas_", datos()$dist, ".txt"),
      content  = function(file) write.table(datos()$tabla, file, sep = "\t", row.names = FALSE, quote = FALSE))
  })
}
