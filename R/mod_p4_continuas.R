# ==============================================================
# Pestaña 4: Generación de Variables Aleatorias Continuas
# Responsable: Sandy
# Usa: R/00_paleta.R               (colores)
#      R/generadores_continuas.R   (algoritmos de simulación)
#      R/mod_p1_pseudoaleatorios.R (estilos y tarjetas compartidas: tarjeta_p1, ayuda_p1, ...)
# ==============================================================

library(plotly)
library(DT)
library(writexl)


# --------------------------------------------------------------
# Identidad de cada distribución
# --------------------------------------------------------------
DISTRIBUCIONES_P4 <- list(
  unif = list(nombre = "Uniforme",     icono = "grip-lines",     color = PALETA$menta,
              lema = "Todos los valores del intervalo son igual de probables"),
  exp  = list(nombre = "Exponencial",  icono = "hourglass-half", color = PALETA$durazno,
              lema = "Tiempo de espera hasta el próximo evento"),
  norm = list(nombre = "Normal",       icono = "bell",           color = PALETA$indigo,
              lema = "La campana de Gauss"),
  pers = list(nombre = "Personalizada", icono = "pen-ruler",     color = PALETA$orquidea,
              lema = "Escribe tu propia densidad f(x)")
)

# Ejemplos de densidades para la opción personalizada
EJEMPLOS_P4 <- list(
  ej1 = list(f = "3*x^2",            a = 0, b = 1),
  ej2 = list(f = "6*x*(1-x)",        a = 0, b = 1),
  ej3 = list(f = "0.5*sin(x)",       a = 0, b = pi),
  ej4 = list(f = "x/2",              a = 0, b = 2),
  ej5 = list(f = "exp(-x)",          a = 0, b = 10)
)

CSS_P4 <- paste0("
.p4-dist .shiny-options-group { display: flex; gap: 14px; }
.p4-dist .radio-inline { flex: 1; margin: 0 !important; padding: 0 !important; }
.p4-dist input[type=radio] { display: none; }
.p4-dist .radio-inline > span { display: block; }
.p4-dist label:has(input:checked) .p1-op { box-shadow: 0 8px 18px rgba(53,56,125,.18); transform: translateY(-2px); }
.p4-dist label:has(input[value=unif]:checked) .p1-op { border-color: ", PALETA$menta,    "; background: ", PALETA$menta,    "22; }
.p4-dist label:has(input[value=exp]:checked)  .p1-op { border-color: ", PALETA$durazno,  "; background: ", PALETA$durazno,  "22; }
.p4-dist label:has(input[value=norm]:checked) .p1-op { border-color: ", PALETA$indigo,   "; background: ", PALETA$indigo,   "22; }
.p4-dist label:has(input[value=pers]:checked) .p1-op { border-color: ", PALETA$orquidea, "; background: ", PALETA$orquidea, "22; }
.p4-cabecera { background: linear-gradient(110deg, ", PALETA$p_rosa, " 0%, ", PALETA$p_lila, " 55%, ",
                 PALETA$p_lavanda, " 100%); }
.p4-input-f input { font-family: Cambria, Georgia, serif; font-size: 18px !important; }
")

# Opción del selector de distribuciones
opcion_dist_p4 <- function(clave) {
  d <- DISTRIBUCIONES_P4[[clave]]
  div(class = "p1-op",
      div(class = "p1-ico", style = paste0("background:", d$color, ";"), icon(d$icono)),
      div(div(class = "p1-tit", d$nombre), div(class = "p1-lema", d$lema)))
}


# ==============================================================
# INTERFAZ (UI)
# ==============================================================
mod_p4_continuas_ui <- function(id) {
  ns <- NS(id)

  div(class = "p1",
    tags$head(tags$style(HTML(CSS_P1)), tags$style(HTML(CSS_P4))),

    # ---------------- Cabecera ----------------
    div(class = "p1-cabecera p4-cabecera",
        h2(icon("wave-square"), " Generación de Variables Aleatorias Continuas"),
        p("Transforma números uniformes U(0,1), producidos por nuestro propio generador congruencial, ",
          "en variables con la distribución que elijas.")),

    # ---------------- Paso 1 ----------------
    paso_p1(1, "Elige la distribución"),
    div(class = "p4-dist",
        radioButtons(ns("dist"), NULL, inline = TRUE, width = "100%",
                     choiceNames  = list(opcion_dist_p4("unif"), opcion_dist_p4("exp"),
                                         opcion_dist_p4("norm"), opcion_dist_p4("pers")),
                     choiceValues = c("unif", "exp", "norm", "pers"))),
    br(),

    fluidRow(
      # ---------------- Paso 2: parámetros ----------------
      column(3,
        div(class = "p1-panel",
          paso_p1(2, "Configura los parámetros"),

          # Uniforme
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'unif'"),
            fluidRow(column(6, numericInput(ns("u_a"), "Límite a", value = 0)),
                     column(6, numericInput(ns("u_b"), "Límite b", value = 1))),
            ayuda_p1("Intervalo [a, b] donde caen los valores (b debe ser mayor que a).")
          ),
          # Exponencial
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'exp'"),
            numericInput(ns("e_lambda"), "Tasa λ", value = 1, min = 0.001, step = 0.1),
            ayuda_p1("Número promedio de eventos por unidad de tiempo. La media es 1/λ.")
          ),
          # Normal
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'norm'"),
            fluidRow(column(6, numericInput(ns("n_mu"), "Media μ", value = 0)),
                     column(6, numericInput(ns("n_sigma"), "Desv. σ", value = 1, min = 0.001, step = 0.1))),
            ayuda_p1("μ es el centro de la campana y σ qué tan ancha es."),
            radioButtons(ns("n_metodo"), "Método de generación",
                         choices = c("Box-Muller" = "bm", "Aceptación y Rechazo (Ross)" = "ar")),
            ayuda_p1("Box-Muller usa 2 uniformes por cada 2 normales. Aceptación y Rechazo descarta algunos candidatos.")
          ),
          # Personalizada
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'pers'"),
            selectInput(ns("p_ejemplo"), "Ejemplos",
                        choices = c("Escribir mi propia función" = "propia",
                                    "f(x) = 3x²  en [0, 1]"           = "ej1",
                                    "f(x) = 6x(1−x)  en [0, 1]  (Beta)" = "ej2",
                                    "f(x) = ½ sen(x)  en [0, π]"      = "ej3",
                                    "f(x) = x/2  en [0, 2]"           = "ej4",
                                    "f(x) = e^(−x)  en [0, 10]"       = "ej5")),
            div(class = "p4-input-f", textInput(ns("p_f"), "Densidad f(x)", value = "3*x^2")),
            ayuda_p1("Use <b>x</b> como variable. Ejemplos: <code>3*x^2</code>, <code>exp(-x)</code>, <code>0.5*sin(x)</code>."),
            fluidRow(column(6, numericInput(ns("p_a"), "Desde a", value = 0)),
                     column(6, numericInput(ns("p_b"), "Hasta b", value = 1))),
            ayuda_p1("Intervalo donde f(x) es distinta de 0. Si el área no es 1, se normaliza automáticamente."),
            radioButtons(ns("p_metodo"), "Método de generación",
                         choices = c("Transformación inversa" = "ti", "Aceptación y Rechazo" = "ar")),
            ayuda_p1("La inversa resuelve F(x) = U. Aceptación y Rechazo lanza puntos bajo la curva.")
          ),

          hr(style = "margin:10px 0;"),
          selectInput(ns("fuente"), "Fuente de números uniformes",
                      choices = c("MCL propio (Pestaña 1)" = "mcl", "runif de R (solo referencia)" = "runif")),
          ayuda_p1("Por defecto se usa nuestro generador congruencial: x(n+1) = (1664525·x + 1013904223) mod 2³²."),
          numericInput(ns("semilla"), "Semilla", value = 2026, min = 1),
          ayuda_p1("Con la misma semilla se repiten exactamente los mismos resultados."),
          numericInput(ns("N"), "Cantidad de valores (N)", value = 2000, min = 10, max = 20000),
          ayuda_p1("Cuántas variables aleatorias se van a generar (10 a 20 000)."),
          sliderInput(ns("bins"), "Barras del histograma", min = 10, max = 80, value = 35),

          uiOutput(ns("boton"))
        ),

        # ---------------- Paso 4: descargas ----------------
        div(class = "p1-panel",
          paso_p1(4, "Descarga los datos", PALETA$menta),
          downloadButton(ns("desc_xlsx"), HTML("<b>Excel</b> (.xlsx)"), class = "p1-desc",
                         style = paste0("background:", PALETA$menta, ";")),
          div(class = "p1-ayuda", style = "margin:2px 0 10px 0;", "Hojas: Datos y Resumen."),
          downloadButton(ns("desc_csv"), HTML("<b>CSV</b> (.csv)"), class = "p1-desc",
                         style = paste0("background:", PALETA$petroleo, ";")),
          downloadButton(ns("desc_txt"), HTML("<b>Texto</b> (.txt)"), class = "p1-desc",
                         style = paste0("background:", PALETA$indigo, ";"))
        )
      ),

      # ---------------- Paso 3: resultados ----------------
      column(9,
        uiOutput(ns("info")),
        uiOutput(ns("avisos")),
        uiOutput(ns("tarjetas")),
        br(),
        div(class = "p1-panel",
          paso_p1(3, "Explora los resultados"),
          tabsetPanel(
            tabPanel(tagList(icon("chart-area"), " Densidad"),
                     que_veo_p1("Histograma de los valores generados con la <b>densidad teórica</b> (línea punteada) y la <b>densidad empírica</b> estimada de los datos (línea continua). Si la simulación es correcta, ambas curvas casi coinciden."),
                     plotlyOutput(ns("g_densidad"), height = "430px")),
            tabPanel(tagList(icon("stairs"), " Función de distribución"),
                     que_veo_p1("Compara la <b>CDF empírica</b> F<sub>n</sub>(x) = proporción de datos ≤ x (escalones) con la <b>CDF teórica</b> F(x)."),
                     uiOutput(ns("ks_texto")),
                     plotlyOutput(ns("g_cdf"), height = "420px")),
            tabPanel(tagList(icon("chart-line"), " Gráfico Q-Q"),
                     que_veo_p1("Cada punto compara un cuantil de los datos con el cuantil teórico. Si los puntos siguen la <b>diagonal</b>, los datos tienen la distribución elegida."),
                     plotlyOutput(ns("g_qq"), height = "430px")),
            tabPanel(tagList(icon("gears"), " ¿Cómo se generó?"),
                     uiOutput(ns("como_texto")),
                     plotlyOutput(ns("g_como"), height = "460px")),
            tabPanel(tagList(icon("table"), " Tabla de datos"),
                     que_veo_p1("Uniformes usadas y valor obtenido en cada simulación."),
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
mod_p4_continuas_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    estilo_p4 <- function(g) {
      g %>% layout(font = list(family = "Segoe UI, Helvetica, Arial", color = PALETA$noche),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   legend = list(orientation = "h", y = -0.15)) %>%
        config(displaylogo = FALSE, locale = "es")
    }

    # ---------- 1. Ejemplos de densidad personalizada ----------
    observeEvent(input$p_ejemplo, {
      if (input$p_ejemplo != "propia") {
        e <- EJEMPLOS_P4[[input$p_ejemplo]]
        updateTextInput(session, "p_f", value = e$f)
        updateNumericInput(session, "p_a", value = round(e$a, 6))
        updateNumericInput(session, "p_b", value = round(e$b, 6))
      }
    })

    # ---------- 2. Botón ----------
    output$boton <- renderUI({
      d <- DISTRIBUCIONES_P4[[input$dist]]
      actionButton(session$ns("generar"), "Generar valores", icon = icon("wand-magic-sparkles"),
                   class = "p1-boton", width = "100%",
                   style = paste0("background: linear-gradient(90deg, ", d$color, ", ", PALETA$noche, ");"))
    })

    # ---------- 3. Simulación ----------
    resultado <- eventReactive(input$generar, {
      req(input$N, input$semilla)
      tryCatch({
        if (input$N < 10 || input$N > 20000) stop("N debe estar entre 10 y 20 000.")
        fuente <- crear_fuente_uniforme(input$semilla, input$fuente)
        dist <- input$dist

        if (dist == "unif") {
          a <- input$u_a; b <- input$u_b
          r <- sim_uniforme(input$N, a, b, fuente)
          r$metodo <- "Transformación inversa"
          r$teo <- list(dens = function(x) ifelse(x >= a & x <= b, 1 / (b - a), 0),
                        cdf  = function(x) pmin(pmax((x - a) / (b - a), 0), 1),
                        cuantil = function(p) a + (b - a) * p,
                        media = (a + b) / 2, var = (b - a)^2 / 12,
                        rango = c(a - 0.05 * (b - a), b + 0.05 * (b - a)))
          r$param_txt <- paste0("U(", a, ", ", b, ")")

        } else if (dist == "exp") {
          l <- input$e_lambda
          r <- sim_exponencial(input$N, l, fuente)
          r$metodo <- "Transformación inversa"
          r$teo <- list(dens = function(x) ifelse(x >= 0, l * exp(-l * x), 0),
                        cdf  = function(x) ifelse(x >= 0, 1 - exp(-l * x), 0),
                        cuantil = function(p) -log(1 - p) / l,
                        media = 1 / l, var = 1 / l^2,
                        rango = c(0, -log(0.001) / l))
          r$param_txt <- paste0("Exp(λ = ", l, ")")

        } else if (dist == "norm") {
          mu <- input$n_mu; s <- input$n_sigma
          r <- if (input$n_metodo == "bm") sim_normal_box_muller(input$N, mu, s, fuente)
               else sim_normal_rechazo(input$N, mu, s, fuente)
          r$metodo <- if (input$n_metodo == "bm") "Box-Muller" else "Aceptación y Rechazo (Ross)"
          r$teo <- list(dens = function(x) exp(-(x - mu)^2 / (2 * s^2)) / (s * sqrt(2 * pi)),
                        cdf  = function(x) pnorm(x, mu, s),
                        cuantil = function(p) qnorm(p, mu, s),
                        media = mu, var = s^2,
                        rango = c(mu - 4 * s, mu + 4 * s))
          r$param_txt <- paste0("N(μ = ", mu, ", σ = ", s, ")")

        } else {
          f <- crear_funcion_usuario(input$p_f)
          a <- input$p_a; b <- input$p_b
          r <- if (input$p_metodo == "ti") sim_personalizada_inversa(input$N, f, a, b, fuente)
               else sim_personalizada_rechazo(input$N, f, a, b, fuente)
          r$metodo <- if (input$p_metodo == "ti") "Transformación inversa (numérica)" else "Aceptación y Rechazo"
          d <- r$densidad
          media <- trapecio(d$xs, d$xs * d$fx)
          r$teo <- list(dens = d$f_norm,
                        cdf  = function(x) ifelse(x <= a, 0, ifelse(x >= b, 1, interpolar(d$xs, d$Fx, x))),
                        cuantil = function(p) interpolar(d$Fx, d$xs, p),
                        media = media, var = trapecio(d$xs, d$xs^2 * d$fx) - media^2,
                        rango = c(a, b))
          r$param_txt <- paste0("f(x) = ", input$p_f, " en [", a, ", ", round(b, 4), "]")
        }
        r$dist <- dist
        r$nombre <- DISTRIBUCIONES_P4[[dist]]$nombre
        r$color  <- DISTRIBUCIONES_P4[[dist]]$color
        r$fuente <- input$fuente
        r
      }, error = function(e) list(error = conditionMessage(e)))
    }, ignoreNULL = FALSE)

    datos <- reactive({
      r <- resultado()
      validate(need(is.null(r$error), r$error))
      r
    })

    # ---------- 4. Descripción del método ----------
    output$info <- renderUI({
      dist <- input$dist
      d <- DISTRIBUCIONES_P4[[dist]]
      if (dist == "unif") {
        titulo <- "Uniforme por transformación inversa"
        idea <- "Si U es uniforme en (0, 1), al <b>estirarla</b> por (b − a) y <b>desplazarla</b> a partir de a se obtiene una uniforme en [a, b]. Es despejar x en F(x) = (x − a)/(b − a) = U."
        formula <- "X = a + (b − a) · U"
      } else if (dist == "exp") {
        titulo <- "Exponencial por transformación inversa"
        idea <- "Se iguala la función de distribución F(x) = 1 − e<sup>−λx</sup> a U y se despeja x. Como 1 − U también es uniforme, se usa directamente U."
        formula <- "X = − ln(U) / λ"
      } else if (dist == "norm" && input$n_metodo == "bm") {
        titulo <- "Normal por Box-Muller"
        idea <- "Dos uniformes independientes U<sub>1</sub>, U<sub>2</sub> se convierten en un <b>radio</b> y un <b>ángulo</b>. Al pasar de coordenadas polares a cartesianas se obtienen <b>dos normales estándar</b> independientes. Luego se escala: X = μ + σZ."
        formula <- "Z = √(−2 ln U<sub>1</sub>) · cos(2π U<sub>2</sub>) &nbsp;→&nbsp; X = μ + σZ"
      } else if (dist == "norm") {
        titulo <- "Normal por Aceptación y Rechazo (Ross)"
        idea <- "Se generan candidatos exponenciales Y = −ln(U<sub>1</sub>) y se <b>aceptan</b> si U<sub>2</sub> ≤ e<sup>−(Y−1)²/2</sup>. Los aceptados siguen la mitad positiva de una normal; con U<sub>3</sub> se elige el <b>signo</b>. Se acepta en promedio 1/c ≈ 76% de los candidatos."
        formula <- "c = √(2e/π) ≈ 1.3155 &nbsp;&nbsp; acepta si U<sub>2</sub> ≤ e<sup>−(Y−1)²/2</sup>"
      } else if (input$p_metodo == "ti") {
        titulo <- "Densidad propia por transformación inversa"
        idea <- "Se calcula F(x) acumulando el <b>área bajo f</b> con la regla del trapecio en 2000 subintervalos. Para cada U se busca el x donde <b>F(x) = U</b> con interpolación lineal."
        formula <- "F(x) = ∫<sub>a</sub><sup>x</sup> f(t) dt &nbsp;→&nbsp; X = F<sup>−1</sup>(U)"
      } else {
        titulo <- "Densidad propia por Aceptación y Rechazo"
        idea <- "Se lanzan puntos al azar dentro del rectángulo [a, b] × [0, M], con M = máx f(x). Se <b>aceptan</b> los que caen <b>debajo de la curva</b> f(x). La proporción aceptada es 1/c, con c = M·(b − a)."
        formula <- "Y = a + (b − a)U<sub>1</sub>, &nbsp; V = M·U<sub>2</sub> &nbsp;→&nbsp; acepta si V ≤ f(Y)"
      }
      div(class = "p1-info",
          style = paste0("background:", transparente(d$color, 0.13), "; border-left:6px solid ", d$color, ";"),
          fluidRow(
            column(7,
                   h4(style = "margin-top:0; font-weight:700;",
                      icon(d$icono, style = paste0("color:", d$color, ";")), paste0(" ¿Cómo funciona? ", titulo)),
                   div(style = "font-size:14px; line-height:1.5;", HTML(idea))),
            column(5, div(class = "p1-formula", HTML(formula)))
          ))
    })

    # ---------- 5. Avisos ----------
    output$avisos <- renderUI({
      r <- resultado()
      if (!is.null(r$error)) {
        div(class = "alert alert-danger", icon("circle-xmark"), " ", r$error)
      } else if (!is.null(r$aviso)) {
        div(class = "alert alert-warning", icon("triangle-exclamation"), " ", r$aviso)
      }
    })

    # ---------- 6. Tarjetas ----------
    output$tarjetas <- renderUI({
      r <- datos(); x <- r$x; N <- length(x)
      # el error se considera pequeño si está dentro de 2 errores estándar
      ee_media <- sqrt(r$teo$var / N)
      ok_media <- abs(mean(x) - r$teo$media) <= 2 * ee_media
      ok_var   <- abs(var(x) - r$teo$var) / r$teo$var < 0.1

      cuarta <- if (!is.null(r$rechazo)) {
        tarjeta_p1("filter", "TASA DE ACEPTACIÓN", paste0(round(100 * r$rechazo$tasa, 1), "%"),
                   paste0("Teórica 1/c = ", round(100 * r$rechazo$tasa_teo, 1), "% · ", r$usadas, " uniformes"),
                   PALETA$orquidea)
      } else {
        tarjeta_p1("dice", "UNIFORMES USADAS", r$usadas,
                   paste0(round(r$usadas / N, 2), " por cada valor generado"), PALETA$celeste)
      }
      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
          tarjeta_p1("list-ol", "VALORES GENERADOS", N, r$param_txt, r$color),
          tarjeta_p1("bullseye", "MEDIA MUESTRAL", round(mean(x), 4),
                     paste0("Teórica ", round(r$teo$media, 4)), if (ok_media) PALETA$menta else PALETA$naranja),
          tarjeta_p1("arrows-left-right", "VARIANZA MUESTRAL", round(var(x), 4),
                     paste0("Teórica ", round(r$teo$var, 4)), if (ok_var) PALETA$menta else PALETA$naranja),
          cuarta)
    })

    # ---------- 7. Densidad: histograma + teórica + empírica ----------
    output$g_densidad <- renderPlotly({
      r <- datos(); x <- r$x
      xs <- seq(r$teo$rango[1], r$teo$rango[2], length.out = 400)
      emp <- density(x)
      plot_ly() %>%
        add_histogram(x = x, histnorm = "probability density", nbinsx = input$bins, name = "Datos simulados",
                      marker = list(color = transparente(r$color, 0.55), line = list(color = "white", width = 1))) %>%
        add_lines(x = emp$x, y = emp$y, name = "Densidad empírica",
                  line = list(color = PALETA$noche, width = 2.5)) %>%
        add_lines(x = xs, y = r$teo$dens(xs), name = "Densidad teórica",
                  line = list(color = PALETA$rojo, width = 3, dash = "dash")) %>%
        layout(title = paste(r$nombre, "–", r$metodo), bargap = 0.03,
               xaxis = list(title = "x", range = r$teo$rango), yaxis = list(title = "Densidad")) %>%
        estilo_p4()
    })

    # ---------- 8. CDF empírica vs teórica ----------
    ks <- reactive({
      r <- datos(); x <- sort(r$x); N <- length(x)
      Fteo <- r$teo$cdf(x)
      D <- max(c((1:N) / N - Fteo, Fteo - (0:(N - 1)) / N))
      j <- which.max(pmax((1:N) / N - Fteo, Fteo - (0:(N - 1)) / N))
      list(x = x, Fn = (1:N) / N, D = D, xD = x[j])
    })

    output$ks_texto <- renderUI({
      k <- ks()
      HTML(formula_p1("D = máx | F<sub>n</sub>(x) − F(x) | = <b>", round(k$D, 4),
                      "</b> &nbsp; (en x = ", round(k$xD, 3), ")"))
    })

    output$g_cdf <- renderPlotly({
      r <- datos(); k <- ks()
      xs <- seq(r$teo$rango[1], r$teo$rango[2], length.out = 400)
      plot_ly() %>%
        add_lines(x = k$x, y = k$Fn, name = "CDF empírica Fn(x)", line = list(shape = "hv", color = r$color, width = 2.5)) %>%
        add_lines(x = xs, y = r$teo$cdf(xs), name = "CDF teórica F(x)",
                  line = list(color = PALETA$rojo, width = 2.5, dash = "dash")) %>%
        add_segments(x = k$xD, xend = k$xD, y = r$teo$cdf(k$xD), yend = k$Fn[which(k$x == k$xD)[1]],
                     name = "Distancia máxima D", line = list(color = PALETA$naranja, width = 5)) %>%
        layout(xaxis = list(title = "x", range = r$teo$rango), yaxis = list(title = "Probabilidad acumulada", range = c(0, 1.02))) %>%
        estilo_p4()
    })

    # ---------- 9. Gráfico Q-Q ----------
    output$g_qq <- renderPlotly({
      r <- datos(); x <- sort(r$x); N <- length(x)
      p <- ((1:N) - 0.5) / N
      teo <- r$teo$cuantil(p)
      lim <- range(c(teo, x))
      plot_ly() %>%
        add_markers(x = teo, y = x, name = "Cuantiles", marker = list(size = 5, color = transparente(r$color, 0.6))) %>%
        add_lines(x = lim, y = lim, name = "Diagonal y = x", line = list(color = PALETA$rojo, dash = "dash", width = 2.5)) %>%
        layout(xaxis = list(title = "Cuantil teórico"), yaxis = list(title = "Cuantil de los datos")) %>%
        estilo_p4()
    })

    # ---------- 10. ¿Cómo se generó? ----------
    output$como_texto <- renderUI({
      r <- datos()
      if (!is.null(r$rechazo)) {
        que_veo_p1(paste0("Cada punto es un <b>candidato</b>. Los <b style='color:", PALETA$menta, "'>verdes</b> cayeron debajo de ",
                          r$rechazo$nombre_f, " y se <b>aceptaron</b>; los <b style='color:", PALETA$rojo,
                          "'>rojos</b> quedaron entre f y la envolvente ", r$rechazo$nombre_env, " y se <b>rechazaron</b>.",
                          " Se muestran los primeros 3000 candidatos."))
      } else if (r$metodo == "Box-Muller") {
        que_veo_p1("Cada punto es un par (Z<sub>1</sub>, Z<sub>2</sub>) generado con <b>un radio y un ángulo</b>. La nube es redonda y más densa en el centro: son dos normales estándar independientes. Los círculos punteados marcan los radios 1, 2 y 3: dentro del radio 2 cae cerca del 86% de los pares.")
      } else {
        que_veo_p1("La curva es la función de distribución F(x). Para 15 valores de U se traza una <b>línea horizontal</b> desde U hasta la curva y luego se <b>baja</b> al eje x: ese punto es X = F<sup>−1</sup>(U).")
      }
    })

    output$g_como <- renderPlotly({
      r <- datos()
      if (!is.null(r$rechazo)) {
        rc <- r$rechazo
        cand <- rc$cand[1:min(nrow(rc$cand), 3000), ]
        ts <- seq(rc$rango[1], rc$rango[2], length.out = 400)
        dentro <- cand[cand$acepta, ]; fuera <- cand[!cand$acepta, ]
        plot_ly() %>%
          add_markers(x = fuera$y, y = fuera$v, name = "Rechazados",
                      marker = list(size = 4, color = transparente(PALETA$rojo, 0.5))) %>%
          add_markers(x = dentro$y, y = dentro$v, name = "Aceptados",
                      marker = list(size = 4, color = transparente(PALETA$menta, 0.6))) %>%
          add_lines(x = ts, y = rc$env(ts), name = paste("Envolvente", rc$nombre_env),
                    line = list(color = PALETA$noche, width = 2.5, dash = "dash")) %>%
          add_lines(x = ts, y = rc$f(ts), name = rc$nombre_f, line = list(color = r$color, width = 3.5)) %>%
          layout(xaxis = list(title = "Candidato y", range = rc$rango), yaxis = list(title = "Altura")) %>%
          estilo_p4()

      } else if (r$metodo == "Box-Muller") {
        pz <- r$pares[1:min(nrow(r$pares), 3000), ]
        # círculos de radio 1, 2 y 3 como guía
        circulos <- list()
        for (rad in 1:3) {
          circulos[[rad]] <- list(type = "circle", x0 = -rad, x1 = rad, y0 = -rad, y1 = rad,
                                  line = list(color = PALETA$orquidea, width = 1.5, dash = "dot"))
        }
        plot_ly() %>%
          add_markers(x = pz$z1, y = pz$z2, name = "Pares (Z1, Z2)",
                      marker = list(size = 5, color = transparente(r$color, 0.45))) %>%
          layout(shapes = circulos,
                 xaxis = list(title = "Z1", range = c(-4.2, 4.2), zeroline = TRUE),
                 yaxis = list(title = "Z2", range = c(-4.2, 4.2), scaleanchor = "x", zeroline = TRUE)) %>%
          estilo_p4()

      } else {
        xs <- seq(r$teo$rango[1], r$teo$rango[2], length.out = 400)
        t  <- r$tabla[1:min(15, nrow(r$tabla)), ]
        g <- plot_ly() %>%
          add_lines(x = xs, y = r$teo$cdf(xs), name = "F(x)", line = list(color = r$color, width = 3.5))
        g <- g %>%
          add_segments(x = rep(r$teo$rango[1], nrow(t)), xend = t$x, y = t$u, yend = t$u,
                       name = "U → F(x)", line = list(color = PALETA$celeste, width = 1.5)) %>%
          add_segments(x = t$x, xend = t$x, y = t$u, yend = rep(0, nrow(t)),
                       name = "x = F⁻¹(U)", line = list(color = PALETA$rojo, width = 1.5, dash = "dot")) %>%
          add_markers(x = t$x, y = rep(0, nrow(t)), name = "Valores X", marker = list(size = 9, color = PALETA$rojo)) %>%
          add_markers(x = rep(r$teo$rango[1], nrow(t)), y = t$u, name = "Uniformes U",
                      marker = list(size = 9, color = PALETA$celeste))
        g %>% layout(xaxis = list(title = "x", range = r$teo$rango), yaxis = list(title = "U = F(x)", range = c(-0.02, 1.02))) %>%
          estilo_p4()
      }
    })

    # ---------- 11. Tabla ----------
    output$tabla <- renderDT({
      t <- datos()$tabla
      datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatRound(columns = 2:ncol(t), digits = 5)
    })

    # ---------- 12. Descargas ----------
    resumen <- reactive({
      r <- datos(); x <- r$x
      data.frame(Medida = c("Distribución", "Método", "Fuente de uniformes", "N",
                            "Media muestral", "Media teórica", "Varianza muestral", "Varianza teórica",
                            "Estadístico D (máx |Fn − F|)", "Uniformes usadas"),
                 Valor = c(r$param_txt, r$metodo, if (r$fuente == "mcl") "MCL propio" else "runif",
                           length(x), round(mean(x), 6), round(r$teo$media, 6),
                           round(var(x), 6), round(r$teo$var, 6), round(ks()$D, 6), r$usadas))
    })

    output$desc_xlsx <- downloadHandler(
      filename = function() paste0("continuas_", datos()$dist, ".xlsx"),
      content  = function(file) write_xlsx(list(Datos = datos()$tabla, Resumen = resumen()), file)
    )
    output$desc_csv <- downloadHandler(
      filename = function() paste0("continuas_", datos()$dist, ".csv"),
      content  = function(file) write.csv(datos()$tabla, file, row.names = FALSE)
    )
    output$desc_txt <- downloadHandler(
      filename = function() paste0("continuas_", datos()$dist, ".txt"),
      content  = function(file) write.table(datos()$tabla, file, sep = "\t", row.names = FALSE, quote = FALSE)
    )
  })
}
