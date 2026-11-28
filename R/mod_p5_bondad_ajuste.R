# ==============================================================
# Pestaña 5: Pruebas de Bondad de Ajuste y Diagnóstico Estocástico
# Responsable: Erika
# Usa: R/bondad_ajuste.R, R/generadores.R (Pestaña 1),
#      R/generadores_continuas.R, R/00_paleta.R y estilos de la Pestaña 1
# ==============================================================

library(plotly)
library(DT)
library(writexl)

GENERADORES_P5 <- c(
  "MCL Numerical Recipes (a = 1664525, m = 2³²)" = "nr",
  "MCL Park-Miller (a = 16807, m = 2³¹ − 1)"    = "pm",
  "MCL RANDU de IBM (a = 65539, m = 2³¹)"        = "randu",
  "MCL ejemplo de clase (a = 5, c = 3, m = 16)"  = "clase",
  "Lehmer (n = 4, c = 76)"                       = "lehmer",
  "Cuadrados Medios (n = 4)"                     = "cm",
  "runif de R (referencia)"                      = "runif"
)

CSS_P5 <- paste0(
  css_tarjetas_opcion("p5-fuente", list(gen = PALETA$indigo, archivo = PALETA$petroleo)), "
.p5-cabecera { background: linear-gradient(110deg, ", PALETA$p_lavanda, " 0%, ", PALETA$p_lila, " 50%, ", PALETA$p_rosa, " 100%); }
.p5-sello { display:inline-block; white-space:nowrap; padding:4px 12px; border-radius:20px; font-weight:700; font-size:13px; color:white; }
.p5-resultados table.dataTable tbody td { vertical-align: middle; font-size: 14.5px; }
")

sello_p5 <- function(rechaza) {
  if (rechaza) paste0("<span class='p5-sello' style='background:", PALETA$rojo, ";'>Se rechaza H₀</span>")
  else paste0("<span class='p5-sello' style='background:", PALETA$menta, ";'>No se rechaza H₀</span>")
}


# ==============================================================
# INTERFAZ
# ==============================================================
mod_p5_bondad_ajuste_ui <- function(id) {
  ns <- NS(id)

  div(class = "p1",
    tags$head(tags$style(HTML(CSS_P1)), tags$style(HTML(CSS_P5))),

    div(class = "p1-cabecera p5-cabecera",
        h2(icon("clipboard-check"), " Pruebas de Bondad de Ajuste y Diagnóstico"),
        p("¿Los números se comportan realmente como aleatorios? Se evalúa si siguen la distribución esperada ",
          "(Kolmogorov-Smirnov) y si son independientes entre sí (Prueba de Rachas).")),

    paso_p1(1, "¿Qué datos quieres evaluar?"),
    div(class = "p5-fuente",
        radioButtons(ns("fuente"), NULL, inline = TRUE, width = "100%",
                     choiceNames = list(
                       opcion_tarjeta("Números generados", "Con los generadores de la Pestaña 1", "dice", PALETA$indigo),
                       opcion_tarjeta("Subir un archivo", "Muestra externa en .csv o .xlsx", "file-arrow-up", PALETA$petroleo)),
                     choiceValues = c("gen", "archivo"))),
    br(),

    fluidRow(
      column(3,
        div(class = "p1-panel",
          paso_p1(2, "Configura los datos"),
          conditionalPanel(
            condition = paste0("input['", ns("fuente"), "'] == 'gen'"),
            selectInput(ns("generador"), "Generador", choices = GENERADORES_P5),
            ayuda_p1("Pruebe RANDU, Lehmer o el ejemplo de clase: las pruebas detectan sus defectos."),
            numericInput(ns("semilla"), "Semilla x₀", value = 4123, min = 1),
            ayuda_p1("Lehmer y Cuadrados Medios necesitan una semilla de máximo 4 cifras."),
            numericInput(ns("N"), "Cantidad de números (N)", value = 1000, min = 20, max = 5000),
            ayuda_p1("Entre 20 y 5 000 números.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("fuente"), "'] == 'archivo'"),
            fileInput(ns("archivo"), "Archivo de datos", accept = c(".csv", ".xlsx", ".xls", ".txt"),
                      buttonLabel = "Buscar...", placeholder = "Ningún archivo"),
            ayuda_p1("La primera fila debe tener los nombres de las columnas."),
            uiOutput(ns("columna_ui")),
            downloadLink(ns("ejemplo_csv"), tagList(icon("download"), " Descargar un archivo de ejemplo"))
          ),

          hr(style = "margin:10px 0;"),
          h4(icon("flask"), " Hipótesis para KS", style = "font-size:16px; font-weight:700;"),
          selectInput(ns("dist"), "Distribución esperada",
                      choices = c("Uniforme (a, b)" = "unif", "Normal (μ, σ)" = "norm", "Exponencial (λ)" = "exp")),
          checkboxInput(ns("estimar"), "Estimar los parámetros con la muestra", value = FALSE),
          conditionalPanel(
            condition = paste0("!input['", ns("estimar"), "']"),
            conditionalPanel(paste0("input['", ns("dist"), "'] == 'unif'"),
                             fluidRow(column(6, numericInput(ns("ua"), "a", value = 0)),
                                      column(6, numericInput(ns("ub"), "b", value = 1)))),
            conditionalPanel(paste0("input['", ns("dist"), "'] == 'norm'"),
                             fluidRow(column(6, numericInput(ns("mu"), "μ", value = 0)),
                                      column(6, numericInput(ns("sigma"), "σ", value = 1, min = 0.0001)))),
            conditionalPanel(paste0("input['", ns("dist"), "'] == 'exp'"),
                             numericInput(ns("lambda"), "λ", value = 1, min = 0.0001))
          ),
          ayuda_p1("Para números pseudoaleatorios use Uniforme(0, 1)."),
          sliderInput(ns("alfa"), "Nivel de significancia α", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
          ayuda_p1("Probabilidad de rechazar H₀ cuando en realidad es cierta. Lo usual es 0.05."),
          actionButton(ns("evaluar"), "Aplicar las pruebas", icon = icon("magnifying-glass-chart"),
                       class = "p1-boton", width = "100%",
                       style = paste0("background: linear-gradient(90deg, ", PALETA$violeta, ", ", PALETA$noche, ");"))
        ),
        div(class = "p1-panel",
          paso_p1(4, "Descarga los resultados", PALETA$menta),
          downloadButton(ns("desc_xlsx"), HTML("<b>Excel</b> (.xlsx)"), class = "p1-desc",
                         style = paste0("background:", PALETA$menta, ";")),
          div(class = "p1-ayuda", style = "margin:2px 0 10px 0;", "Hojas: Resultados, Datos y Autocorrelación."),
          downloadButton(ns("desc_csv"), HTML("<b>CSV</b> (.csv)"), class = "p1-desc",
                         style = paste0("background:", PALETA$petroleo, ";")),
          downloadButton(ns("desc_txt"), HTML("<b>Texto</b> (.txt)"), class = "p1-desc",
                         style = paste0("background:", PALETA$indigo, ";"))
        )
      ),

      column(9,
        div(class = "p1-info",
            style = paste0("background:", transparente(PALETA$violeta, 0.12), "; border-left:6px solid ", PALETA$violeta, ";"),
            fluidRow(
              column(6, h4(style = "margin-top:0; font-weight:700;", icon("stairs", style = paste0("color:", PALETA$violeta, ";")),
                           " Kolmogorov-Smirnov: ¿siguen la distribución?"),
                     div(style = "font-size:14px; line-height:1.5;",
                         HTML("Mide la <b>mayor distancia vertical</b> entre la función de distribución empírica de los datos y la teórica. Si esa distancia es grande, los datos no siguen la distribución.")),
                     div(class = "p1-formula", style = "font-size:17px;", HTML("D = máx | F<sub>n</sub>(x) − F(x) |"))),
              column(6, h4(style = "margin-top:0; font-weight:700;", icon("shuffle", style = paste0("color:", PALETA$violeta, ";")),
                           " Rachas: ¿son independientes?"),
                     div(style = "font-size:14px; line-height:1.5;",
                         HTML("Una <b>racha</b> es una sucesión de valores del mismo tipo (sobre la mediana, o subiendo). Muy pocas rachas indican tendencias; demasiadas, alternancia artificial.")),
                     div(class = "p1-formula", style = "font-size:17px;", HTML("Z = (R − E[R]) / √V[R]")))
            )),
        uiOutput(ns("avisos")),
        uiOutput(ns("tarjetas")),
        br(),
        div(class = "p1-panel p5-resultados",
          paso_p1(3, "Resultados de las pruebas"),
          DTOutput(ns("resultados")),
          br(),
          tabsetPanel(
            tabPanel(tagList(icon("stairs"), " Diagnóstico KS"),
                     uiOutput(ns("ks_texto")),
                     plotlyOutput(ns("g_ks"), height = "400px"),
                     plotlyOutput(ns("g_hist"), height = "330px")),
            tabPanel(tagList(icon("shuffle"), " Diagnóstico de rachas"),
                     uiOutput(ns("rachas_texto")),
                     plotlyOutput(ns("g_secuencia"), height = "380px"),
                     plotlyOutput(ns("g_longitudes"), height = "300px")),
            tabPanel(tagList(icon("link-slash"), " Independencia"),
                     que_veo_p1("Izquierda: pares consecutivos (xᵢ, xᵢ₊₁); si son independientes no forman patrones. Derecha: autocorrelación para cada retardo k; las barras deben quedar <b>dentro de las bandas</b> ± 1.96/√n."),
                     fluidRow(column(6, plotlyOutput(ns("g_lag"), height = "400px")),
                              column(6, plotlyOutput(ns("g_acf"), height = "400px")))),
            tabPanel(tagList(icon("table"), " Datos"),
                     DTOutput(ns("tabla_datos")))
          )
        )
      )
    )
  )
}


# ==============================================================
# SERVIDOR
# ==============================================================
mod_p5_bondad_ajuste_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    estilo_p5 <- function(g) {
      g %>% layout(font = list(family = "Segoe UI, Helvetica, Arial", color = PALETA$noche),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   legend = list(orientation = "h", y = -0.22, yanchor = "top")) %>%
        config(displaylogo = FALSE, locale = "es")
    }

    # ---------- Archivo subido ----------
    archivo <- reactive({
      req(input$archivo)
      tryCatch(leer_archivo_datos(input$archivo$datapath, input$archivo$name),
               error = function(e) list(error = conditionMessage(e)))
    })

    output$columna_ui <- renderUI({
      d <- archivo()
      if (is.list(d) && !is.data.frame(d)) return(div(class = "alert alert-danger", d$error))
      tagList(selectInput(session$ns("columna"), "Columna a evaluar", choices = names(d)),
              div(class = "p1-ayuda", paste(nrow(d), "filas leídas.")))
    })

    output$ejemplo_csv <- downloadHandler(
      filename = function() "ejemplo_tiempos_espera.csv",
      content = function(file) {
        u <- crear_fuente_uniforme(99)(300)
        write.csv(data.frame(tiempo_espera_min = round(-log(u) * 4, 3)), file, row.names = FALSE)
      })

    # ---------- Datos a evaluar ----------
    obtener_datos <- function() {
      if (input$fuente == "gen") {
        N <- input$N; x0 <- input$semilla
        if (is.na(N) || N < 20 || N > 5000) stop("N debe estar entre 20 y 5 000.")
        r <- switch(input$generador,
                    nr     = gen_mcl(x0, 1664525, 1013904223, 2^32, N),
                    pm     = gen_mcl(x0, 16807, 0, 2^31 - 1, N),
                    randu  = gen_mcl(x0, 65539, 0, 2^31, N),
                    clase  = gen_mcl(x0, 5, 3, 16, N),
                    lehmer = gen_lehmer(x0, 76, 4, N),
                    cm     = gen_cuadrados_medios(x0, 4, N),
                    runif  = { set.seed(x0); list(u = runif(N)) })
        if (length(r$u) < 20) stop(paste0("El generador solo produjo ", length(r$u),
                                          " números antes de detenerse (", r$aviso, "). Pruebe otra semilla."))
        list(x = r$u, origen = names(GENERADORES_P5)[GENERADORES_P5 == input$generador], aviso = r$aviso)
      } else {
        if (is.null(input$archivo)) stop("Primero suba un archivo .csv o .xlsx con los datos.")
        d <- archivo()
        if (is.list(d) && !is.data.frame(d)) stop(d$error)
        columna <- if (is.null(input$columna) || !(input$columna %in% names(d))) names(d)[1] else input$columna
        x <- d[[columna]]
        x <- x[!is.na(x)]
        if (length(x) < 10) stop("La columna necesita al menos 10 valores numéricos.")
        list(x = x, origen = paste0(input$archivo$name, " → ", columna), aviso = NULL)
      }
    }

    # ---------- Ejecutar las pruebas ----------
    resultado <- eventReactive(input$evaluar, {
      tryCatch({
        d <- obtener_datos(); x <- d$x
        alfa <- input$alfa
        dist <- input$dist
        if (dist == "unif") {
          a <- if (input$estimar) min(x) else input$ua
          b <- if (input$estimar) max(x) else input$ub
          if (!(b > a)) stop("En la Uniforme, b debe ser mayor que a.")
          cdf <- function(t) pmin(pmax((t - a) / (b - a), 0), 1)
          dens <- function(t) ifelse(t >= a & t <= b, 1 / (b - a), 0)
          hip <- paste0("Uniforme(", signif(a, 4), ", ", signif(b, 4), ")")
        } else if (dist == "norm") {
          mu <- if (input$estimar) mean(x) else input$mu
          s  <- if (input$estimar) sd(x) else input$sigma
          if (!(s > 0)) stop("σ debe ser mayor que 0.")
          cdf <- function(t) pnorm(t, mu, s)
          dens <- function(t) exp(-(t - mu)^2 / (2 * s^2)) / (s * sqrt(2 * pi))
          hip <- paste0("Normal(μ = ", signif(mu, 4), ", σ = ", signif(s, 4), ")")
        } else {
          l <- if (input$estimar) 1 / mean(x) else input$lambda
          if (!(l > 0)) stop("λ debe ser mayor que 0.")
          cdf <- function(t) ifelse(t < 0, 0, 1 - exp(-l * t))
          dens <- function(t) ifelse(t < 0, 0, l * exp(-l * t))
          hip <- paste0("Exponencial(λ = ", signif(l, 4), ")")
        }
        list(x = x, origen = d$origen, aviso = d$aviso, alfa = alfa, hip = hip, cdf = cdf, dens = dens,
             estimado = input$estimar,
             ks = prueba_ks(x, cdf, alfa),
             med = prueba_rachas_mediana(x, alfa),
             asc = prueba_rachas_ascendentes(x, alfa),
             acf = autocorrelacion(x, min(20, length(x) - 2)))
      }, error = function(e) list(error = conditionMessage(e)))
    }, ignoreNULL = FALSE)

    datos <- reactive({
      r <- resultado()
      validate(need(is.null(r$error), r$error))
      r
    })

    output$avisos <- renderUI({
      r <- resultado()
      if (!is.null(r$error)) return(div(class = "alert alert-danger", icon("circle-xmark"), " ", r$error))
      tagList(
        if (!is.null(r$aviso)) div(class = "alert alert-warning", icon("triangle-exclamation"), " Generador: ", r$aviso),
        if (isTRUE(r$estimado)) div(class = "alert alert-info", icon("circle-info"),
                                    " Los parámetros se estimaron con la misma muestra: el valor p de KS resulta conservador (corrección de Lilliefors).")
      )
    })

    # ---------- Tarjetas ----------
    output$tarjetas <- renderUI({
      r <- datos(); x <- r$x
      superadas <- sum(!c(r$ks$rechaza, r$med$rechaza, r$asc$rechaza))
      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
          tarjeta_p1("database", "DATOS EVALUADOS", length(x), r$origen, PALETA$violeta),
          tarjeta_p1("bullseye", "MEDIA", round(mean(x), 4), paste0("Desv. estándar ", round(sd(x), 4)), PALETA$petroleo),
          tarjeta_p1("flask", "HIPÓTESIS H₀", r$hip, paste0("α = ", r$alfa), PALETA$indigo),
          tarjeta_p1(if (superadas == 3) "award" else "triangle-exclamation", "PRUEBAS SUPERADAS",
                     paste0(superadas, " de 3"),
                     if (superadas == 3) "Los datos parecen aleatorios" else "Hay evidencia de un problema",
                     if (superadas == 3) PALETA$menta else PALETA$rojo))
    })

    # ---------- Tabla de resultados ----------
    tabla_resultados <- reactive({
      r <- datos()
      data.frame(
        Prueba = c("Kolmogorov-Smirnov", "Rachas sobre/bajo la mediana", "Rachas ascendentes/descendentes"),
        Hipótesis = c(paste("H₀: los datos siguen", r$hip), "H₀: la secuencia es independiente", "H₀: la secuencia es independiente"),
        Estadístico = c(paste0("D = ", round(r$ks$D, 5)),
                        paste0("R = ", r$med$R, " (E = ", round(r$med$esperado, 2), ") · Z = ", round(r$med$Z, 4)),
                        paste0("R = ", r$asc$R, " (E = ", round(r$asc$esperado, 2), ") · Z = ", round(r$asc$Z, 4))),
        Valor_crítico = c(paste0("D crít = ", round(r$ks$critico, 5)),
                          paste0("|Z| crít = ", round(r$med$critico, 4)),
                          paste0("|Z| crít = ", round(r$asc$critico, 4))),
        Valor_p = c(r$ks$p_valor, r$med$p_valor, r$asc$p_valor),
        Rechaza = c(r$ks$rechaza, r$med$rechaza, r$asc$rechaza))
    })

    output$resultados <- renderDT({
      t <- tabla_resultados()
      t$Valor_p <- ifelse(t$Valor_p < 1e-4, "< 0.0001", format(round(t$Valor_p, 4), nsmall = 4))
      t$Decisión <- sapply(t$Rechaza, sello_p5)
      t$Rechaza <- NULL
      datatable(t, rownames = FALSE, escape = FALSE, options = list(dom = "t", ordering = FALSE),
                colnames = c("Prueba", "Hipótesis", "Estadístico", "Valor crítico", "Valor p", "Decisión"))
    })

    # ---------- Diagnóstico KS ----------
    output$ks_texto <- renderUI({
      k <- datos()$ks
      que_veo_p1(paste0("La distancia máxima entre la CDF empírica y la teórica es <b>D = ", round(k$D, 5),
                        "</b> (barra naranja, en x = ", signif(k$x_max, 4), "). Se rechaza H₀ si D supera el valor crítico ",
                        round(k$critico, 5), "."))
    })

    output$g_ks <- renderPlotly({
      r <- datos(); k <- r$ks
      rango <- range(k$xs); ancho <- diff(rango); ts <- seq(rango[1] - 0.05 * ancho, rango[2] + 0.05 * ancho, length.out = 400)
      j <- which(k$xs == k$x_max)[1]
      plot_ly() %>%
        add_lines(x = k$xs, y = k$Fn, name = "CDF empírica Fn(x)", line = list(shape = "hv", color = PALETA$violeta, width = 2.5)) %>%
        add_lines(x = ts, y = r$cdf(ts), name = "CDF teórica F(x)", line = list(color = PALETA$rojo, dash = "dash", width = 2.5)) %>%
        add_segments(x = k$x_max, xend = k$x_max, y = k$Fx[j], yend = if (k$d_mas >= k$d_menos) k$Fn[j] else k$Fn[j] - 1 / length(k$xs),
                     name = "Distancia máxima D", line = list(color = PALETA$naranja, width = 6)) %>%
        layout(xaxis = list(title = "x"), yaxis = list(title = "Probabilidad acumulada", range = c(0, 1.03))) %>%
        estilo_p5()
    })

    output$g_hist <- renderPlotly({
      r <- datos(); x <- r$x
      rango <- range(x); ancho <- diff(rango); ts <- seq(rango[1] - 0.05 * ancho, rango[2] + 0.05 * ancho, length.out = 400)
      plot_ly() %>%
        add_histogram(x = x, histnorm = "probability density", nbinsx = 30, name = "Datos",
                      marker = list(color = transparente(PALETA$violeta, 0.55), line = list(color = "white", width = 1))) %>%
        add_lines(x = ts, y = r$dens(ts), name = "Densidad de H₀", line = list(color = PALETA$rojo, width = 3, dash = "dash")) %>%
        layout(xaxis = list(title = "x"), yaxis = list(title = "Densidad"), bargap = 0.03) %>%
        estilo_p5()
    })

    # ---------- Diagnóstico de rachas ----------
    output$rachas_texto <- renderUI({
      m <- datos()$med
      que_veo_p1(paste0("Cada punto está <b style='color:", PALETA$petroleo, "'>sobre</b> o <b style='color:", PALETA$durazno,
                        "'>bajo</b> la mediana (", signif(m$mediana, 4), "). Se contaron <b>", m$R, " rachas</b> (", m$n1,
                        " valores arriba y ", m$n2, " abajo); si la secuencia fuera aleatoria se esperarían unas <b>",
                        round(m$esperado, 1), "</b>. Se muestran los primeros 200 valores."))
    })

    output$g_secuencia <- renderPlotly({
      r <- datos(); x <- r$x; m <- r$med$mediana
      ver <- 1:min(200, length(x))
      arriba <- x[ver] > m
      plot_ly() %>%
        add_lines(x = ver, y = x[ver], name = "Secuencia", line = list(color = PALETA$p_lavanda, width = 1.5)) %>%
        add_markers(x = ver[arriba], y = x[ver][arriba], name = "Sobre la mediana",
                    marker = list(color = PALETA$petroleo, size = 7)) %>%
        add_markers(x = ver[!arriba], y = x[ver][!arriba], name = "Bajo la mediana",
                    marker = list(color = PALETA$durazno, size = 7)) %>%
        add_lines(x = range(ver), y = rep(m, 2), name = "Mediana", line = list(color = PALETA$noche, dash = "dash", width = 2)) %>%
        layout(xaxis = list(title = "Posición i"), yaxis = list(title = "xᵢ")) %>%
        estilo_p5()
    })

    output$g_longitudes <- renderPlotly({
      lon <- datos()$med$longitudes
      tab <- table(lon)
      # bajo independencia, una racha de longitud L tiene probabilidad aproximada (1/2)^L
      L <- as.numeric(names(tab))
      esperado <- length(lon) * 0.5^L
      plot_ly() %>%
        add_bars(x = L, y = as.numeric(tab), name = "Rachas observadas", marker = list(color = PALETA$violeta)) %>%
        add_lines(x = L, y = esperado, name = "Esperado si es aleatoria ≈ R·(1/2)ᴸ",
                  line = list(color = PALETA$rojo, dash = "dash", width = 2.5)) %>%
        layout(title = "Longitud de las rachas", xaxis = list(title = "Longitud L", dtick = 1),
               yaxis = list(title = "Cantidad de rachas")) %>%
        estilo_p5()
    })

    # ---------- Independencia ----------
    output$g_lag <- renderPlotly({
      x <- datos()$x; n <- length(x)
      plot_ly(x = x[1:(n - 1)], y = x[2:n], type = "scatter", mode = "markers",
              marker = list(size = 5, color = transparente(PALETA$violeta, 0.55))) %>%
        layout(title = "Pares consecutivos", xaxis = list(title = "xᵢ"), yaxis = list(title = "xᵢ₊₁")) %>%
        estilo_p5()
    })

    output$g_acf <- renderPlotly({
      r <- datos(); a <- r$acf; banda <- 1.96 / sqrt(length(r$x))
      fuera <- abs(a$r) > banda
      plot_ly() %>%
        add_bars(x = a$lag, y = a$r, name = "r(k)", marker = list(color = ifelse(fuera, PALETA$rojo, PALETA$petroleo))) %>%
        add_lines(x = c(0.5, max(a$lag) + 0.5), y = c(banda, banda), name = "± 1.96/√n",
                  line = list(color = PALETA$noche, dash = "dash")) %>%
        add_lines(x = c(0.5, max(a$lag) + 0.5), y = c(-banda, -banda), showlegend = FALSE,
                  line = list(color = PALETA$noche, dash = "dash")) %>%
        layout(title = "Autocorrelación", xaxis = list(title = "Retardo k", dtick = 2), yaxis = list(title = "r(k)")) %>%
        estilo_p5()
    })

    output$tabla_datos <- renderDT({
      x <- datos()$x
      datatable(data.frame(i = seq_along(x), x = x), rownames = FALSE, options = list(pageLength = 10)) %>%
        formatRound(columns = "x", digits = 6)
    })

    # ---------- Descargas ----------
    resultados_exportar <- reactive({
      t <- tabla_resultados()
      t$Valor_p <- round(t$Valor_p, 6)
      t$Decisión <- ifelse(t$Rechaza, "Se rechaza H0", "No se rechaza H0")
      t$Rechaza <- NULL
      t$alfa <- datos()$alfa
      t
    })
    output$desc_xlsx <- downloadHandler(
      filename = function() "bondad_de_ajuste.xlsx",
      content  = function(file) write_xlsx(list(Resultados = resultados_exportar(),
                                                Datos = data.frame(i = seq_along(datos()$x), x = datos()$x),
                                                Autocorrelacion = datos()$acf), file))
    output$desc_csv <- downloadHandler(
      filename = function() "bondad_de_ajuste.csv",
      content  = function(file) write.csv(resultados_exportar(), file, row.names = FALSE))
    output$desc_txt <- downloadHandler(
      filename = function() "bondad_de_ajuste.txt",
      content  = function(file) write.table(resultados_exportar(), file, sep = "\t", row.names = FALSE, quote = FALSE))
  })
}
