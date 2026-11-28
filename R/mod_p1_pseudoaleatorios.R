# ==============================================================
# Pestaña 1: Generación de Números Pseudoaleatorios
# Responsable: Sandy
# Usa: R/00_paleta.R            
#      R/generadores.R           
#      R/analisis_generadores.R
# ==============================================================

library(plotly)    # gráficos interactivos (2D y 3D)
library(DT)        # tablas interactivas
library(writexl)   # descarga en Excel


# --------------------------------------------------------------
# Identidad de cada método: color, icono y descripción
# --------------------------------------------------------------
METODOS_P1 <- list(
  mcl = list(
    nombre  = "Congruencial Lineal",
    corto   = "MCL",
    icono   = "calculator",
    fuerte  = PALETA$petroleo,
    suave   = PALETA$p_celeste,
    lema    = "Multiplica, suma y se queda con el residuo",
    formula = "x<sub>n+1</sub> = (a · x<sub>n</sub> + c) mod m &nbsp;&nbsp;→&nbsp;&nbsp; u<sub>n</sub> = x<sub>n</sub> / m",
    idea    = paste("Cada número se obtiene del anterior: se multiplica por <b>a</b>, se le suma <b>c</b>",
                    "y se toma el <b>residuo</b> de dividir para <b>m</b>. Al dividir ese residuo para m",
                    "se obtiene un número entre 0 y 1. Es el método más usado en la práctica."),
    ventaja = "Periodo muy largo si a, c y m cumplen el Teorema de Hull-Dobell.",
    cuidado = "Con malos parámetros aparecen patrones (ejemplo: RANDU de IBM)."
  ),
  lehmer = list(
    nombre  = "Método de Lehmer",
    corto   = "Lehmer",
    icono   = "scissors",
    fuerte  = PALETA$durazno,
    suave   = PALETA$p_rosa,
    lema    = "Multiplica, corta las cifras y resta",
    formula = "x<sub>i</sub> · c = [ k cifras | n cifras ] &nbsp;&nbsp;→&nbsp;&nbsp; x<sub>i+1</sub> = derecha − izquierda",
    idea    = paste("Propuesto por D. H. Lehmer (1951). La semilla se multiplica por la constante <b>c</b>;",
                    "al resultado se le <b>cortan las k primeras cifras</b> y se le restan a las <b>n cifras restantes</b>.",
                    "El número u se obtiene dividiendo para 10<sup>n</sup>."),
    ventaja = "Cálculo muy sencillo, se puede hacer a mano.",
    cuidado = "Puede producir números negativos y entonces el método se detiene."
  ),
  cm = list(
    nombre  = "Cuadrados Medios",
    corto   = "Cuadrados Medios",
    icono   = "border-all",
    fuerte  = PALETA$violeta,
    suave   = PALETA$p_lila,
    lema    = "Eleva al cuadrado y toma el centro",
    formula = "x<sub>i+1</sub> = n cifras centrales de x<sub>i</sub><sup>2</sup> &nbsp;&nbsp;→&nbsp;&nbsp; u<sub>i</sub> = x<sub>i</sub> / 10<sup>n</sup>",
    idea    = paste("Propuesto por Von Neumann y Metropolis (1946), es uno de los primeros generadores.",
                    "La semilla se <b>eleva al cuadrado</b>, se completa con ceros hasta 2n cifras",
                    "y las <b>n cifras del centro</b> forman el siguiente número."),
    ventaja = "Muy fácil de entender; fue histórico en las primeras computadoras.",
    cuidado = "Suele caer en ciclos cortos o degenerar en cero."
  )
)

# Generadores congruenciales conocidos
PRESETS_MCL <- list(
  clase = list(x0 = 7,    a = 5,       c = 3,          m = 16,        N = 20),
  pm    = list(x0 = 4122, a = 16807,   c = 0,          m = 2^31 - 1,  N = 2000),
  randu = list(x0 = 1,    a = 65539,   c = 0,          m = 2^31,      N = 3000),
  nr    = list(x0 = 4122, a = 1664525, c = 1013904223, m = 2^32,      N = 2000)
)


# --------------------------------------------------------------
# Estilos de la pestaña (solo afectan a elementos con clase p1-)
# --------------------------------------------------------------
CSS_P1 <- paste0("
.p1 { color: ", PALETA$noche, "; }
.p1-cabecera { background: linear-gradient(110deg, ", PALETA$p_celeste, " 0%, ", PALETA$p_lavanda, " 50%, ",
                 PALETA$p_lila, " 100%); border-radius: 18px; padding: 18px 26px; margin-bottom: 18px; }
.p1-cabecera h2 { margin: 0; font-weight: 700; color: ", PALETA$noche, "; }
.p1-cabecera p  { margin: 4px 0 0 0; color: ", PALETA$pizarra, "; font-size: 15px; }

/* Selector de métodos en forma de tarjetas */
.p1-metodos .shiny-options-group { display: flex; gap: 16px; }
.p1-metodos .radio-inline { flex: 1; margin: 0 !important; padding: 0 !important; }
.p1-metodos input[type=radio] { display: none; }
.p1-metodos .radio-inline > span { display: block; }
.p1-op { border: 2px solid #e6e8f0; border-radius: 16px; padding: 12px 16px; background: white;
         cursor: pointer; transition: all .2s; display: flex; align-items: center; gap: 14px; }
.p1-op:hover { transform: translateY(-2px); box-shadow: 0 6px 14px rgba(53,56,125,.12); }
.p1-op .p1-ico { width: 46px; height: 46px; border-radius: 12px; display: flex; align-items: center;
                 justify-content: center; font-size: 20px; color: white; flex-shrink: 0; }
.p1-op .p1-tit { font-weight: 700; font-size: 16px; color: ", PALETA$noche, "; }
.p1-op .p1-lema { font-size: 12.5px; color: #6b6f8a; }
.p1-metodos label:has(input:checked) .p1-op { box-shadow: 0 8px 18px rgba(53,56,125,.18); transform: translateY(-2px); }
.p1-metodos .radio-inline:nth-child(1) label, .p1-metodos .radio-inline:nth-child(1) { }
.p1-metodos label:has(input[value=mcl]:checked) .p1-op    { border-color: ", PALETA$petroleo, "; background: ", PALETA$p_celeste, "55; }
.p1-metodos label:has(input[value=lehmer]:checked) .p1-op { border-color: ", PALETA$durazno, ";  background: ", PALETA$p_rosa, "88; }
.p1-metodos label:has(input[value=cm]:checked) .p1-op     { border-color: ", PALETA$violeta, ";  background: ", PALETA$p_lila, "55; }

/* Paneles */
.p1-panel { background: white; border: 1px solid #eceef5; border-radius: 16px; padding: 16px 18px;
            box-shadow: 0 2px 8px rgba(53,56,125,.06); margin-bottom: 16px; }
.p1-panel h4 { font-weight: 700; color: ", PALETA$noche, "; margin-top: 0; }
.p1-paso-num { display: inline-block; width: 26px; height: 26px; border-radius: 50%; text-align: center;
               line-height: 26px; color: white; font-weight: 700; margin-right: 6px; font-size: 14px; }
.p1-ayuda { font-size: 12px; color: #7a7e99; margin: -10px 0 12px 0; line-height: 1.35; }
.p1-ayuda .fa, .p1-ayuda .fas, .p1-ayuda svg { color: ", PALETA$celeste, "; }
.p1 .form-control { border-radius: 10px; }
.p1 .control-label { font-weight: 600; color: ", PALETA$pizarra, "; }
.p1-boton { border: none; border-radius: 12px; font-weight: 700; font-size: 17px; padding: 10px;
            color: white; box-shadow: 0 6px 14px rgba(53,56,125,.2); }
.p1-boton:hover, .p1-boton:focus { color: white; filter: brightness(1.07); }

.p1-desc { display: block; width: 100%; border: none; border-radius: 12px; color: white !important;
           padding: 9px; margin-bottom: 8px; text-align: left; font-size: 14.5px; }
.p1-desc:hover, .p1-desc:focus { filter: brightness(1.08); color: white; }
.p1-desc .fa, .p1-desc svg { margin-right: 6px; }

.p1-proceso { background: #f7f8fc; border: 1px solid #eceef5; border-radius: 16px; padding: 10px 20px 16px 20px;
              margin: 6px 0 18px 0; }

/* Descripción del método */
.p1-info { border-radius: 16px; padding: 16px 20px; margin-bottom: 16px; }
.p1-formula { font-family: Cambria, Georgia, serif; font-size: 19px; background: rgba(255,255,255,.75);
              border-radius: 10px; padding: 8px 12px; margin: 8px 0; text-align: center; }
.p1-chip { display: inline-block; border-radius: 20px; padding: 5px 12px; font-size: 12.5px; margin: 4px 0;
           background: rgba(255,255,255,.8); }

/* Pestañas internas */
.p1 .nav-tabs > li > a { color: ", PALETA$pizarra, "; border-radius: 12px 12px 0 0; font-weight: 600; }
.p1 .nav-tabs > li.active > a { color: ", PALETA$noche, "; border-top: 3px solid ", PALETA$indigo, "; }
.p1 .nav-pills > li > a { border-radius: 20px; color: ", PALETA$pizarra, "; padding: 6px 14px; }
.p1 .nav-pills > li.active > a { background: ", PALETA$indigo, "; color: white; }
.p1 .nav-pills { margin-top: 6px; }
.p1-que { background: ", PALETA$p_rosa, "66; border-left: 4px solid ", PALETA$coral, "; border-radius: 10px;
          padding: 8px 12px; font-size: 13px; margin: 8px 0 4px 0; color: ", PALETA$pizarra, "; }
.p1 .alert { border-radius: 12px; }
")


# --------------------------------------------------------------
# Funciones auxiliares de presentación
# --------------------------------------------------------------

# Tarjeta de resumen
tarjeta_p1 <- function(icono, titulo, valor, nota, color) {
  div(style = "flex:1; min-width:165px; background:white; border:1px solid #eef0f6; border-radius:14px; padding:10px 14px; display:flex; gap:10px; align-items:center;",
      div(style = paste0("width:30px; height:30px; border-radius:9px; display:flex; align-items:center; justify-content:center;",
                         "font-size:13px; flex-shrink:0; color:", color, "; background:", transparente(color, 0.15), ";"),
          icon(icono)),
      div(div(style = "font-size:10.5px; color:#8a8ea8; font-weight:600; letter-spacing:.4px;", titulo),
          div(style = paste0("font-size:19px; font-weight:700; color:", PALETA$noche, "; line-height:1.2;"), valor),
          div(style = "font-size:11px; color:#9a9eb5;", nota)))
}

# Texto de ayuda debajo de un parámetro
ayuda_p1 <- function(texto) div(class = "p1-ayuda", icon("circle-info"), " ", HTML(texto))

# Breve explicación de qué muestra un gráfico
que_veo_p1 <- function(texto) div(class = "p1-que", icon("lightbulb"), " ", HTML(texto))

# Título de bloque numerado
paso_p1 <- function(numero, texto, color = PALETA$indigo) {
  h4(span(class = "p1-paso-num", style = paste0("background:", color, ";"), numero), texto)
}

# Opción del selector de métodos (tarjeta con icono)
opcion_metodo_p1 <- function(clave) {
  m <- METODOS_P1[[clave]]
  div(class = "p1-op",
      div(class = "p1-ico", style = paste0("background:", transparente(m$fuerte, 0.16), "; color:", m$fuerte, ";"), icon(m$icono)),
      div(div(class = "p1-tit", m$nombre), div(class = "p1-lema", m$lema)))
}

# Número sin notación científica
num_p1 <- function(x) format(x, scientific = FALSE, big.mark = "")

# Completa con ceros a la izquierda hasta d cifras
ceros_p1 <- function(x, d) {
  texto <- num_p1(x)
  if (nchar(texto) < d) texto <- paste0(strrep("0", d - nchar(texto)), texto)
  texto
}

# Muestra las cifras de un número en cajitas, pintando de color las cifras [ini, fin]
cifras_html <- function(texto, ini, fin, color) {
  cajas <- ""
  for (j in 1:nchar(texto)) {
    resaltar <- j >= ini && j <= fin
    estilo <- paste0("display:inline-block; width:36px; padding:6px 0; margin:2px; text-align:center;",
                     "font-size:22px; font-family:monospace; border-radius:8px;",
                     if (resaltar) paste0("background:", color, "; color:white; font-weight:bold;")
                     else "background:#eef0f6; color:#555;")
    cajas <- paste0(cajas, "<span style='", estilo, "'>", substr(texto, j, j), "</span>")
  }
  cajas
}

# Línea de fórmula centrada (HTML, no necesita internet)
formula_p1 <- function(...) {
  paste0("<div style='text-align:center; font-family:Cambria, Georgia, serif; font-size:20px; margin:10px 0;'>",
         ..., "</div>")
}

# Resultado enmarcado
caja_p1 <- function(x) {
  paste0("<span style='border:2px solid ", PALETA$rojo, "; border-radius:8px; padding:1px 8px; font-weight:bold;'>",
         x, "</span>")
}


# ==============================================================
# INTERFAZ (UI)
# ==============================================================
mod_p1_pseudoaleatorios_ui <- function(id) {
  ns <- NS(id)

  div(class = "p1",
    tags$head(tags$style(HTML(CSS_P1))),

    # ---------------- Cabecera ----------------
    div(class = "p1-cabecera",
        h2(icon("dice"), " Generación de Números Pseudoaleatorios"),
        p("Produce secuencias de números entre 0 y 1 que se comportan como aleatorios, ",
          "usando solo una fórmula y un valor inicial (la semilla).")),

    # ---------------- Paso 1: elegir el método ----------------
    paso_p1(1, "Elige el método de generación"),
    div(class = "p1-metodos",
        radioButtons(ns("metodo"), NULL, inline = TRUE, width = "100%",
                     choiceNames  = list(opcion_metodo_p1("mcl"), opcion_metodo_p1("lehmer"),
                                         opcion_metodo_p1("cm")),
                     choiceValues = c("mcl", "lehmer", "cm"))),
    br(),

    fluidRow(
      # ---------------- Paso 2: parámetros ----------------
      column(3,
        div(class = "p1-panel",
          paso_p1(2, "Configura los parámetros"),

          # --- MCL ---
          conditionalPanel(
            condition = paste0("input['", ns("metodo"), "'] == 'mcl'"),
            selectInput(ns("preset"), "Generador predefinido",
                        choices = c("Personalizado"                        = "pers",
                                    "Ejemplo de clase (a=5, c=3, m=16)"    = "clase",
                                    "Park-Miller (a=16807, m=2³¹−1)"       = "pm",
                                    "RANDU de IBM (a=65539, m=2³¹)"        = "randu",
                                    "Numerical Recipes (a=1664525, m=2³²)" = "nr")),
            ayuda_p1("Carga los parámetros de generadores conocidos para compararlos.")
          ),

          numericInput(ns("x0"), "Semilla x₀", value = 4122, min = 1),
          ayuda_p1("Número inicial. Con la misma semilla siempre se obtiene la misma secuencia."),

          conditionalPanel(
            condition = paste0("input['", ns("metodo"), "'] == 'mcl'"),
            numericInput(ns("a"), "Multiplicador a", value = 69069, min = 0),
            ayuda_p1("Valor por el que se multiplica el número anterior en cada paso."),
            numericInput(ns("c_mcl"), "Incremento c", value = 1, min = 0),
            ayuda_p1("Se suma en cada paso. Si c = 0, el generador es <b>multiplicativo</b>."),
            numericInput(ns("m"), "Módulo m", value = 2^32, min = 2),
            ayuda_p1("Divisor: los valores x quedan entre 0 y m − 1, así que hay como máximo m números distintos.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("metodo"), "'] == 'lehmer'"),
            numericInput(ns("n_leh"), "Cifras de la semilla (n)", value = 4, min = 2, max = 10),
            ayuda_p1("Cuántas cifras tiene la semilla; cada número u tendrá n decimales."),
            numericInput(ns("c_leh"), "Constante c", value = 76, min = 1),
            ayuda_p1("Multiplicador de <b>k</b> cifras (k = cifras de c). Se recomienda que k sea menor que n.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("metodo"), "'] == 'cm'"),
            selectInput(ns("n_cm"), "Cifras de la semilla (n)", choices = c(2, 4, 6), selected = 4),
            ayuda_p1("Debe ser par: se toman las n cifras del centro de x² (que tiene 2n cifras).")
          ),

          numericInput(ns("N"), "Cantidad de números (N)", value = 500, min = 1, max = 20000),
          ayuda_p1("Cuántos números pseudoaleatorios se van a generar (máximo 20 000)."),

          sliderInput(ns("bins"), "Barras del histograma", min = 5, max = 50, value = 20),

          uiOutput(ns("boton")),
          conditionalPanel(
            condition = paste0("input['", ns("metodo"), "'] != 'mcl'"),
            div(style = "text-align:center; margin-top:10px;",
                actionLink(ns("ejemplo"), "Cargar el ejemplo del anexo", icon = icon("book-open")))
          )
        ),

        # ---------------- Paso 4: descargas ----------------
        div(class = "p1-panel",
          paso_p1(4, "Descarga los datos", PALETA$menta),
          div(class = "p1-ayuda", style = "margin:0 0 10px 0;",
              icon("circle-info"), " Se descargan los números del último botón ", tags$b("Generar"), "."),
          downloadButton(ns("desc_xlsx"), HTML("<b>Excel</b> (.xlsx)"), class = "p1-desc",
                         style = paste0("background:", PALETA$menta, ";")),
          div(class = "p1-ayuda", style = "margin:2px 0 10px 0;",
              "Incluye hojas: Datos, Resumen, Chi-cuadrado y Comparación."),
          downloadButton(ns("desc_csv"), HTML("<b>CSV</b> (.csv)"), class = "p1-desc",
                         style = paste0("background:", PALETA$petroleo, ";")),
          downloadButton(ns("desc_txt"), HTML("<b>Texto</b> (.txt)"), class = "p1-desc",
                         style = paste0("background:", PALETA$indigo, ";"))
        )
      ),

      # ---------------- Paso 3: resultados ----------------
      column(9,
        uiOutput(ns("info_metodo")),
        uiOutput(ns("avisos")),
        uiOutput(ns("tarjetas")),
        br(),

        div(class = "p1-panel",
          paso_p1(3, "Explora los resultados"),
          tabsetPanel(
            # ===== A. Resultados =====
            tabPanel(tagList(icon("chart-column"), " Resultados"),
              tabsetPanel(type = "pills",
                tabPanel("Histograma",
                         que_veo_p1("Si el generador es bueno, las barras se parecen a la <b>línea punteada</b>: la densidad de una U(0,1) es constante e igual a 1."),
                         plotlyOutput(ns("histograma"), height = "400px")),
                tabPanel("Secuencia",
                         que_veo_p1("Cada punto es un número generado, en orden. La <b>zona sombreada</b> marca desde dónde la secuencia se repite (ciclo)."),
                         plotlyOutput(ns("secuencia"), height = "400px")),
                tabPanel("Dispersión 2D",
                         que_veo_p1("Cada punto es un par de números seguidos (u<sub>i</sub>, u<sub>i+1</sub>). Si son independientes, cubren el cuadrado <b>sin formar líneas ni patrones</b>."),
                         plotlyOutput(ns("dispersion"), height = "400px")),
                tabPanel("Dispersión 3D",
                         que_veo_p1("Ternas de números seguidos. <b>Gire el cubo con el mouse.</b> Pruebe el generador RANDU: sus puntos caen en solo 15 planos."),
                         plotlyOutput(ns("disp3d"), height = "500px")),
                tabPanel("Tabla de datos",
                         que_veo_p1("Cálculo de cada iteración, igual que en clase. Los datos se descargan desde el panel <b>④ Descarga los datos</b>."),
                         DTOutput(ns("tabla")))
              )
            ),

            # ===== B. Paso a paso =====
            tabPanel(tagList(icon("person-chalkboard"), " Paso a paso"), br(),
              que_veo_p1("Mueva el deslizador o pulse <b>▶</b> para ver cómo se calcula cada número, igual que en clase."),
              sliderInput(ns("paso"), "Iteración", min = 1, max = 10, value = 1, step = 1,
                          width = "100%", animate = animationOptions(interval = 1800)),
              div(class = "p1-proceso", uiOutput(ns("explicacion"))),
              h4(icon("chart-line"), " Ubicación del paso actual en la secuencia"),
              que_veo_p1("Se muestran los números cercanos al paso actual (punto grande). Al avanzar el deslizador, la ventana se desplaza."),
              plotlyOutput(ns("traza"), height = "340px")
            ),

            # ===== C. Análisis del generador =====
            tabPanel(tagList(icon("magnifying-glass-chart"), " Análisis del generador"), br(),
              h4(icon("infinity"), " ¿Tendrá periodo completo?"),
              que_veo_p1("El <b>Teorema de Hull-Dobell</b> dice si un MCL recorre los m valores posibles antes de repetirse."),
              uiOutput(ns("hull")),
              hr(),
              h4(icon("scale-balanced"), " ¿Son uniformes? Prueba Chi-cuadrado"),
              que_veo_p1("Divide [0, 1) en k intervalos y compara cuántos números cayeron en cada uno (<b>observado</b>) con los que deberían caer si fueran uniformes (<b>esperado</b> = N/k)."),
              fluidRow(
                column(3, numericInput(ns("k_chi"), "Intervalos (k)", value = 10, min = 2, max = 50),
                          selectInput(ns("alfa"), "Significancia α", choices = c(0.01, 0.05, 0.10), selected = 0.05)),
                column(9, uiOutput(ns("chi_decision")))
              ),
              plotlyOutput(ns("chi_grafico"), height = "360px"),
              h5(icon("table"), " Detalle por intervalo", style = "font-weight:700; margin-top:14px;"),
              div(style = "overflow-x:auto;", tableOutput(ns("chi_tabla")))
            ),

            # ===== D. Comparar métodos =====
            tabPanel(tagList(icon("code-compare"), " Comparar métodos"), br(),
              que_veo_p1("Se ejecutan los <b>tres algoritmos</b> con la misma semilla x₀ y el mismo N, usando los parámetros de cada método. Gana el que tenga errores más pequeños y no se repita."),
              DTOutput(ns("comp_tabla")),
              br(),
              plotlyOutput(ns("comp_hist"), height = "380px")
            )
          )
        )
      )
    )
  )
}


# ==============================================================
# SERVIDOR (cálculos y gráficos)
# ==============================================================
mod_p1_pseudoaleatorios_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    nombres <- c(mcl = "Congruencial Lineal", lehmer = "Lehmer", cm = "Cuadrados Medios")

    # ---------- 0. Descripción del método elegido y botón ----------
    output$info_metodo <- renderUI({
      m <- METODOS_P1[[input$metodo]]
      div(class = "p1-info",
          style = paste0("background:", m$suave, "66; border-left:6px solid ", m$fuerte, ";"),
          fluidRow(
            column(7,
                   h4(style = paste0("margin-top:0; font-weight:700; color:", PALETA$noche, ";"),
                      icon(m$icono, style = paste0("color:", m$fuerte, ";")), paste0(" ¿Cómo funciona: ", m$nombre, "?")),
                   div(style = "font-size:14px; line-height:1.5;", HTML(m$idea))),
            column(5,
                   div(class = "p1-formula", HTML(m$formula)),
                   div(class = "p1-chip", icon("thumbs-up", style = paste0("color:", PALETA$menta, ";")),
                       " ", m$ventaja),
                   div(class = "p1-chip", icon("triangle-exclamation", style = paste0("color:", PALETA$naranja, ";")),
                       " ", m$cuidado))
          ))
    })

    output$boton <- renderUI({
      m <- METODOS_P1[[input$metodo]]
      actionButton(session$ns("generar"), "Generar números", icon = icon("wand-magic-sparkles"),
                   class = "p1-boton", width = "100%",
                   style = paste0("background: linear-gradient(90deg, ", m$fuerte, ", ", PALETA$indigo, ");"))
    })

    # Estilo común de los gráficos
    estilo_p1 <- function(g) {
      g %>% layout(font = list(family = "Segoe UI, Helvetica, Arial", color = PALETA$noche),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   legend = list(orientation = "h", y = -0.22, yanchor = "top")) %>%
        config(displaylogo = FALSE, locale = "es")
    }
    color_metodo <- reactive(METODOS_P1[[datos()$clave]]$fuerte)

    # ---------- 1. Generadores predefinidos y ejemplo del anexo ----------
    observeEvent(input$preset, {
      if (input$preset != "pers") {
        p <- PRESETS_MCL[[input$preset]]
        updateNumericInput(session, "x0", value = p$x0)
        updateNumericInput(session, "a", value = p$a)
        updateNumericInput(session, "c_mcl", value = p$c)
        updateNumericInput(session, "m", value = p$m)
        updateNumericInput(session, "N", value = p$N)
      }
    })

    observeEvent(input$ejemplo, {
      updateNumericInput(session, "x0", value = 4122)
      updateNumericInput(session, "n_leh", value = 4)
      updateNumericInput(session, "c_leh", value = 76)
      updateSelectInput(session, "n_cm", selected = 4)
      updateNumericInput(session, "N", value = 6)
    })

    # ---------- 2. Generar los números ----------
    ejecutar <- function(metodo) {
      tryCatch({
        if (metodo == "mcl") {
          gen_mcl(input$x0, input$a, input$c_mcl, input$m, input$N)
        } else if (metodo == "lehmer") {
          gen_lehmer(input$x0, input$c_leh, input$n_leh, input$N)
        } else {
          gen_cuadrados_medios(input$x0, as.numeric(input$n_cm), input$N)
        }
      }, error = function(e) list(error = conditionMessage(e)))
    }

    resultado <- eventReactive(input$generar, {
      req(input$x0, input$N)
      r <- ejecutar(input$metodo)
      r$clave  <- input$metodo
      r$metodo <- nombres[[input$metodo]]
      r$param  <- list(a = input$a, c = input$c_mcl, m = input$m,
                       n_leh = input$n_leh, c_leh = input$c_leh, n_cm = as.numeric(input$n_cm))
      r
    }, ignoreNULL = FALSE)

    datos <- reactive({
      r <- resultado()
      validate(need(is.null(r$error), r$error))
      r
    })

    # Subíndices de u: el MCL empieza en 1, Lehmer y Cuadrados Medios en 0
    indices <- reactive({
      if (datos()$clave == "mcl") seq_along(datos()$u) else seq_along(datos()$u) - 1
    })

    # ---------- 3. Avisos ----------
    output$avisos <- renderUI({
      r <- resultado()
      if (!is.null(r$error)) {
        div(class = "alert alert-danger", icon("circle-xmark"), " ", r$error)
      } else if (!is.null(r$aviso)) {
        div(class = "alert alert-warning", icon("triangle-exclamation"), " ", r$aviso)
      }
    })

    # ---------- 4. Resumen estadístico ----------
    output$tarjetas <- renderUI({
      u <- datos()$u
      media    <- mean(u)
      varianza <- if (length(u) > 1) var(u) else NA
      periodo  <- datos()$periodo
      bien  <- PALETA$menta
      ojo   <- PALETA$naranja
      color_media <- if (abs(media - 0.5) < 0.02) bien else ojo
      color_var   <- if (!is.na(varianza) && abs(varianza - 1/12) < 0.008) bien else ojo

      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
          tarjeta_p1("list-ol", "NÚMEROS GENERADOS", length(u), datos()$metodo, color_metodo()),
          tarjeta_p1("bullseye", "MEDIA  E[X]", round(media, 4),
                     paste0("Teórica 0.5 · error ", num_p1(round(abs(media - 0.5), 4))), color_media),
          tarjeta_p1("arrows-left-right", "VARIANZA  V(X)", round(varianza, 4),
                     paste0("Teórica 1/12 ≈ 0.0833 · error ", num_p1(round(abs(varianza - 1/12), 4))), color_var),
          tarjeta_p1("rotate", "PERIODO", if (is.na(periodo)) "Sin ciclo" else periodo,
                     if (is.na(periodo)) "no se repitió en los N valores" else "valores antes de repetirse",
                     if (is.na(periodo)) bien else PALETA$rojo))
    })

    # ---------- 5. Histograma ----------
    output$histograma <- renderPlotly({
      u <- datos()$u
      plot_ly() %>%
        add_histogram(x = u, histnorm = "probability density", name = "Datos generados",
                      xbins = list(start = 0, end = 1, size = 1 / input$bins),
                      marker = list(color = transparente(color_metodo(), 0.8),
                                    line = list(color = "white", width = 1.5))) %>%
        add_lines(x = c(0, 1), y = c(1, 1), name = "Densidad U(0,1): f(x) = 1",
                  line = list(color = PALETA$rojo, width = 3, dash = "dash")) %>%
        layout(title = paste("Histograma –", datos()$metodo),
               xaxis = list(title = "u", range = c(0, 1)),
               yaxis = list(title = "Densidad"),
               bargap = 0.03) %>%
        estilo_p1()
    })

    # ---------- 6. Secuencia con el ciclo resaltado ----------
    output$secuencia <- renderPlotly({
      u <- datos()$u
      i <- indices()
      g <- plot_ly(x = i, y = u, type = "scatter", mode = "lines+markers", name = "u(i)",
                   line = list(color = color_metodo(), width = 1.2),
                   marker = list(size = 5, color = color_metodo()))
      formas <- list()
      if (!is.na(datos()$inicio)) {
        formas <- list(list(type = "rect", x0 = datos()$inicio, x1 = max(i), y0 = 0, y1 = 1,
                            fillcolor = transparente(PALETA$amarillo, 0.25), line = list(width = 0)))
      }
      g %>% layout(title = "Secuencia generada", shapes = formas,
                   xaxis = list(title = "i"), yaxis = list(title = "u(i)", range = c(0, 1))) %>%
        estilo_p1()
    })

    # ---------- 7. Dispersión 2D ----------
    output$dispersion <- renderPlotly({
      u <- datos()$u
      validate(need(length(u) > 2, "Se necesitan al menos 3 números."))
      n <- length(u)
      plot_ly(x = u[1:(n - 1)], y = u[2:n], type = "scatter", mode = "markers",
              marker = list(size = 6, color = transparente(color_metodo(), 0.65))) %>%
        layout(title = "Pares consecutivos",
               xaxis = list(title = "u(i)", range = c(0, 1)),
               yaxis = list(title = "u(i+1)", range = c(0, 1))) %>%
        estilo_p1()
    })

    # ---------- 8. Dispersión 3D ----------
    output$disp3d <- renderPlotly({
      u <- datos()$u
      n <- length(u)
      validate(need(n > 3, "Se necesitan al menos 4 números."))
      # Con RANDU se usa un ángulo desde el que se ven los 15 planos de canto
      es_randu <- datos()$clave == "mcl" && datos()$param$a == 65539 && datos()$param$m == 2^31
      camara <- if (es_randu) list(eye = list(x = 0.96, y = 1.63, z = 1.15), projection = list(type = "orthographic")) else NULL
      plot_ly(x = u[1:(n - 2)], y = u[2:(n - 1)], z = u[3:n], type = "scatter3d", mode = "markers",
              marker = list(size = 2.5, color = u[3:n],
                            colorscale = list(c(0, PALETA$celeste), c(0.5, PALETA$indigo), c(1, PALETA$orquidea)))) %>%
        layout(title = "Ternas consecutivas (u(i), u(i+1), u(i+2))",
               scene = list(camera = camara, xaxis = list(title = "u(i)"), yaxis = list(title = "u(i+1)"),
                            zaxis = list(title = "u(i+2)"))) %>%
        estilo_p1()
    })

    # ---------- 9. Tabla paso a paso ----------
    output$tabla <- renderDT({
      tabla <- datos()$tabla
      datatable(tabla, rownames = FALSE, colnames = nombres_columnas(names(tabla)), options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatRound(columns = ncol(tabla), digits = 4)
    })

    # ---------- 10. Paso a paso (modo didáctico) ----------
    observeEvent(datos(), {
      updateSliderInput(session, "paso", max = min(nrow(datos()$tabla), 300), value = 1)
    })

    output$explicacion <- renderUI({
      req(input$paso)
      r <- datos()
      t <- r$tabla
      f <- min(input$paso, nrow(t))
      p <- r$param

      if (r$clave == "mcl") {
        n <- t$n[f]
        q <- floor(t$ax_mas_c[f] / p$m)
        texto <- paste0(
          "<h4>Iteración n = ", n, "</h4>",
          formula_p1("x<sub>n</sub> = (a · x<sub>n−1</sub> + c) mod m"),
          formula_p1("x<sub>", n, "</sub> = (", num_p1(p$a), " · ", num_p1(t$x_anterior[f]), " + ",
                     num_p1(p$c), ") mod ", num_p1(p$m)),
          "<p>Se divide para m y el <b>residuo</b> es el nuevo número:</p>",
          formula_p1(num_p1(t$ax_mas_c[f]), " = ", num_p1(q), " · ", num_p1(p$m), " + ",
                     caja_p1(num_p1(t$x_n[f]))),
          formula_p1("u<sub>", n, "</sub> = x<sub>", n, "</sub> / m = ", num_p1(t$x_n[f]), " / ",
                     num_p1(p$m), " = <b>", round(t$u_n[f], 6), "</b>"))

      } else if (r$clave == "lehmer") {
        i <- t$i[f]
        k <- nchar(num_p1(p$c_leh))
        prod_txt <- ceros_p1(t$x_por_c[f], p$n_leh + k)
        texto <- paste0(
          "<h4>Iteración i = ", i, "</h4>",
          formula_p1("x<sub>", i, "</sub> · c = ", num_p1(t$x_i[f]), " · ", num_p1(p$c_leh), " = ",
                     num_p1(t$x_por_c[f])),
          "<p>Se separan las <b style='color:", PALETA$coral, "'>k = ", k, " cifras de la izquierda</b> y las ",
          "<b style='color:", PALETA$indigo, "'>n = ", p$n_leh, " cifras de la derecha</b>:</p>",
          "<div style='text-align:center; margin:10px 0;'>",
          cifras_html(substr(prod_txt, 1, k), 1, k, PALETA$coral),
          "<span style='font-size:26px; margin:0 6px;'>|</span>",
          cifras_html(substr(prod_txt, k + 1, nchar(prod_txt)), 1, p$n_leh, PALETA$indigo), "</div>",
          formula_p1("x<sub>", i + 1, "</sub> = derecha − izquierda = ", ceros_p1(t$derecha[f], p$n_leh), " − ",
                     ceros_p1(t$izquierda[f], k), " = ", caja_p1(num_p1(t$x_siguiente[f]))),
          formula_p1("u<sub>", i, "</sub> = x<sub>", i, "</sub> / 10<sup>", p$n_leh, "</sup> = ",
                     num_p1(t$x_i[f]), " / 10<sup>", p$n_leh, "</sup> = <b>", round(t$u_i[f], 6), "</b>"))

      } else {
        i <- t$i[f]
        n <- p$n_cm
        cuad_txt <- ceros_p1(t$x_i_cuad[f], 2 * n)
        texto <- paste0(
          "<h4>Iteración i = ", i, "</h4>",
          formula_p1("x<sub>", i, "</sub><sup>2</sup> = ", ceros_p1(t$x_i[f], n), "<sup>2</sup> = ",
                     num_p1(t$x_i_cuad[f])),
          "<p>Se completa a <b>2n = ", 2 * n, "</b> cifras y se toman las <b style='color:", PALETA$violeta,
          "'>n = ", n, " cifras centrales</b>:</p>",
          "<div style='text-align:center; margin:10px 0;'>", cifras_html(cuad_txt, n / 2 + 1, n / 2 + n, PALETA$violeta), "</div>",
          formula_p1("x<sub>", i + 1, "</sub> = ", caja_p1(ceros_p1(t$centrales[f], n))),
          formula_p1("u<sub>", i, "</sub> = x<sub>", i, "</sub> / 10<sup>", n, "</sup> = ",
                     ceros_p1(t$x_i[f], n), " / 10<sup>", n, "</sup> = <b>", round(t$u_i[f], 6), "</b>"))
      }
      HTML(texto)
    })

    output$traza <- renderPlotly({
      req(input$paso)
      u <- datos()$u
      i <- indices()
      f <- min(input$paso, length(u))
      # ventana de 25 valores antes y después del paso actual
      ver <- max(1, f - 25):min(length(u), f + 25)
      plot_ly() %>%
        add_trace(x = i[ver], y = u[ver], type = "scatter", mode = "lines+markers", name = "u(i)",
                  line = list(color = PALETA$p_lavanda, width = 2), marker = list(size = 7, color = PALETA$p_lavanda),
                  hovertemplate = "i = %{x}<br>u = %{y:.4f}<extra></extra>") %>%
        add_trace(x = i[f], y = u[f], type = "scatter", mode = "markers", name = "Paso actual",
                  marker = list(size = 18, color = color_metodo(), line = list(color = "white", width = 3))) %>%
        layout(showlegend = FALSE,
               xaxis = list(title = "i"), yaxis = list(title = "u(i)", range = c(0, 1))) %>%
        estilo_p1()
    })

    # ---------- 11. Teorema de Hull-Dobell ----------
    output$hull <- renderUI({
      r <- datos()
      if (r$clave != "mcl") {
        return(div(class = "alert alert-info",
                   icon("circle-info"), " El Teorema de Hull-Dobell aplica solo al Método Congruencial Lineal. ",
                   "En ", r$metodo, " el periodo depende de la semilla y no hay una regla general; ",
                   "por eso estos métodos suelen caer en ciclos cortos. Periodo observado: ",
                   strong(if (is.na(r$periodo)) "sin repetición en los N valores" else r$periodo), "."))
      }
      p <- r$param
      if (p$c == 0) {
        primo <- length(factores_primos(p$m)) == 1 && factores_primos(p$m)[1] == p$m
        return(div(class = "alert alert-info", icon("circle-info"), HTML(paste0(
          " Con <b>c = 0</b> el generador es <b>multiplicativo</b> y Hull-Dobell no aplica. ",
          "Su periodo máximo es <b>m − 1</b>, y se alcanza cuando m es primo y a es raíz primitiva módulo m. ",
          "Aquí m <b>", if (primo) "sí es primo" else "no es primo", "</b>."))))
      }

      hd <- hull_dobell(p$a, p$c, p$m)
      filas <- list()
      for (j in 1:nrow(hd)) {
        ok <- hd$Cumple[j]
        filas[[j]] <- div(style = paste0("flex:1; min-width:220px; border-left:5px solid ", if (ok) PALETA$menta else PALETA$rojo,
                                         "; background:#f7f8fc; padding:10px 14px; border-radius:10px;"),
                          icon(if (ok) "circle-check" else "circle-xmark",
                               style = paste0("color:", if (ok) PALETA$menta else PALETA$rojo)),
                          strong(" ", hd$Condicion[j]), br(),
                          span(style = "font-size:13px; color:#555;", hd$Detalle[j]))
      }
      completo <- all(hd$Cumple)
      tagList(
        div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:12px;", filas),
        div(class = if (completo) "alert alert-success" else "alert alert-warning",
            if (completo) tagList(icon("trophy"), " Cumple las tres condiciones: el periodo es completo, ",
                                  strong("periodo = m = ", num_p1(p$m)), ".")
            else tagList(icon("triangle-exclamation"),
                         " No cumple todas las condiciones: el periodo será ", strong("menor que m"), "."))
      )
    })

    # ---------- 12. Prueba Chi-cuadrado ----------
    chi <- reactive({
      req(input$k_chi)
      validate(need(length(datos()$u) >= 5 * input$k_chi,
                    "Se recomienda N ≥ 5·k para que la prueba Chi-cuadrado sea válida."))
      chi_cuadrado_uniforme(datos()$u, input$k_chi, as.numeric(input$alfa))
    })

    output$chi_decision <- renderUI({
      ch <- chi()
      div(class = if (ch$rechaza) "alert alert-danger" else "alert alert-success",
          HTML(paste0(
            formula_p1("χ<sup>2</sup> = Σ (O<sub>j</sub> − E<sub>j</sub>)<sup>2</sup> / E<sub>j</sub> = <b>",
                       round(ch$chi2, 4), "</b> &nbsp;&nbsp; vs. &nbsp;&nbsp; χ<sup>2</sup><sub>", ch$gl, "; ",
                       1 - as.numeric(input$alfa), "</sub> = ", round(ch$critico, 4)),
            "<b>Valor p ", if (ch$p_valor < 0.0001) "< 0.0001" else paste("=", round(ch$p_valor, 4)), "</b> → ",
            if (ch$rechaza) "Se <b>rechaza</b> H₀: los números NO parecen uniformes."
            else "<b>No se rechaza</b> H₀: los números son compatibles con U(0,1).")))
    })

    output$chi_grafico <- renderPlotly({
      ch <- chi()
      plot_ly() %>%
        add_bars(x = ch$tabla$Intervalo, y = ch$tabla$Observada, name = "Observada",
                 marker = list(color = transparente(color_metodo(), 0.85))) %>%
        add_lines(x = ch$tabla$Intervalo, y = ch$tabla$Esperada, name = "Esperada",
                  line = list(color = PALETA$rojo, dash = "dash", width = 3)) %>%
        layout(xaxis = list(title = "", categoryorder = "array", categoryarray = ch$tabla$Intervalo),
               yaxis = list(title = "Frecuencia"), legend = list(orientation = "h", y = 1.15, yanchor = "bottom")) %>%
        estilo_p1()
    })

    output$chi_tabla <- renderTable(chi()$tabla, striped = TRUE, hover = TRUE, digits = 2, width = "100%")

    # ---------- 13. Comparar los tres métodos ----------
    comparacion <- eventReactive(input$generar, {
      req(input$x0, input$N)
      lista <- list()
      for (clave in names(nombres)) lista[[clave]] <- ejecutar(clave)
      lista
    }, ignoreNULL = FALSE)

    tabla_comparacion <- reactive({
      lista <- comparacion()
      filas <- list()
      for (clave in names(lista)) {
        r <- lista[[clave]]
        if (!is.null(r$error)) {
          filas[[clave]] <- data.frame(Método = nombres[[clave]], N = NA, Media = NA, Error_media = NA,
                                       Varianza = NA, Error_varianza = NA, Periodo = r$error,
                                       p_valor_Chi2 = NA)
        } else {
          u <- r$u
          p_chi <- if (length(u) >= 50) chi_cuadrado_uniforme(u, 10)$p_valor else NA
          p_chi <- if (is.na(p_chi)) "—" else if (p_chi < 1e-4) "< 0.0001" else format(round(p_chi, 4), nsmall = 4)
          filas[[clave]] <- data.frame(Método = nombres[[clave]], N = length(u),
                                       Media = round(mean(u), 4), Error_media = round(abs(mean(u) - 0.5), 4),
                                       Varianza = round(var(u), 4), Error_varianza = round(abs(var(u) - 1/12), 4),
                                       Periodo = if (is.na(r$periodo)) "Sin repetición" else as.character(r$periodo),
                                       p_valor_Chi2 = p_chi)
        }
      }
      do.call(rbind, filas)
    })

    output$comp_tabla <- renderDT({
      datatable(tabla_comparacion(), rownames = FALSE, options = list(dom = "t", ordering = FALSE),
                colnames = c("Método", "N", "Media", "|Media − 0.5|", "Varianza", "|Var − 1/12|",
                             "Periodo", "Valor p χ²"))
    })

    output$comp_hist <- renderPlotly({
      lista <- comparacion()
      colores <- c(mcl = PALETA$petroleo, lehmer = PALETA$durazno, cm = PALETA$violeta)
      g <- plot_ly()
      for (clave in names(lista)) {
        r <- lista[[clave]]
        if (is.null(r$error)) {
          g <- g %>% add_histogram(x = r$u, histnorm = "probability density", name = nombres[[clave]],
                                   xbins = list(start = 0, end = 1, size = 0.05), opacity = 0.55,
                                   marker = list(color = colores[[clave]]))
        }
      }
      g %>% add_lines(x = c(0, 1), y = c(1, 1), name = "U(0,1)",
                      line = list(color = PALETA$rojo, width = 3, dash = "dash")) %>%
        layout(barmode = "overlay", title = "Comparación de histogramas",
               xaxis = list(title = "u", range = c(0, 1)), yaxis = list(title = "Densidad", range = c(0, 3))) %>%
        estilo_p1()
    })

    # ---------- 14. Descargas ----------
    resumen <- reactive({
      u <- datos()$u
      data.frame(Medida = c("Método", "N", "E[X] muestral", "V(X) muestral",
                            "E[X] teórico", "V(X) teórico", "Periodo"),
                 Valor  = c(datos()$metodo, length(u), round(mean(u), 6), round(var(u), 6),
                            0.5, round(1 / 12, 6),
                            if (is.na(datos()$periodo)) "Sin repetición" else datos()$periodo))
    })

    output$desc_xlsx <- downloadHandler(
      filename = function() paste0("pseudoaleatorios_", datos()$clave, ".xlsx"),
      content  = function(file) {
        hojas <- list(Datos = datos()$tabla, Resumen = resumen())
        u <- datos()$u
        if (length(u) >= 20) {
          ch <- chi_cuadrado_uniforme(u, 10)
          hojas$Chi_cuadrado <- rbind(ch$tabla,
                                      data.frame(Intervalo = c("Chi2 calculado", "Valor crítico (α = 0.05)", "Valor p"),
                                                 Observada = NA, Esperada = NA,
                                                 Aporte = round(c(ch$chi2, ch$critico, ch$p_valor), 4)))
        }
        hojas$Comparacion <- tabla_comparacion()
        write_xlsx(hojas, file)
      }
    )
    output$desc_csv <- downloadHandler(
      filename = function() paste0("pseudoaleatorios_", datos()$clave, ".csv"),
      content  = function(file) write.csv(datos()$tabla, file, row.names = FALSE)
    )
    output$desc_txt <- downloadHandler(
      filename = function() paste0("pseudoaleatorios_", datos()$clave, ".txt"),
      content  = function(file) write.table(datos()$tabla, file, sep = "\t", row.names = FALSE, quote = FALSE)
    )
  })
}
