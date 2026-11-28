# Simulación Estocástica – Aplicativo Shiny

Trabajo Grupal N.º 1 - **Simulación** · Escuela Politécnica Nacional · Semestre 2026-B

**Autoras:** Sandy González · Erika Haro

**Aplicación en línea:** https://d83jj7-erika-haro.shinyapps.io/Proyecto1_Simulacion_EPN/

---

## Descripción

Aplicativo interactivo desarrollado en **R Shiny** que integra los métodos de simulación estocástica y numérica vistos en la materia. Todos los algoritmos se implementan desde sus fundamentos: las variables aleatorias se construyen a partir de números uniformes producidos por un **generador congruencial lineal propio**, sin usar `runif` como generador base.

La interfaz está organizada en 5 pestañas (`navbarPage`). Cada una tiene la misma estructura: (1) elegir el método, (2) configurar los parámetros, (3) explorar los resultados y (4) descargar los datos.

## Contenido de las pestañas

### 1. Generación de números pseudoaleatorios
- **Métodos:** Congruencial Lineal (MCL), Lehmer y Cuadrados Medios, implementados desde sus ecuaciones recurrentes.
- **Resultados:** histograma frente a la densidad U(0,1), tabla paso a paso, E[X] y V(X) comparados con sus valores teóricos.
- **Extras:** detección automática de ciclos y periodo; modo paso a paso animado; dispersión 2D y 3D (caso RANDU); generadores predefinidos (Park-Miller, RANDU, Numerical Recipes); verificación del Teorema de Hull-Dobell; prueba Chi-cuadrado; comparación de los tres métodos.

### 2. Integración por el método de Montecarlo
- **Entradas:** función f(x), límites [a, b] y número de simulaciones N.
- **Resultados:** gráfico geométrico, gráfico de convergencia con intervalo de confianza del 95%, estimación, error estándar y comparación con la integración numérica de referencia (`integrate`).
- **Extras:** métodos de media muestral y de acierto-error (con soporte para áreas negativas), gráfico del error frente a σ/√N y comparación de eficiencia entre los dos métodos.

### 3. Variables aleatorias discretas
- **Distribuciones:** Binomial, Poisson, Geométrica y personalizada, por el método de la transformación inversa discreta.
- **Validación:** en la distribución personalizada se verifica que las probabilidades sumen 1.
- **Resultados:** gráfico de barras empírico frente a teórico, función de distribución y resumen descriptivo.
- **Extras:** búsqueda ordenada por probabilidad (mejora de Ross), prueba Chi-cuadrado de ajuste y visualización del proceso de la transformación inversa.

### 4. Variables aleatorias continuas
- **Distribuciones:** Uniforme, Exponencial y Normal (Box-Muller o aceptación-rechazo de Ross).
- **Distribución personalizada:** el usuario ingresa la densidad f(x) y elige transformación inversa numérica o aceptación-rechazo.
- **Resultados:** histograma con la densidad empírica frente a la teórica y CDF empírica frente a la teórica.
- **Extras:** gráfico Q-Q, tasa de aceptación observada frente a la teórica, visualización de puntos aceptados y rechazados, normalización automática de f(x).

### 5. Pruebas de bondad de ajuste
- **Datos:** números generados en la aplicación o una muestra externa subida en `.csv` o `.xlsx`.
- **Pruebas:** Kolmogorov-Smirnov y prueba de rachas (sobre y bajo la mediana, y ascendentes y descendentes).
- **Resultados:** tabla con el estadístico, valor crítico, valor p y decisión para un nivel α configurable, más gráficos diagnósticos.
- **Extras:** función de autocorrelación, gráfico de pares consecutivos y distribución de las longitudes de las rachas.

**Exportación:** todas las pestañas permiten descargar los datos en `.xlsx`, `.csv` y `.txt`.

## Estructura del repositorio

```
├── app.R                          Ensambla las 5 pestañas (navbarPage), estilos y pie de página
├── README.md
└── R/                             Shiny carga automáticamente todos los archivos de esta carpeta
    ├── 00_paleta.R                Paleta de colores y funciones de estilo compartidas
    ├── generadores.R              MCL, Lehmer y Cuadrados Medios
    ├── analisis_generadores.R     Teorema de Hull-Dobell y prueba Chi-cuadrado
    ├── generadores_continuas.R    Fuente de uniformes propia, transformación inversa, Box-Muller, aceptación-rechazo
    ├── montecarlo.R               Métodos de media muestral y acierto-error
    ├── discretas.R                Transformación inversa discreta y Chi-cuadrado
    ├── bondad_ajuste.R            Kolmogorov-Smirnov, rachas, autocorrelación y lectura de archivos
    ├── mod_p1_pseudoaleatorios.R  Pestaña 1
    ├── mod_p2_montecarlo.R        Pestaña 2
    ├── mod_p3_discretas.R         Pestaña 3
    ├── mod_p4_continuas.R         Pestaña 4
    └── mod_p5_bondad_ajuste.R     Pestaña 5
```

## Ejecución local

### Requisitos
- [R](https://cran.r-project.org/) versión 4.1 o superior
- [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado)

### Pasos

1. **Descargar el repositorio**, de una de estas formas:
   - En GitHub, botón **Code → Download ZIP**, y descomprimir la carpeta.
   - O con Git:
     ```bash
     git clone https://github.com/sandymeliz/Proyecto1_Simulacion_EPN.git
     ```

2. **Instalar los paquetes** (solo la primera vez). En la consola de R:
   ```r
   install.packages(c("shiny", "plotly", "DT", "writexl", "readxl"))
   ```

3. **Ejecutar la aplicación**, de una de estas formas:
   - En RStudio, abrir `app.R` y pulsar **Run App**.
   - O desde la consola de R, ubicándose en la carpeta del proyecto:
     ```r
     shiny::runApp()
     ```

La aplicación se abre en el navegador.

### Paquetes utilizados

| Paquete   | Uso |
|-----------|-----|
| `shiny`   | Estructura del aplicativo |
| `plotly`  | Gráficos interactivos |
| `DT`      | Tablas interactivas |
| `writexl` | Descarga en Excel |
| `readxl`  | Lectura de archivos `.xlsx` en la Pestaña 5 |

Los algoritmos de simulación usan solo funciones básicas de R.

## Trabajo colaborativo

El desarrollo se organizó con ramas y pull requests en GitHub. Cada pestaña se trabajó en su propia rama y fue revisada por la otra integrante antes de integrarse a `master`.

| Integrante     | Aporte |
|----------------|--------|
| Sandy González | Estructura base, Pestaña 1, Pestaña 4, diseño visual |
| Erika Haro     | Pestaña 2, Pestaña 3, Pestaña 5 |

## Referencias

- Ross, S. M. *Simulación*, 2.ª ed. Prentice Hall.
- Material del curso de Simulación, Escuela Politécnica Nacional.
