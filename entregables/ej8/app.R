#
# NYT Book Explorer: Tendencias literarias
# App de Shiny para explorar la lista de best sellers
# del New York Times (1931-2020).
#
# Para ejecutarla, haz clic en 'Run App' en RStudio.
# Más información: https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# ---- Lectura y preparación de datos ------------------------------------------
# Cada fila es un libro que entró a la lista de best sellers.
#   year        = año en que el libro debutó en la lista
#   total_weeks = semanas totales que permaneció en la lista
#   best_rank   = mejor posición que alcanzó (1 = primer lugar)
datos <- read.csv("data/nyt_titles.csv")
datos$author[is.na(datos$author) | datos$author == ""] <- "Autor desconocido"

anio_min <- min(datos$year, na.rm = TRUE)
anio_max <- max(datos$year, na.rm = TRUE)

# Textos de cada métrica (se usan en el selector y en las etiquetas del eje y)
metricas <- c(
  "Número de libros que debutaron en la lista" = "n_libros",
  "Semanas promedio que duraron en la lista"   = "semanas_prom"
)
etiquetas_y <- c(
  n_libros     = "Libros que debutaron en la lista",
  semanas_prom = "Semanas promedio en la lista"
)

# Calcula la métrica elegida para cada año
resumir_por_anio <- function(df, metrica) {
  if (metrica == "n_libros") {
    res <- aggregate(total_weeks ~ year, data = df, FUN = length)
  } else {
    res <- aggregate(total_weeks ~ year, data = df, FUN = mean)
  }
  names(res) <- c("year", "valor")
  res
}

# ---- Interfaz de usuario (UI) -------------------------------------------------
ui <- fluidPage(

  titlePanel("NYT Book Explorer: Tendencias literarias"),

  sidebarLayout(
    sidebarPanel(
      helpText(
        "Explora los libros que entraron a la lista de best sellers",
        "del New York Times entre ", anio_min, " y ", anio_max, "."
      ),

      sliderInput("anios",
                  "Periodo (año de debut en la lista):",
                  min = anio_min, max = anio_max,
                  value = c(anio_min, anio_max),
                  step = 1, sep = ""),

      selectInput("metrica",
                  "¿Qué quieres ver por año?",
                  choices = metricas),

      checkboxInput("solo_top1",
                    "Mostrar solo libros que llegaron al 1er lugar",
                    value = FALSE),

      hr(),
      sliderInput("top_n",
                  "Autores a mostrar en el ranking:",
                  min = 5, max = 20, value = 10, step = 1)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Tendencia por año",
                 br(),
                 htmlOutput("resumen"),
                 plotOutput("grafica_libros", height = "420px")),
        tabPanel("Autores con más best sellers",
                 br(),
                 plotOutput("grafica_autores", height = "480px"))
      )
    )
  )
)

# ---- Servidor (SERVER) --------------------------------------------------------
server <- function(input, output, session) {

  # Datos filtrados según los controles; lo usan todas las salidas
  datos_filtrados <- reactive({
    df <- subset(
      datos,
      year >= input$anios[1] &
        year <= input$anios[2]
    )
    if (input$solo_top1) {
      df <- df[df$best_rank == 1, ]
    }
    validate(need(nrow(df) > 0,
                  "No hay libros en este periodo con estos filtros. Prueba ampliar el periodo."))
    df
  })

  periodo <- reactive(paste0(input$anios[1], "–", input$anios[2]))

  # Resumen en texto
  output$resumen <- renderUI({
    df <- datos_filtrados()
    top <- df[which.max(df$total_weeks), ]
    HTML(paste0(
      "<p>En <b>", periodo(), "</b> hay <b>", nrow(df), "</b> libros de <b>",
      length(unique(df$author)), "</b> autores. En promedio duraron <b>",
      round(mean(df$total_weeks), 1), "</b> semanas en la lista y el ",
      round(mean(df$best_rank == 1) * 100, 1), "% llegó al 1er lugar. ",
      "El más duradero: <i>", tools::toTitleCase(tolower(top$title)), "</i> (",
      top$author, ", ", top$total_weeks, " semanas).</p>"
    ))
  })

  # Gráfica 1: tendencia por año de la métrica elegida
  output$grafica_libros <- renderPlot({
    df <- datos_filtrados()
    res <- resumir_por_anio(df, input$metrica)
    nombre_metrica <- names(metricas)[metricas == input$metrica]

    ggplot(res, aes(x = year, y = valor)) +
      geom_col(fill = "#0077B6") +
      labs(
        title = paste0(nombre_metrica, " (", periodo(), ")"),
        subtitle = "Cada libro se cuenta en el año en que debutó en la lista",
        x = "Año de debut en la lista",
        y = etiquetas_y[[input$metrica]],
        caption = "Fuente: lista de best sellers del New York Times"
      ) +
      theme_minimal(base_size = 14)
  })

  # Gráfica 2: autores con más libros en la lista
  output$grafica_autores <- renderPlot({
    df <- datos_filtrados()
    conteo <- aggregate(
      cbind(libros = 1, semanas = df$total_weeks) ~ author,
      data = df, FUN = sum
    )
    conteo <- head(conteo[order(-conteo$libros, -conteo$semanas), ], input$top_n)

    ggplot(conteo, aes(x = libros, y = reorder(author, libros), fill = semanas)) +
      geom_col() +
      geom_text(aes(label = libros), hjust = -0.3, size = 4) +
      scale_fill_gradient(low = "#90E0EF", high = "#03045E") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = paste0("Autores con más libros en la lista (", periodo(), ")"),
        x = "Libros que entraron a la lista",
        y = NULL,
        fill = "Semanas totales\nen la lista",
        caption = "Fuente: lista de best sellers del New York Times"
      ) +
      theme_minimal(base_size = 14)
  })

}

# ---- Ejecutar la App ----------------------------------------------------------
shinyApp(ui = ui, server = server)
