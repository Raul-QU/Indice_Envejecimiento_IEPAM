library(shiny)

shinyUI(fluidPage(
  titlePanel("Índice de Envejecimiento en México"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("estado", "Selecciona un estado:", choices = unique(datos$NOMGEO)),
      hr(),
      h4("Configuración de Mapa"),
      selectInput("año_mapa", "Año del mapa:", choices = c(2019, 2023)),
      selectInput("dominio_mapa", "Dominio a mostrar:", choices = DOMINIOS)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen",
                 h3(textOutput("titulo_estado")),
                 tableOutput("tabla_resumen")),
        tabPanel("Gráfico Radar", plotOutput("grafico_radar")),
        tabPanel("Evolución 2019 2023", plotOutput("grafico_evolucion")),
        tabPanel("Comparativa con Nuevo León", plotOutput("comparativa_nl")),
        tabPanel("Mapa del Índice", leafletOutput("mapa_dominio", height = "600px"))
      )
    )
  )
))