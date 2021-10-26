# Paquetes
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(shiny)
library(shinydashboard)


# Datos

# Lectura de una capa vectorial (GeoJSON) de provincias de Costa Rica
provincias <-
  st_read(
    "https://github.com/tpb728O-programaciongeoespacialr/2021ii/raw/main/datos/ign/delimitacion-territorial-administrativa/provincias-simplificadas_100m.geojson",
    quiet = TRUE
  )
# Transformación del CRS del objeto provincias
provincias <-
  provincias %>%
  st_transform(4326)

# Lectura de un archivo CSV con registros de presencia de felinos en Costa Rica
felidae <-
  st_read(
    "/vsicurl/https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/main/datos/gbif/felidae.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )
# Asignación de un CRS al objeto felidae
st_crs(felidae) <- 4326

# Lectura de una capa raster de altitud
altitud <-
  rast(
    "/vsicurl/https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/master/datos/worldclim/altitud.tif"
  )


# Lista ordenada de especies + "Todas"
lista_especies <- unique(felidae$species)
lista_especies <- sort(lista_especies)
lista_especies <- c("Todas", lista_especies)

# Lista ordenada de provincias + "Todas"
lista_provincias <- unique(felidae$stateProvince)
lista_provincias <- sort(lista_provincias)
lista_provincias <- c("Todas", lista_provincias)


# Componentes de la aplicación Shiny
# Definición del objeto ui
ui <-
  dashboardPage(
    dashboardHeader(title = "Felidae de Costa Rica"),
    dashboardSidebar(sidebarMenu(
      menuItem(
        text = "Filtros",
        selectInput(
          inputId = "especie",
          label = "Especie",
          choices = lista_especies,
          selected = "Todas"
        ),
        selectInput(
          inputId = "provincia",
          label = "Provincia",
          choices = lista_provincias,
          selected = "Todas"
        ),
        dateRangeInput(
          inputId = "fecha",
          label = "Fecha",
          start = "1800-01-01",
          end   = Sys.Date(),
          separator = " a ",
          language = "es"
        ),
        startExpanded = TRUE
      )
    )),
    dashboardBody(fluidRow(
      box(
        title = "Mapa de distribución",
        leafletOutput(outputId = "mapa"),
        width = 6
      ),
      box(
        title = "Registros de presencia",
        DTOutput(outputId = "tabla"),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "Estacionalidad",
        plotlyOutput(outputId = "grafico_estacionalidad"),
        width = 12
      )      
    ))
  )

# Definición de la función server
server <- function(input, output, session) {
  filtrarRegistros <- reactive({
    # Remoción de geometrías y selección de columnas
    felidae_filtrado <-
      felidae %>%
      dplyr::select(species, stateProvince, eventDate)
    
    # Filtrado de felidae por fecha
    felidae_filtrado <-
      felidae_filtrado %>%
      filter(
        eventDate >= as.Date(input$fecha[1], origin = "1970-01-01") &
          eventDate <= as.Date(input$fecha[2], origin = "1970-01-01")
      )
    # Filtrado de felidae por especie
    if (input$especie != "Todas") {
      felidae_filtrado <-
        felidae_filtrado %>%
        filter(species == input$especie)
    }
    # Filtrado de felidae por provincia
    if (input$provincia != "Todas") {
      felidae_filtrado <-
        felidae_filtrado %>%
        filter(stateProvince == input$provincia)
    }
    
    return(felidae_filtrado)
  })
  
  output$mapa <- renderLeaflet({
    registros <-
      filtrarRegistros()
    
    # Conversión del objeto altitud a la clase RasterLayer
    altitud_rl <- raster::raster(altitud)
    
    # Mapa Leaflet con capas de provincias y registros de presencia de felinos
    leaflet() %>%
      setView(lng = -84.19452,
              lat = 9.572735,
              zoom = 7) %>%
      addTiles() %>%
      addRasterImage(altitud_rl,
                     opacity = 0.6) %>%
      addPolygons(
        data = provincias,
        color = "black",
        fillColor = "transparent",
        stroke = TRUE,
        weight = 1.0,
      ) %>%
      addCircleMarkers(
        data = registros,
        stroke = TRUE,
        radius = 4,
        fillColor = 'red',
        fillOpacity = 1,
        label = paste0(
          registros$species,
          ", ",
          registros$stateProvince,
          ", ",
          registros$eventDate
        )
      )
  })
  
  output$tabla <- renderDT({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      datatable()
  })
  
  output$grafico_estacionalidad <- renderPlotly({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      group_by(mes = format(as.Date(eventDate, "%Y-%m-%d"), "%m")) %>%
      summarize(suma_registros = n()) %>%
      filter(!is.na(mes))  %>%
      plot_ly(
        x = ~ mes,
        y = ~ suma_registros,
        type = "scatter",
        mode = "markers",
        fill = "tozeroy",
        fillcolor = "green"
      ) %>%
      layout(
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Cantidad de registros")
      )
    
  })
}

# Llamado a la función shinyApp()
shinyApp(ui, server)