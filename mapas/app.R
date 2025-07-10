library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(mapSpain)
library(bslib)
library(ggnewscale)
library(shinycssloaders)

#---------------------Pre-carga de datos----------------------------------------------
CCAA_sf <- esp_get_ccaa(moveCAN = FALSE)
CCAA_sf <- st_transform(CCAA_sf, 3035)
municipios <- esp_get_munic(moveCAN = FALSE)
municipios <- st_transform(municipios, 3035)

hypsobath <- esp_get_hypsobath() #Obtenemos la hipsobatimetria

# hay que corregir un error en los datos de origen
# Remove:
hypsobath <- hypsobath[!sf::st_is_empty(hypsobath), ] #quitamos lo que esta vacio 

hypsobath <- st_transform(hypsobath, 3035)

hypsobath_depths <- hypsobath[hypsobath$val_inf < 0, ]
# Colores a patir de Wikipedia
# https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps
bath_tints <- colorRampPalette( 
  rev(
    c(
      "#ACDBFB",
      "#8DC1EA", "#84B9E3", "#79B2DE",
      "#71ABD8"
    )
  )
) #Genero una paleta de colores para las capas del mar 



ui <- page_sidebar(
  
  ##--------------------Titulo de la aplicacion---------------------
  title = "-CartoAmbiente- ",
  
  sidebar = sidebar(
    ##-----------------Selector de comunidad------------------------
    selectInput("comunidad", "Selecciona una comunidad autónoma:",
                choices = c("-" = "", sort(unique(CCAA_sf$ccaa.shortname.es))), 
                selected = NULL),
    ##-----------------Selector de Municipio------------------------
    selectizeInput("municipio", "Selecciona un municipio:",
                   choices = NULL,
                   selected = NULL,
                   options = list(placeholder = 'Escriba un municipio',
                                  maxOptions = 2300)),
    input_dark_mode(id = "mode", mode = "light")
  ),
  
  ##--------------------Panel principal----------------------------
  div(
    id = "main-panel",
    
    ##------ Pantalla de bienvenida (condicional) ------
    conditionalPanel(
      condition = "!output.showMapPanels",
      div(
        style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
        img(
          src = "Logoshiny.png",
          style = "max-width: 200px; margin-bottom: 1em;"
        ),
        h2("Bienvenido a CartoAmbiente"),
        p("CartoAmbiente es una aplicación diseñada para generar mapas ambientales a nivel municipal en toda España."),
        p("Su objetivo es facilitar el acceso a la cartografía ambiental de manera intuitiva y accesible para todos."),
        p("Selecciona una comunidad autonoma y un municipio para comenzar a explorar.")
      )
    ),
    
    ##------ Paneles de contenido con mapas ------
    conditionalPanel(
      condition = "output.showMapPanels",
      tabsetPanel(
        tabPanel("Usos del suelo",
                 withSpinner(
                   plotOutput("Corine"), type = 4, color = "#2c7a7b", 
                   ),
                 br(),
                 uiOutput("textocorine"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$showMapPanels <- reactive({
    # Muestra los paneles solo si se ha seleccionado un municipio
    !is.null(input$municipio) && input$municipio != ""
  })
  outputOptions(output, "showMapPanels", suspendWhenHidden = FALSE)
  
  # Actualiza la lista de municipios según comunidad seleccionada
  observeEvent(input$comunidad, {
    req(input$comunidad != "")  
    
    cod_comunidad <- CCAA_sf %>% 
      filter(ccaa.shortname.es == input$comunidad) %>% 
      pull(codauto)
    
    municipios_filtrados <- municipios %>% 
      filter(codauto == cod_comunidad) 
    
    updateSelectizeInput(session, "municipio", 
                         choices = sort(unique(municipios_filtrados$name)),
                         selected = "",
                         server = TRUE)
    
  })
  
  #Variables reactivas
  
  nombre_corregido <- reactive({gsub(" ", "_", gsub("/", "o", input$municipio))})
  
  nombrecom_corregido <- reactive({gsub(" ", "%20", input$comunidad)})
  
  municipio_sf <- reactive ({
    cod_comunidad <- CCAA_sf %>%
      filter(ccaa.shortname.es == input$comunidad) %>%
      pull(codauto)
    
    municipio_filtrado <- municipios %>%
    filter(name == input$municipio, codauto == cod_comunidad)
    
    if(input$municipio %in% c("Palma de Mallorca", "Almería", "Ceuta", "Melilla")) {
      municipio <- municipio_filtrado %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON") %>%
        mutate(area = st_area(st_geometry(.))) %>%
        slice_max(area, n = 1)
    } else {
      municipio <- municipio_filtrado
    }
    
    municipio})
  
  lims <- reactive({
    bbox <- st_bbox(municipio_sf())
    x_diff <- bbox[3] - bbox[1]
    y_diff <- bbox[4] - bbox[2]
    max_diff <- max(x_diff, y_diff)
    x_buffer <- max_diff * 0.15
    y_buffer <- max_diff * 0.15
    
    list(
      xlim = c(bbox[1] - x_buffer, bbox[3] + x_buffer),
      ylim = c(bbox[2] - y_buffer, bbox[4] + y_buffer),
      n_x_breaks = max(2, round(x_buffer / 2000)),
      n_y_breaks = max(2, round(y_buffer / 2000))
    )
  })
  
  
  visible_area <- reactive({
    bbox <- st_bbox(municipio_sf())
    x_diff <- bbox[3] - bbox[1]
    y_diff <- bbox[4] - bbox[2]
    max_diff <- max(x_diff, y_diff)
    x_buffer <- max_diff * 0.15
    y_buffer <- max_diff * 0.15
    
    caja <- structure(
    c(
      xmin = bbox$xmin - x_buffer,
      ymin = bbox$ymin - y_buffer,
      xmax = bbox$xmax + x_buffer,
      ymax = bbox$ymax + y_buffer
    ),
    class = "bbox",
    crs = st_crs(municipio_sf())
    )
    
    visible_area <- st_as_sfc(st_bbox(caja, crs = st_crs(municipio_sf ())))
    })
  
  # Capa gris: diferencia entre área visible y municipio
  #
  # Unión del municipio para asegurar un solo polígono
  municipio_union <- reactive({
    st_union(municipio_sf())})
  
  # Capa gris: diferencia entre área visible y municipio
  area_fuera_municipio <- reactive({
    st_difference(visible_area(), municipio_union())})
  
  hypsobath_crop <- reactive({
    st_intersection(hypsobath_depths, visible_area())})
  
  capas_gg_comunes <- reactive({
    lims <- lims()
    
    list(
      annotation_scale(),
        coord_sf(datum = st_crs(3035), xlim = lims$xlim, ylim = lims$ylim),
      scale_x_continuous(breaks = pretty(lims$xlim, n = lims$n_x_breaks)),
      scale_y_continuous(breaks = pretty(lims$ylim, n = lims$n_y_breaks)),
      annotation_north_arrow(
        location = "tr",
        style = north_arrow_fancy_orienteering,
        pad_x = unit(0.2, "in"),
        pad_y = unit(0.3, "in")
      ),
        theme_minimal(base_family = "sans"),
        theme(
      plot.background = element_rect(color = "black", fill = "white", linewidth = 1),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 16)))})
  
  # Renderiza el mapa
  output$Corine <- renderPlot({
    
    validate(
      need(input$comunidad != "" && input$municipio != "", "")
    )
    
    # Solo si ambas selecciones están presentes, se valida la coherencia entre comunidad y municipio
    if (input$comunidad != "" && input$municipio != "") {
      validate(
        need({
          cod_com <- CCAA_sf %>%
            filter(ccaa.shortname.es == input$comunidad) %>%
            pull(codauto)
          mun <- municipios %>%
            filter(name == input$municipio, codauto == cod_com)
          nrow(mun) > 0
        }, "")
      )
    }
    
    
    # Normaliza nombre para la URL
    
    nombre_corregido <- nombre_corregido()
    nombrecom_corregido <- nombrecom_corregido()
 
    url_geojson <- paste0("https://github.com/Carlos5682/DatosCorine/raw/refs/heads/main/Capasfinales/Corine/", 
                          nombrecom_corregido, "/", nombre_corregido, ".geojson")
    
    corine_muni <- tryCatch({
      st_read(url_geojson, quiet = TRUE)
    }, error = function(e) {
      NULL
    })
    
    municipio_sf <- municipio_sf()
    
    lims <- lims()
    
    visible_area <- visible_area()
    
    # Capa gris: diferencia entre área visible y municipio
    area_fuera_municipio <- area_fuera_municipio() 
    
    hypsobath_crop <- hypsobath_crop()
    
    add_bath <- nrow(hypsobath_crop()) > 0     
    
    corine_muni <- st_intersection(corine_muni, visible_area)
    
    ## ordenamos los niveles de alturas
    levels <- sort(unique(hypsobath_crop$val_inf))
    
    p <- ggplot()       
    
    # 3a) Batimetría (solo si existe)
    if (add_bath) {
      p <- p +
        geom_sf(data = hypsobath_crop,
                aes(fill = as.factor(val_inf)),
                colour = NA) +
        scale_fill_manual(
          values = bath_tints(length(levels))) +
        guides(fill = "none") + # <-- Aquí quitas la leyenda 
        ggnewscale::new_scale_fill()               # reiniciar escala fill
    }
    
    p <- p +
      geom_sf(data = corine_muni, aes(fill = Uso), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(values = setNames(corine_muni$color, corine_muni$Uso)) +
      ggtitle(paste("Mapa de usos del suelo del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
      
    print(p)
  }, bg = "transparent")
  
  output$textocorine <- renderUI({
    
    validate(
      need(input$comunidad != "" && input$municipio != "", "")
    )
    
    # Solo si ambas selecciones están presentes, se valida la coherencia entre comunidad y municipio
    if (input$comunidad != "" && input$municipio != "") {
      validate(
        need({
          cod_com <- CCAA_sf %>%
            filter(ccaa.shortname.es == input$comunidad) %>%
            pull(codauto)
          mun <- municipios %>%
            filter(name == input$municipio, codauto == cod_com)
          nrow(mun) > 0
        }, "")
      )
    }
    
    
    
    
    nombre_corregido <- nombre_corregido()
    nombrecom_corregido <- nombrecom_corregido()
    
    url_geojson <- paste0("https://github.com/Carlos5682/DatosCorine/raw/refs/heads/main/Capasfinales/Corine/", 
                          nombrecom_corregido, "/", nombre_corregido, ".geojson")
    
    corine_muni <- tryCatch({
      st_read(url_geojson, quiet = TRUE)
    }, error = function(e) {
      NULL
    })
    
    
    municipio <- municipio_sf()
    corine_muni <- st_intersection(corine_muni, municipio)
    
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> select(Uso, color, area)
    
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- (uso_summary$area / total_area) * 100
    
    top_usos <- uso_summary |> arrange(desc(porcentaje)) |> slice(1:3)
    
    # Texto con fondo de color
    textos_top <- paste0(
      seq_len(nrow(top_usos)), 
      '. <span style="background-color:', top_usos$color, 
      '; color: black; padding: 4px 8px; border-radius: 6px;">',
      top_usos$Uso, 
      '</span> (', 
      round(top_usos$porcentaje, 2), '%)'
    )
    
    HTML(paste0(
      "<b>En el municipio de ", input$municipio, ", los principales usos del suelo son:</b><br><br>",
      paste(textos_top, collapse = "<br>")
    ))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
