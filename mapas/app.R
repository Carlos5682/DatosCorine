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

#----------------------Mensajes explicativos de los mapas-----------------------

info_corine_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  
  h4("Información general sobre el mapa de usos del suelo"),
  p("Este mapa se ha elaborado a partir de los datos del proyecto CORINE Land Cover 2018, una iniciativa europea que proporciona información armonizada sobre la ocupación y el uso del suelo en Europa."),
  p("CORINE (Coordination of Information on the Environment) clasifica el territorio europeo en distintas categorías de uso del suelo, lo que permite analizar la estructura del paisaje y su evolución temporal."),
  
  br(),
  
  h4("Presentación y niveles de clasificación"),
  p("El mapa está disponible en tres niveles jerárquicos de clasificación: el nivel 1 corresponde a las categorías más generales, mientras que el nivel 3 ofrece el mayor nivel de detalle. Esta estructura facilita distintos niveles de análisis, desde una visión panorámica hasta un estudio más específico."),
  
  br(),
  
  h4("Acceso a los datos"),
  p("Los datos utilizados para generar este mapa pueden descargarse desde el Centro de Descargas del CNIG (Centro Nacional de Información Geográfica). Están disponibles en: ",
    a("https://centrodedescargas.cnig.es/CentroDescargas/corine-land-cover",
      href = "https://centrodedescargas.cnig.es/CentroDescargas/corine-land-cover",
      target = "_blank")
  ),
  
  br(),
  
  h4("Interpretación del mapa"),
  p("Para facilitar la interpretación, bajo el mapa se muestran los usos del suelo mayoritarios en el municipio seleccionado, junto con su respectivo porcentaje. Esta información permite obtener una visión general rápida de cómo se distribuye el territorio.")
)





#---------------------------------------------UI---------------------------------------
ui <- page_sidebar(
  
  ##-------------------- Titulo de la aplicacion --------------------
  title = "CartoAmbiente",
  
  sidebar = sidebar(
    
    ##----------------- Selector de comunidad ------------------------
    selectInput(
      "comunidad",
      "Selecciona una comunidad autónoma:",
      choices = c("-" = "", sort(unique(CCAA_sf$ccaa.shortname.es))),
      selected = NULL
    ),
    
    ##----------------- Selector de Municipio ------------------------
    selectizeInput(
      "municipio",
      "Selecciona un municipio:",
      choices = NULL,
      selected = NULL,
      options = list(
        placeholder = 'Escriba un municipio',
        maxOptions = 2300
      )
    ),
    
    ##---------------------Selector de Modo Oscuro---------------------------
    input_dark_mode(id = "mode", mode = "light")
  ),
  
  ##-------------------- Panel principal ----------------------------
  div(
    id = "main-panel",
    
    ###---------------- Pantalla de bienvenida (condicional) ------
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
    
    ###--------------------- Paneles de contenido ----------------------------
    conditionalPanel(
      condition = "output.showMapPanels",
      tabsetPanel(
        ####----------------------Usos del suelo------------------------------
        tabPanel("Usos del suelo",
                 tabsetPanel(
                   #####-------- Corine nivel 1--------------------------------
                   tabPanel("Corine nivel 1",
                            info_corine_ui,
                            ######-----------Pantalla de carga-----------------
                            withSpinner(
                              plotOutput("Corine1"), type = 4, color = "#2c7a7b"
                            ),
                            br(),
                            ######-------------Resumen de usos-----------------
                            uiOutput("textocorine1"),
                            div(
                              style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                              img(
                                src = "Logoshiny.png",
                                style = "max-width: 200px; margin-bottom: 1em;"
                              )
                            )
                   ),
                   #####----------Corine nivel 2----------------
                   tabPanel("Corine nivel 2",
                            info_corine_ui,
                            ######-----------Pantalla de carga-----------------
                            withSpinner(
                              plotOutput("Corine2"), type = 4, color = "#2c7a7b"
                            ),
                            br(),
                            ######-------------Resumen de usos-----------------
                            uiOutput("textocorine2"),
                            div(
                              style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                              img(
                                src = "Logoshiny.png",
                                style = "max-width: 200px; margin-bottom: 1em;"
                              )
                            )
                   ),
                   
                   #####-------------------- Corine nivel 3----------------
                   tabPanel("Corine nivel 3",
                            info_corine_ui,
                            ######-------------Pantalla de carga---------------
                            withSpinner(
                              plotOutput("Corine"), type = 4, color = "#2c7a7b"
                            ),
                            br(),
                            ######-----------------Resumen de usos-------------
                            uiOutput("textocorine"),
                            div(
                              style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                              img(
                                src = "Logoshiny.png",
                                style = "max-width: 200px; margin-bottom: 1em;"
                              )
                            )
                   )
                 )
        ),
        
        ####------------------------------ Litología---------------------------
        tabPanel("Litología",
                 div(
                   style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                   h4("Información general sobre el mapa de litologia"),
                   p("Este mapa ha sido elaborado a partir de los datos del proyecto (--), "),
                   p("CORINE (Coordination of Information on the Environment) clasifica el territorio en distintas categorías de uso del suelo, permitiendo el análisis espacial del paisaje y su evolución a lo largo del tiempo."),
                   p("Esta capa puede descargarse desde el Centro de Descargas del CNIG (Centro Nacional de Información Geográfica)."),
                   br(),
                   h5("Resumen de las litologias"),
                   p("Para facilitar la interpretación, bajo el mapa se muestran las litologías mayoritarios en el municipio seleccionado, junto con su respectivo porcentaje. Esto permite obtener una idea general de la distribución del territorio.")
                 ),
                 withSpinner(
                   plotOutput("Litologia"), type = 4, color = "#2c7a7b"
                 ),
                 br(),
                 uiOutput("textolitologia"),
                 div(
                   style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                   img(
                     src = "Logoshiny.png",
                     style = "max-width: 200px; margin-bottom: 1em;"
                   )
                 )
        )
      )
    )
  )
)



#--------------------------------------SERVER-----------------------------------------

server <- function(input, output, session) {
  
  #------------------------Actualizar Bienvenida-------------------
  
  output$showMapPanels <- reactive({
    # Muestra los paneles solo si se ha seleccionado un municipio
    !is.null(input$municipio) && input$municipio != ""
  })
  outputOptions(output, "showMapPanels", suspendWhenHidden = FALSE)
  
  #-----------------------Actualizar Municipios---------------------
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
  
  #--------------------------Variables Reactivas--------------------------------
  

  ##-----------------------------Generales-------------------------------------
  
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
  
  validar_municipio <- reactive({
    req(input$comunidad, input$municipio)
    
    validate(
      need(input$comunidad != "" && input$municipio != "", "Selecciona comunidad y municipio.")
    )
    
    cod_com <- CCAA_sf %>%
      filter(ccaa.shortname.es == input$comunidad) %>%
      pull(codauto)
    
    mun <- municipios %>%
      filter(name == input$municipio, codauto == cod_com)
    
    validate(
      need(nrow(mun) > 0, "El municipio no pertenece a la comunidad seleccionada.")
    )
    
    # No devuelve nada: solo controla
    invisible(NULL)
  })
  
  ##------------------------------Especificas-------------------------
  ###-----------------------------Capa Corine n 1--------------------------
  
  corine_n1_capa_completa <- reactive({
    
    nombre_corregido <- nombre_corregido()
    nombrecom_corregido <- nombrecom_corregido()
    
    url_geojson <- paste0("https://github.com/Carlos5682/DatosCorine/raw/refs/heads/main/Capasfinales/Corine/", 
                          nombrecom_corregido, "/", nombre_corregido, ".geojson")
    
    corine_muni <- tryCatch({
      st_read(url_geojson, quiet = TRUE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(corine_muni)) {
      return(NULL)
    }
    
    visible_area <- visible_area()
    
    corine_muni_recorte <- st_intersection(corine_muni, visible_area)
    
    
    corine_muni_recorte <- corine_muni_recorte %>%
      mutate(code_group = substr(CODE_18, 1, 1))
    
    corine_union <- corine_muni_recorte %>%
      group_by(code_group) %>%
      summarise(geometry = st_union(geometry))
    
    leyenda_n1 <- data.frame(
      CODE = c(
        "1", "2", "3", "4", "5"
      ),
      Uso_n1 = c(
        "Superficies artificiales",
        "Zonas agrícolas", 
        "Zonas forestales con vegetación natural y espacios abiertos", 
        "Zonas húmedas",
        "Superficies de agua"
      ),
      color_n1 = c(
        "#E6004D",
        "#FFFFA8",
        "#80FF00",
        "#A6A6FF",
        "#00CCF2"
      )
    )
    
    corine_n1 <- left_join(corine_union, leyenda_n1, by = c("code_group" = "CODE"))
    
    
  })
  
  
  ###------------------------Capa Corine n 1 recortada---------------------
  
  
  corine_n1_capa_recortada <- reactive({
    
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
    
    corine_muni <- corine_muni %>%
      mutate(code_group = substr(CODE_18, 1, 1))
    
    corine_union <- corine_muni %>%
      group_by(code_group) %>%
      summarise(geometry = st_union(geometry))
    
    leyenda_n1 <- data.frame(
      CODE = c(
        "1", "2", "3", "4", "5"
      ),
      Uso_n1 = c(
        "Superficies artificiales",
        "Zonas agrícolas", 
        "Zonas forestales con vegetación natural y espacios abiertos", 
        "Zonas húmedas",
        "Superficies de agua"
      ),
      color_n1 = c(
        "#E6004D",
        "#FFFFA8",
        "#80FF00",
        "#A6A6FF",
        "#00CCF2"
      )
    )
    
    corine_n1 <- left_join(corine_union, leyenda_n1, by = c("code_group" = "CODE"))
    
    
    
  })
  ###-----------------------------Capa Corine n 2--------------------------
  
  corine_n2_capa_completa <- reactive({
    
    nombre_corregido <- nombre_corregido()
    nombrecom_corregido <- nombrecom_corregido()
    
    url_geojson <- paste0("https://github.com/Carlos5682/DatosCorine/raw/refs/heads/main/Capasfinales/Corine/", 
                          nombrecom_corregido, "/", nombre_corregido, ".geojson")
    
    corine_muni <- tryCatch({
      st_read(url_geojson, quiet = TRUE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(corine_muni)) {
      return(NULL)
    }
    
    visible_area <- visible_area()
    
    corine_muni_recorte <- st_intersection(corine_muni, visible_area)

    
    corine_muni_recorte <- corine_muni_recorte %>%
      mutate(code_group = substr(CODE_18, 1, 2))
    
    corine_union <- corine_muni_recorte %>%
      group_by(code_group) %>%
      summarise(geometry = st_union(geometry))
    
    leyenda_n2 <- data.frame(
      CODE = c(
        "11", "12", "13", "14",
        "21", "22", "23", "24",
        "31", "32", "33",
        "41", "42", "51", "52"
      ),
      Uso_n2 = c(
        "Zonas urbanas", 
        "Zonas industriales, comerciales y de trasnporte",
        "Zonas de extracción minera, vertederos y de construcción",
        "Zonas verdes artifiacles, no agrícolas",
        "Tierras de labor",
        "Cultivos permanentes",
        "Prados y praderas",
        "Zonas agrícolas heterogéneas",
        "Bosques", 
        "Espacios de vegetación arbustiva y/o herbácea", 
        "Espacios abiertos con poca o sin vegetación",
        "Zonas húmedas continentales",
        "Zonas húmedas litorales", 
        "Aguas continentales", 
        "Aguas marinas"
      ),
      color_n2 = c(
        "#E6004D",
        "#CC4DF2",
        "#A600CC",
        "#FFA6E1",
        "#FFFFA8",
        "#E68000",
        "#E6E64D",
        "#FFE64D",
        "#80FF00",
        "#CCF24D",
        "#E6E6E6",
        "#A6A6FF",
        "#CCCCFF",
        "#00CCF2",
        "#00FFA6"
      )
    )
    
    corine_n2 <- left_join(corine_union, leyenda_n2, by = c("code_group" = "CODE"))
    
    
  })
  ###--------------------------Capa Corine n 2 recortada-------------------
  
  corine_n2_capa_recortada <- reactive({
    
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
    
    corine_muni <- corine_muni %>%
      mutate(code_group = substr(CODE_18, 1, 2))
    
    corine_union <- corine_muni %>%
      group_by(code_group) %>%
      summarise(geometry = st_union(geometry))
    
    leyenda_n2 <- data.frame(
      CODE = c(
        "11", "12", "13", "14",
        "21", "22", "23", "24",
        "31", "32", "33",
        "41", "42", "51", "52"
      ),
      Uso_n2 = c(
        "Zonas urbanas", 
        "Zonas industriales, comerciales y de trasnporte",
        "Zonas de extracción minera, vertederos y de construcción",
        "Zonas verdes artifiacles, no agrícolas",
        "Tierras de labor",
        "Cultivos permanentes",
        "Prados y praderas",
        "Zonas agrícolas heterogéneas",
        "Bosques", 
        "Espacios de vegetación arbustiva y/o herbácea", 
        "Espacios abiertos con poca o sin vegetación",
        "Zonas húmedas continentales",
        "Zonas húmedas litorales", 
        "Aguas continentales", 
        "Aguas marinas"
      ),
      color_n2 = c(
        "#E6004D",
        "#CC4DF2",
        "#A600CC",
        "#FFA6E1",
        "#FFFFA8",
        "#E68000",
        "#E6E64D",
        "#FFE64D",
        "#80FF00",
        "#CCF24D",
        "#E6E6E6",
        "#A6A6FF",
        "#CCCCFF",
        "#00CCF2",
        "#00FFA6"
      )
    )
    
    corine_n2 <- left_join(corine_union, leyenda_n2, by = c("code_group" = "CODE"))
    
    
    
  })
  
  
  ###-----------------------------Capa Corine n 3--------------------------

  corine_capa_completa <- reactive({
    
    nombre_corregido <- nombre_corregido()
    nombrecom_corregido <- nombrecom_corregido()
    
    url_geojson <- paste0("https://github.com/Carlos5682/DatosCorine/raw/refs/heads/main/Capasfinales/Corine/", 
                          nombrecom_corregido, "/", nombre_corregido, ".geojson")
    
    corine_muni <- tryCatch({
      st_read(url_geojson, quiet = TRUE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(corine_muni)) {
      return(NULL)
    }
    
    visible_area <- visible_area()
    
    corine_muni_recorte <- st_intersection(corine_muni, visible_area)
    
    return(corine_muni_recorte)
  })
  
  ###--------------------------Capa Corine n 3 recortada-------------------
  
  corine_capa_recortada <- reactive({
    
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
  })
  
  ###-------------------------Capa Ltologia gen --------------------------
  
  litologia_capa_completa <- reactive({
    
  nombre_corregido <- nombre_corregido()
  nombrecom_corregido <- nombrecom_corregido()
  
  url_geojson <- paste0("https://github.com/Carlos5682/DatosLitologia/raw/refs/heads/main/Capasfinales/Litologia/", 
                        nombrecom_corregido, "/", nombre_corregido, ".geojson")
  
  litologia_muni <- tryCatch({
    st_read(url_geojson, quiet = TRUE)
  }, error = function(e) {
    NULL
  })
  
  visible_area <- visible_area()
  litologia_muni <- st_intersection(litologia_muni, visible_area)
  
  })
  
  ###---------------------------Capa Litologia gen recortada------------------
  
  litologia_capa_recortada <- reactive({
    
  nombre_corregido <- nombre_corregido()
  nombrecom_corregido <- nombrecom_corregido()
  
  url_geojson <- paste0("https://github.com/Carlos5682/DatosLitologia/raw/refs/heads/main/Capasfinales/Litologia/", 
                        nombrecom_corregido, "/", nombre_corregido, ".geojson")
  
  litologia_muni <- tryCatch({
    st_read(url_geojson, quiet = TRUE)
  }, error = function(e) {
    NULL
  })
  
  municipio <- municipio_sf()
  litologia_muni <- st_intersection(litologia_muni, municipio)
  
  })
  
  #---------------------------------Outputs-----------------------------------
  
  ##------------------------------Mapa Corine nivel 1------------------------
  
  output$Corine1 <- renderPlot({
    
    validar_municipio()
    
    municipio_sf <- municipio_sf()
    
    lims <- lims()
    
    # Capa gris: diferencia entre área visible y municipio
    area_fuera_municipio <- area_fuera_municipio() 
    
    hypsobath_crop <- hypsobath_crop()
    
    add_bath <- nrow(hypsobath_crop()) > 0  
    
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
      geom_sf(data = corine_n1_capa_completa(), aes(fill = Uso_n1), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Usos del suelo nivel 1:",
                        values = setNames(corine_n1_capa_completa()$color_n1,
                                          corine_n1_capa_completa()$Uso_n1)) +
      ggtitle(paste("Mapa de usos del suelo del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
    
    print(p)
  }, bg = "transparent")
  
  ##------------------------------Texto Corine nivel 1----------------------
  
  output$textocorine1 <- renderUI({
    
    validar_municipio()
    
    corine_muni <- corine_n1_capa_recortada()
    
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> select(Uso_n1, color_n1, area)
    
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- (uso_summary$area / total_area) * 100
    
    top_usos <- uso_summary |> arrange(desc(porcentaje)) |> slice(1:3)
    
    # Texto con fondo de color
    textos_top <- paste0(
      seq_len(nrow(top_usos)), 
      '. <span style="background-color:', top_usos$color_n1, 
      '; color: black; padding: 4px 8px; border-radius: 6px;">',
      top_usos$Uso_n1, 
      '</span> (', 
      round(top_usos$porcentaje, 2), '%)'
    )
    
    HTML(paste0(
      "<b>En el municipio de ", input$municipio, ", los principales usos del suelo son:</b><br><br>",
      paste(textos_top, collapse = "<br>")
    ))
  })
  
  ##------------------------------Mapa Corine nivel 2------------------------
  
  output$Corine2 <- renderPlot({
    
    validar_municipio()
    
    municipio_sf <- municipio_sf()
    
    lims <- lims()
    
    # Capa gris: diferencia entre área visible y municipio
    area_fuera_municipio <- area_fuera_municipio() 
    
    hypsobath_crop <- hypsobath_crop()
    
    add_bath <- nrow(hypsobath_crop()) > 0  
    
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
      geom_sf(data = corine_n2_capa_completa(), aes(fill = Uso_n2), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Usos del suelo nivel 2:",
                        values = setNames(corine_n2_capa_completa()$color_n2,
                                          corine_n2_capa_completa()$Uso_n2)) +
      ggtitle(paste("Mapa de usos del suelo del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
    
    print(p)
  }, bg = "transparent")
  
  ##------------------------------------Texto Corine nivel 2------------------
  
  output$textocorine2 <- renderUI({
    
    validar_municipio()
    
    corine_muni <- corine_n2_capa_recortada()
    
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> select(Uso_n2, color_n2, area)
    
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- (uso_summary$area / total_area) * 100
    
    top_usos <- uso_summary |> arrange(desc(porcentaje)) |> slice(1:3)
    
    # Texto con fondo de color
    textos_top <- paste0(
      seq_len(nrow(top_usos)), 
      '. <span style="background-color:', top_usos$color_n2, 
      '; color: black; padding: 4px 8px; border-radius: 6px;">',
      top_usos$Uso_n2, 
      '</span> (', 
      round(top_usos$porcentaje, 2), '%)'
    )
    
    HTML(paste0(
      "<b>En el municipio de ", input$municipio, ", los principales usos del suelo son:</b><br><br>",
      paste(textos_top, collapse = "<br>")
    ))
  })
  
  ##-----------------------------Mapa Corine nivel 3-------------------------
  output$Corine <- renderPlot({
    
    validar_municipio()
    
    municipio_sf <- municipio_sf()
    
    lims <- lims()
    
    # Capa gris: diferencia entre área visible y municipio
    area_fuera_municipio <- area_fuera_municipio() 
    
    hypsobath_crop <- hypsobath_crop()
    
    add_bath <- nrow(hypsobath_crop()) > 0  
    
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
      geom_sf(data = corine_capa_completa(), aes(fill = Uso), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Usos del suelo nivel 3:",
                        values = setNames(corine_capa_completa()$color,
                                          corine_capa_completa()$Uso)) +
      ggtitle(paste("Mapa de usos del suelo del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
      
    print(p)
  }, bg = "transparent")
  
  ##-------------------------Texto Corine nivel 3---------------------------
  
  output$textocorine <- renderUI({
    
    validar_municipio()
    
    corine_muni <- corine_capa_recortada()
    
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
  
  ##-----------------------------Mapa Litologia General----------------------- 
  output$Litologia <- renderPlot({
    
    validar_municipio()
    
    # Capa gris: diferencia entre área visible y municipio
    area_fuera_municipio <- area_fuera_municipio() 
    
    municipio_sf <- municipio_sf()
    
    lims <- lims()
    
    
    hypsobath_crop <- hypsobath_crop()
    
    add_bath <- nrow(hypsobath_crop()) > 0     
    
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
      geom_sf(data = litologia_capa_completa(), aes(fill = LITOLOGIA), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Litología", 
                        values = setNames(litologia_capa_completa()$color, 
                                          litologia_capa_completa()$litologia)) +
      ggtitle(paste("Mapa de litologias del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
    
    print(p)
  }, bg = "transparent")
  
  ##-------------------------Texto de litologia general-----------------
  
  output$textolitologia <- renderUI({
    
    validar_municipio()
    
    
    litologia_muni <- litologia_capa_recortada()
    
    litologia_muni$area <- st_area(litologia_muni)
    
    uso_summary <- litologia_muni |> select(LITOLOGIA, color, area)
    
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- (uso_summary$area / total_area) * 100
    
    # Excluimos litología "Sin datos" antes de seleccionar las más comunes
    top_usos <- uso_summary |>
      filter(LITOLOGIA != "Sin datos") |>     # <- Aquí se descarta
      arrange(desc(porcentaje)) |>
      slice(1:3)
    
    
    # Texto con fondo de color
    # Texto con fondo de color, ajustando color de texto si el fondo es negro
    textos_top <- sapply(seq_len(nrow(top_usos)), function(i) {
      litologia <- top_usos$LITOLOGIA[i]
      color_fondo <- top_usos$color[i]
      
      # Verificamos si la litología corresponde a la que tiene fondo negro
      color_texto <- if (litologia == "Conglomerados, areniscas, pizarras y calizas. Carbón") {
        "white"
      } else {
        "black"
      }
      
      paste0(
        i, '. <span style="background-color:', color_fondo, 
        '; color:', color_texto, 
        '; padding: 4px 8px; border-radius: 6px;">',
        litologia, 
        '</span> (', 
        round(top_usos$porcentaje[i], 2), '%)'
      )
    })
    
    
    HTML(paste0(
      "<b>En el municipio de ", input$municipio, ", las principales litologías son:</b><br><br>",
      paste(textos_top, collapse = "<br>")
    ))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
