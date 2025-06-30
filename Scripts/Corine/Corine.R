rm(list=ls())
library(ggrepel)
library(ggspatial)
library(mapSpain)
library(sf)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(future.apply)
setwd("C://Users//samue//Desktop//proyectotfg")

#---------------------------------------------1 Recortar Comunidades autonomas--------------------------------------------

##---------------------------------------1.1 Procesar capas-----------------------------------

#Ver las capas que incluye el geopackage descargado del centro de de descargas del CNIG 

corinegpkg <- "Datos/Datosdescargados/CLC2018_GPKG/CLC2018_ES.gpkg"
st_layers(corinegpkg)

#Canarias y Peninsula tienen diferentes capas, hay que hacer todo el proceso 2 veces 
#Comenzamos con la primera capa para Peninsula y Baleares

###----------------------------1.1.1 Peninsula y Baleares------------------------------------------

####---------------------1.1.1.1 Leer capa--------------------------

corinepenybal <- st_read(corinegpkg, layer = "CLC18_ES") 

####--------------------1.1.1.2 Corregir geometrias----------------

corinepenybal <- st_cast(corinepenybal, "MULTIPOLYGON")
corinepenybal <- st_make_valid(corinepenybal)

####----------------1.1.1.3 Disolver geometrias----------------

corinepybdisueltos <- corinepenybal %>%
  group_by(CODE_18) %>%
  summarise(geom = st_union(geom), .groups = "drop") 

####---------------1.1.1.4 Reproyectar geometrías--------------

corinepenreproy <-  st_transform(corinepybdisueltos, 3035)

####------------------1.1.1.5 Personalizar leyenda------------------
leyenda_corine <- data.frame(
  code = c(
    "111", "112", "121", "122", "123", "124", "131", "132", "133", "141", "142",
    "211", "212", "213", "221", "222", "223", "231", "241", "242", "243", "244",
    "311", "312", "313", "321", "322", "323", "324", "331", "332", "333", "334",
    "335", "411", "412", "421", "422", "423", "511", "512", "521", "522", "523"
  ),
  Uso = c(
    "Tejido urbano continuo",
    "Tejido urbano discontinuo",
    "Zonas industriales o comerciales",
    "Redes viarias, ferroviarias y terrenos asociados",
    "Zonas portuarias",
    "Aeropuertos",
    "Zonas de extracción minera",
    "Escombreras y vertederos",
    "Zonas en construcción",
    "Zonas verdes urbanas",
    "Instalaciones deportivas y recreativas",
    "Tierras de labor en secano",
    "Terrenos regados permanentemente",
    "Arrozales",
    "Viñedos",
    "Frutales",
    "Olivares",
    "Praderas",
    "Cultivos anuales asociados con cultivos permanentes",
    "Mosaico de cultivos",
    "Terrenos principalmente agrícolas, pero con importantes espacios de vegetación natural",
    "Sistemas agroforestales",
    "Bosques de frondosas",
    "Bosques de coníferas",
    "Bosque mixto",
    "Pastizales naturales",
    "Landas y matorrales",
    "Vegetación esclerófila",
    "Matorral boscoso de transición",
    "Playas, dunas y arenales",
    "Roquedo",
    "Espacios con vegetación escasa",
    "Zonas quemadas",
    "Glaciares y nieves permanentes",
    "Humedales y zonas pantanosas",
    "Turberas",
    "Marismas",
    "Salinas",
    "Zonas llanas intermareales",
    "Cursos de agua",
    "Láminas de agua",
    "Lagunas costeras",
    "Estuarios",
    "Mares y océanos"
  ),
  color = c(
    "#E6004D", "#FF0000", "#CC4DF2", "#CC0000", "#E6CCCC", "#E6CCE6", "#A600CC", "#A64D00", "#FF4DFF", "#FFA6E1", "#FFE6FF", "#FFFFA8", "#FFFF00", "#E6E600", "#E68000", "#F2A64D", "#E6A600", "#E6E64D", "#FFE6A6", "#FFE64D", "#E6CC4D", "#F2CCA6", "#80FF00", "#00A600", "#4DFF00", "#CCF24D", "#A6FF80", "#A6E64D", "#A6F200", "#E6E6E6", "#CCCCCC", "#CCFFCC", "#000000", "#A6E6CC", "#A6A6FF", "#4D4DFF", "#CCCCFF", "#E6E6FF", "#A6A6E6", "#00CCF2", "#80F2E6", "#00FFA6", "#A6FFE6", "#E6F2FF"
  )
)
corinefinal <- left_join(corinepenreproy, leyenda_corine, by = c("CODE_18" = "code"))

####------------------1.1.1.6 Guardar capa final-------------------

st_write(corinefinal, 
         dsn = "Datos/Datoscorregidos/Corinecorregido/corinecorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)
###----------------------------1.1.2 Canarias------------------------------------------

####---------------------1.1.2.1 Leer capa--------------------------

corinecan <- st_read(corinegpkg, layer = "CLC18_ES_Canarias") 

####--------------------1.1.2.2 Corregir geometrias----------------

corinecan <- st_cast(corinecan, "MULTIPOLYGON")
corinecan <- st_make_valid(corinecan)

####----------------1.1.2.3 Disolver geometrias----------------

corinecandisuelto <- corinecan %>%
  group_by(CODE_18) %>%
  summarise(geom = st_union(geom), .groups = "drop") 

####---------------1.1.2.4 Reproyectar geometrías--------------

corinecanreproy <-  st_transform(corinecandisuelto, 3035)

####------------------1.1.2.5 Personalizar leyenda------------------
leyenda_corine <- data.frame(
  code = c(
    "111", "112", "121", "122", "123", "124", "131", "132", "133", "141", "142",
    "211", "212", "213", "221", "222", "223", "231", "241", "242", "243", "244",
    "311", "312", "313", "321", "322", "323", "324", "331", "332", "333", "334",
    "335", "411", "412", "421", "422", "423", "511", "512", "521", "522", "523"
  ),
  Uso = c(
    "Tejido urbano continuo",
    "Tejido urbano discontinuo",
    "Zonas industriales o comerciales",
    "Redes viarias, ferroviarias y terrenos asociados",
    "Zonas portuarias",
    "Aeropuertos",
    "Zonas de extracción minera",
    "Escombreras y vertederos",
    "Zonas en construcción",
    "Zonas verdes urbanas",
    "Instalaciones deportivas y recreativas",
    "Tierras de labor en secano",
    "Terrenos regados permanentemente",
    "Arrozales",
    "Viñedos",
    "Frutales",
    "Olivares",
    "Praderas",
    "Cultivos anuales asociados con cultivos permanentes",
    "Mosaico de cultivos",
    "Terrenos principalmente agrícolas, pero con importantes espacios de vegetación natural",
    "Sistemas agroforestales",
    "Bosques de frondosas",
    "Bosques de coníferas",
    "Bosque mixto",
    "Pastizales naturales",
    "Landas y matorrales",
    "Vegetación esclerófila",
    "Matorral boscoso de transición",
    "Playas, dunas y arenales",
    "Roquedo",
    "Espacios con vegetación escasa",
    "Zonas quemadas",
    "Glaciares y nieves permanentes",
    "Humedales y zonas pantanosas",
    "Turberas",
    "Marismas",
    "Salinas",
    "Zonas llanas intermareales",
    "Cursos de agua",
    "Láminas de agua",
    "Lagunas costeras",
    "Estuarios",
    "Mares y océanos"
  ),
  color = c(
    "#E6004D", "#FF0000", "#CC4DF2", "#CC0000", "#E6CCCC", "#E6CCE6", "#A600CC", "#A64D00", "#FF4DFF", "#FFA6E1", "#FFE6FF", "#FFFFA8", "#FFFF00", "#E6E600", "#E68000", "#F2A64D", "#E6A600", "#E6E64D", "#FFE6A6", "#FFE64D", "#E6CC4D", "#F2CCA6", "#80FF00", "#00A600", "#4DFF00", "#CCF24D", "#A6FF80", "#A6E64D", "#A6F200", "#E6E6E6", "#CCCCCC", "#CCFFCC", "#000000", "#A6E6CC", "#A6A6FF", "#4D4DFF", "#CCCCFF", "#E6E6FF", "#A6A6E6", "#00CCF2", "#80F2E6", "#00FFA6", "#A6FFE6", "#E6F2FF"
  )
)
corinecanfinal <- left_join(corinecanreproy, leyenda_corine, by = c("CODE_18" = "code"))

####------------------1.1.2.6 Guardar capa final-------------------

st_write(corinecanfinal, 
         dsn = "Datos/Datoscorregidos/Corinecorregido/corinecancorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)


##---------------------------------------1.2 Bucle para las comunidades autonomas------------------------------------
###-------------------------1.2.1 Peninsula y Baleares---------------------------------
corinefinal <- st_read("Datos/Datoscorregidos/Corinecorregido/corinecorregido.gpkg")

####-------------1.2.1.1 Obtener comunidades autónomas---------
ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

for (i in 1:nrow(ccaa_sf)) {
  nombre_ccaa <- ccaa_sf$ccaa.shortname.es[i]
  codigo_ccaa <- ccaa_sf$codauto[i]
  
  cat("Procesando:", nombre_ccaa, "\n")
  
  ccaa_geom <- ccaa_sf[i, ]
  
  ####----------1.2.1.2 Calcular y expandir bbox---------------
  bbox <- st_bbox(ccaa_geom)
  
  x_diff <- bbox$xmax - bbox$xmin
  y_diff <- bbox$ymax - bbox$ymin
  max_diff <- max(x_diff, y_diff)
  
  x_buffer <- max_diff * 0.15
  y_buffer <- max_diff * 0.15
  
  bbox_expandida <- structure(
    c(
      xmin = bbox$xmin - x_buffer,
      ymin = bbox$ymin - y_buffer,
      xmax = bbox$xmax + x_buffer,
      ymax = bbox$ymax + y_buffer
    ),
    class = "bbox",
    crs = st_crs(ccaa_geom)
  )
  
  ####----------1.2.1.3 Convertir bbox a polígono-------------
  bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(ccaa_geom)))
  
  ####----------1.2.1.4 Intersección con polígono--------------
  corine_crop <- st_intersection(corinefinal, bbox_poly)
  
  ####------------1.2.1.5 Guardar como GeoPackage---------------
  st_write(corine_crop, paste0("Datos/DatosporComunidad/Corine/", nombre_ccaa, ".gpkg"), delete_dsn = TRUE)
}

###-----------------------------1.2.2 Canarias---------------------------------

corinecanfinal <- st_read("Datos/Datoscorregidos/Corinecorregido/corinecancorregido.gpkg")

####-------------1.2.2.1 Obtener comunidades autónomas---------

ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

####---------------1.2.2.2 Filtrar Canarias------------------------

canarias_geom <- ccaa_sf[ccaa_sf$ccaa.shortname.es == "Canarias", ]
  
####----------1.2.2.3 Calcular y expandir bbox---------------

bbox <- st_bbox(canarias_geom)
  
x_diff <- bbox$xmax - bbox$xmin
y_diff <- bbox$ymax - bbox$ymin
max_diff <- max(x_diff, y_diff)

x_buffer <- max_diff * 0.15
y_buffer <- max_diff * 0.15

bbox_expandida <- structure(
  c(
    xmin = bbox$xmin - x_buffer,
    ymin = bbox$ymin - y_buffer,
    xmax = bbox$xmax + x_buffer,
    ymax = bbox$ymax + y_buffer
  ),
  class = "bbox",
  crs = st_crs(canarias_geom)
  )
  
  ####----------1.2.2.4 Convertir bbox a polígono-------------
bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(canarias_geom)))
  
  ####----------1.2.2.5 Intersección con polígono--------------
corinecan_crop <- st_intersection(corinecanfinal, bbox_poly)
  
  ####------------1.2.2.6 Guardar como GeoPackage---------------
st_write(corinecan_crop, "Datos/DatosporComunidad/Corine/Canarias.gpkg", delete_dsn = TRUE)

#---------------------------------------2 Recortar municipios-----------------------------------
##-----------------------------2.1 Configurar paralelización--------------------
###-------------------2.1.1 Maximo de memoria----------------
options(future.globals.maxSize = 2 * 1024^3)  

###-------------------2.1.2 Numero de nucleos-------------------
parallel::detectCores()
future::availableCores()

plan(multisession, workers = 10)

##-----------------------------2.2 Obtener municipios-----------------------
municipios <- esp_get_munic(moveCAN = FALSE, epsg = 3035)
CCAA_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)

##----------------------------2.3 Comunidades a paralelizar------------------
comunidades <- unique(CCAA_sf$ccaa.shortname.es)

##-----------------------------2.4 Función a paralelizar--------------------------

procesar_comunidad <- function(comunidad_objetivo) {
  cat("▶ Procesando comunidad:", comunidad_objetivo, "\n")
  
  ###------------------------2.4.1 Codigo de comunidad------------------------
  cod_comunidad <- CCAA_sf %>%
    filter(ccaa.shortname.es == comunidad_objetivo) %>%
    pull(codauto)
  
  ###------------------------2.4.2 Muncipios de comunidad---------------------
  municipios_comunidad <- municipios %>%
    filter(codauto == cod_comunidad) %>%
    st_make_valid()
  
  ###-----------------------2.4.3 Ruta de Corine por comunidad---------------
  
  corine_path <- paste0("Datos/DatosporComunidad/Corine/", comunidad_objetivo, ".gpkg")
  dir_out <- paste0("Capasfinales/Corine/", comunidad_objetivo)
  
  if (!file.exists(corine_path)) {
    return(paste0("⚠ Archivo CORINE no encontrado para ", comunidad_objetivo))
  }
  
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  ###-----------------------2.4.4 Leer Corine---------------------------------
  corine_ccaa <- st_read(corine_path, quiet = TRUE)
  
  resultados <- vector("list", nrow(municipios_comunidad))
  ###-------------------------2.4.5 Bucle--------------------------------------
  for (i in seq_len(nrow(municipios_comunidad))) {
    muni <- municipios_comunidad[i, ]
    nombre_muni <- gsub(" ", "_", gsub("/", "o", muni$name))
    output_path <- file.path(dir_out, paste0(nombre_muni, ".geojson"))
  
  ####--------------------2.4.5.1 Mantener municipios ya obtenidos------------    
    if (file.exists(output_path)) {
      resultados[[i]] <- paste0("✔ Ya existe: ", nombre_muni)
      next
    }
    
    tryCatch({
      
      ###------------------2.4.5.2 Calcular y expandir bbox-----------------
      bbox <- st_bbox(muni)
      x_diff <- bbox$xmax - bbox$xmin
      y_diff <- bbox$ymax - bbox$ymin
      max_diff <- max(x_diff, y_diff)
      
      x_buffer <- max_diff * 0.15
      y_buffer <- max_diff * 0.15
      
      bbox_expandido <- structure(
        c(
          xmin = bbox$xmin - x_buffer,
          ymin = bbox$ymin - y_buffer,
          xmax = bbox$xmax + x_buffer,
          ymax = bbox$ymax + y_buffer
        ),
        class = "bbox",
        crs = st_crs(muni)
      )
      
      ###------------------2.4.5.3 Convertir bbox a polígono--------------
      bbox_poly <- st_as_sfc(st_bbox(bbox_expandido, crs = st_crs(muni)))
      
      ###------------------2.4.5.4 Intersección con poligono--------------
      recorte <- st_intersection(corine_ccaa, bbox_poly)
      
      ###------------------2.4.5.5 Detector de error---------------------

      if (nrow(recorte) > 0) {
        st_write(recorte, output_path, delete_layer = TRUE, quiet = TRUE)
        resultados[[i]] <- paste0("💾 Guardado: ", nombre_muni)
      } else {
        st_write(muni[0, ], output_path, delete_layer = TRUE, quiet = TRUE)  # Archivo vacío
        resultados[[i]] <- paste0("⚠ Vacío (guardado): ", nombre_muni)
      }
    }, error = function(e) {
      resultados[[i]] <- paste0("❌ Error en ", nombre_muni, ": ", e$message)
    })
  }
  
  return(resultados)
}

##----------------------------2.5 Ejecutar en paralelo-------------------
resultados_totales <- future_lapply(comunidades, procesar_comunidad)

##----------------------------2.6 Resumen final--------------------------
cat("\n✅ Procesamiento completado.\n")

