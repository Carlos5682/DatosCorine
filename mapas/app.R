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
library(zip)
library(stringr)

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

##---------------------------------Mensaje Corine-------------------------------

info_corine_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  
  h4("Información general sobre el Mapa de Litologías"),
  p("El mapa muestra la distribución de los distintos usos del suelo en el territorio nacional: áreas urbanas, tierras agrícolas, zonas forestales, espacios naturales, entre otros."),
  p("Está basado en los datos del proyecto europeo CORINE Land Cover 2018, desarrollado para recopilar y armonizar información sobre el uso del suelo en los países europeos."),
  p("CORINE (acrónimo de 'Coordinación de la Información sobre el Medio Ambiente') clasifica el territorio según criterios comunes para toda Europa. Esta homogeneidad permite analizar cambios a lo largo del tiempo, comparar regiones y disponer de una base común para la toma de decisiones en materia de medio ambiente y ordenación territorial."),
  
  br(),
  
  h4("Presentación y niveles de clasificación"),
  p("El sistema de codificación de CORINE se organiza en tres niveles jerárquicos, con distinto grado de detalle:"),
  tags$ul(
    tags$li(strong("Nivel 1:"), " agrupa los grandes tipos de uso del suelo, como superficies artificiales, zonas agrícolas, etc."),
    tags$li(strong("Nivel 2:"), " subdivide estas categorías en tipos más específicos, como zonas urbanas, zonas industriales, zonas de extracción minera, etc."),
    tags$li(strong("Nivel 3:"), " ofrece el máximo nivel de detalle, distinguiendo unidades concretas como zonas en construcción, viñedos, olivares, etc.")
  ),
  p("Esta clasificación reproduce fielmente la estructura original de los datos CORINE, sin modificaciones ni interpretaciones propias. Su jerarquía facilita distintos niveles de análisis, desde una visión general del territorio hasta estudios más detallados."),
  
  br(),
  
  h4("Tipo de información que puede extraerse"),
  p("El mapa permite identificar patrones de ocupación del suelo y realizar análisis espaciales de carácter ambiental o territorial. Algunos ejemplos relevantes:"),
  tags$ul(
    tags$li("En municipios con predominio de áreas naturales (bosques, matorrales, zonas húmedas), pueden detectarse zonas especialmente sensibles a transformaciones de origen humano."),
    tags$li("La presencia mayoritaria de cultivos puede estar asociada a dinámicas agrícolas intensivas o al riesgo de abandono rural."),
    tags$li("La expansión del suelo urbano ofrece indicadores de presión sobre el entorno natural o agrícola."),
    tags$li("Comparar regiones según el tipo de uso del suelo dominante facilita el análisis de desequilibrios territoriales."),
    tags$li("Los cambios entre diferentes ediciones del mapa permiten observar procesos como la urbanización, la reforestación o la pérdida de espacios agrarios.")
  ),
  
  br(),
  
  h4("Acceso a los datos originales"),
  p("Los datos del proyecto CORINE Land Cover están disponibles para su consulta y descarga en el Centro Nacional de Información Geográfica (CNIG): ",
    a("https://centrodedescargas.cnig.es/CentroDescargas/corine-land-cover",
      href = "https://centrodedescargas.cnig.es/CentroDescargas/corine-land-cover",
      target = "_blank")
  ),
  
  br(),
  
  h4("Mapa")
)


info2_corine_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  
  h4("Interpretación del mapa"),
  p("Para facilitar la interpretación, se muestran un gráfico con la distribución de usos del suelo en el municipio seleccionado, además de los 3 principales usos junto con su respectivo porcentaje. Esta información permite obtener una visión general rápida de cómo se distribuye el territorio."))

nota_corine_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  p(em("Nota:")),
  tags$ul(
    tags$li("La leyenda del mapa corresponde a los usos visibles en el municipio seleccionado y sus alrededores."),
    tags$li("Los porcentajes mostrados debajo del mapa hacen referencia exclusivamente al área del municipio."),
    tags$li("El sistema de coordenadas utilizado es ETRS89 / LAEA Europe (EPSG:3035)."),
    tags$li(
      "El procesamiento y visualización del mapa se ha realizado en ",
      strong("RStudio"),
      " utilizando funciones de los paquetes ",
      code("sf"), ", ",
      code("dplyr"), ", ",
      code("ggplot2"), " y ",
      code("mapSpain"), "."
    )
  )
)



##-----------------------------------Mensaje Litologias-------------------------

info_litologia_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  
  h4("Información general sobre el mapa de litologías"),
  p("Este mapa ha sido elaborado a partir del Mapa Litológico de la Península Ibérica, Baleares y Canarias a escala 1:1.000.000."),
  p("Fue desarrollado por el Instituto Geológico y Minero de España (IGME), a partir del Mapa Geológico de la Península Ibérica, Baleares y Canarias editado en 1995."),
  
  br(),
  
  h4("Presentación y niveles de clasificación"),
  p("El mapa ofrece dos niveles jerárquicos de clasificación:"),
  tags$ul(
    tags$li(strong("Nivel 1:"), " corresponde a categorías generales de litologías, basadas directamente en la columna ", code("LITOLOGIA"), " de los datos originales."),
    tags$li(strong("Nivel 2:"), " proporciona una clasificación más detallada, utilizando la columna ", code("DLO"), ".")
  ),
  p("Ambos niveles provienen directamente de la estructura de los datos proporcionados por el IGME, sin modificaciones o reinterpretaciones propias. Esta jerarquía facilita distintos enfoques de análisis, desde una visión general hasta un estudio más detallado."),
  
  br(),
  
  h4("Acceso a los datos"),
  p("Los datos utilizados para generar este mapa pueden descargarse desde el IGME (Instituto Geológico y Minero de España). Están disponibles en: ",
    a("https://info.igme.es/cartografiadigital/geologica/Geologicos1MMapa.aspx?Id=Litologico1000&language=es",
      href = "https://info.igme.es/cartografiadigital/geologica/Geologicos1MMapa.aspx?Id=Litologico1000&language=es",
      target = "_blank")
  ),
  
  br(),
  
  h4("Mapa")
)

info2_litologia_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  h4("Interpretación del mapa"),
  p("Para facilitar la interpretación, se muestra un gráfico con la distribución de las litologías en el municipio seleccionado, además de las principales litologías junto con su respectivo porcentaje. Esta información permite obtener una visión general rápida de cómo se distribuye el territorio."))
  

nota_litologia_ui <- div(
  style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
  p(em("Nota:")),
  tags$ul(
    tags$li("La leyenda del mapa corresponde a las litologias visibles en el municipio seleccionado y sus alrededores."),
    tags$li("Los porcentajes mostrados debajo del mapa hacen referencia exclusivamente al área del municipio."),
    tags$li("El sistema de coordenadas utilizado es ETRS89 / LAEA Europe (EPSG:3035)."),
    tags$li(
      "El procesamiento y visualización del mapa se ha realizado en ",
      strong("RStudio"),
      " utilizando funciones de los paquetes ",
      code("sf"), ", ",
      code("dplyr"), ", ",
      code("ggplot2"), " y ",
      code("mapSpain"), "."
    )
  )
)

#-------------------------------------Diccionarios------------------------------------

dicc_corine_2 <- data.frame(
  Uso = c("Zonas urbanas", 
          "Zonas industriales, comerciales y de transporte",
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
          "Aguas marinas"),
  Explicacion = c(
    "Zonas ocupadas principalmente por viviendas y edificios utilizados para servicios administrativos o públicos, incluyendo sus áreas conexas (terrenos asociados, red vial de acceso, estacionamientos).",
    "Zonas ocupadas principalmente por actividades industriales, comercio, servicios financieros y de transporte, incluyendo carreteras, vías férreas, aeropuertos, puertos fluviales y marítimos, así como sus terrenos asociados e infraestructuras de acceso. Incluye instalaciones de cría industrial de ganado.",
    "Zonas artificiales ocupadas principalmente por actividades extractivas, sitios de construcción y vertederos de residuos creados por el ser humano, junto con sus terrenos asociados.",
    "Zonas creadas voluntariamente para uso recreativo. Incluye parques urbanos, instalaciones deportivas y de ocio.",
    "Tierras en rotación utilizadas para cultivos anuales y barbechos, ya sea de secano o regadío. Incluye cultivos inundados como arrozales.",
    "Superficies ocupadas por cultivos perennes, sin rotación. Incluye frutales extensivos, olivares, castañares, nogales, viñedos y otros cultivos leñosos permanentes.",
    "Tierras utilizadas de forma permanente (al menos 5 años) para producción de forraje. Incluye praderas naturales o sembradas, con uso agrícola moderado.",
    "Zonas donde coexisten cultivos anuales y permanentes en la misma parcela o adyacentes, así como mezclas de cultivos, pastos y vegetación natural en mosaico.",
    "Zonas cubiertas por árboles (coníferas o de hoja ancha, nativos o exóticos), con un patrón forestal y un dosel mínimo del 30 %, y árboles de más de 5 metros de altura bajo condiciones normales. Incluye plantaciones jóvenes con al menos 500 árboles por hectárea.",
    "Zonas naturales o seminaturales cubiertas principalmente por arbustos o hierbas, incluyendo:

Matorrales templados, mediterráneos y submediterráneos (maquia, garriga, matorral).

Etapas de transición de bosque (recolonización natural o degradación).

Praderas secas, húmedas, alpinas o subalpinas.

Pastizales naturales en suelos pobres, laderas o zonas montañosas.
Estas zonas pueden contener árboles dispersos (menos del 15 % de cobertura de copa) y vegetación adaptada a condiciones difíciles, muchas veces en zonas abandonadas o afectadas por eventos naturales.",
    "Áreas naturales con escasa o nula cobertura vegetal. Incluye terrenos arenosos o rocosos afectados por erosión, pastizales esteparios, dunas, acantilados, pedregales, zonas con nieve o hielo permanentes, y áreas quemadas con vegetación leñosa.",
    "Zonas inundadas o susceptibles de inundación durante gran parte del año por agua dulce, salobre o estancada, con vegetación específica compuesta por arbustos bajos, especies semileñosas o herbáceas. Incluye:

Vegetación ribereña de lagos, ríos y arroyos.

Turberas ricas (eutróficas), manantiales, pantanos y ciénagas en transición.

Turberas altas y extensas con comunidades muy pobres en nutrientes y altamente ácidas, dominadas por esfagnos (musgos) que crecen sobre turba.",
    "Zonas que se inundan por mareas altas en algún momento del ciclo anual. Incluye:

Praderas salinas y marismas con distintos grados de salinidad y humedad.

Áreas fangosas o arenosas que quedan sumergidas parcialmente en cada marea, normalmente sin plantas vasculares.

Balsas de evaporación para extracción de sal, activas o recientemente abandonadas.",
    "Lagos, lagunas y charcas de origen natural con agua dulce (no salina), así como aguas corrientes como ríos y arroyos. Incluye cuerpos de agua dulce artificiales como embalses y canales.",
    "Aguas oceánicas y de plataforma continental, bahías y canales estrechos, incluyendo fiordos, estuarios y ensenadas marinas. También incluye aguas costeras salinas o salobres formadas por entradas de mar parcialmente aisladas por bancos de arena o barro."
  )
)

dicc_corine_3 <- data.frame(
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
  Explicacion = c(
    "Zonas donde las estructuras urbanas y redes de transporte dominan claramente la superficie. Más del 80% del suelo está cubierto por elementos impermeables como edificios, carreteras y superficies artificiales. Las áreas con vegetación o suelo desnudo son excepcionales.

",
    "Zonas urbanas con presencia significativa de áreas vegetadas y superficies desnudas distribuidas de forma discontinua. Las estructuras impermeables como edificios, vías y superficies pavimentadas cubren entre 30 y 80% del terreno.",
    "Zonas dominadas por edificios, otras construcciones y superficies artificiales (hormigón, asfalto, tierra apisonada, etc.). Puede haber presencia de vegetación (usualmente césped) u otras superficies no selladas.
Incluye también:

Áreas con instalaciones cubiertas, establos, talleres, almacenes y zonas de carga relacionadas con la agroindustria.

Áreas deterioradas con vegetación ruderal.

Edificaciones agrícolas ligadas a procesos de colectivización.
Esta clase se asigna a unidades de terreno utilizadas para actividades industriales, comerciales o servicios públicos.",
    "Autopistas y líneas ferroviarias, junto con sus instalaciones (estaciones, andenes, terraplenes) y franjas de vegetación lineal de menos de 100m de ancho.
Ancho mínimo para su inclusión: 100m.",
    "Infraestructuras de puertos, tanto en tierra como en agua, incluyendo muelles, astilleros y marinas.",
    "Instalaciones aeroportuarias: pistas, edificios y terrenos asociados. Esta clase abarca cualquier tipo de infraestructura terrestre destinada al transporte aéreo.",
    "Zonas de extracción a cielo abierto de materiales de construcción (como canteras de arena, graveras) u otros minerales (minas a cielo abierto). Incluye también excavaciones mineras que han quedado inundadas.",
    "Zonas utilizadas como vertederos de residuos públicos, industriales o procedentes de actividades mineras.",
    "Áreas en proceso de desarrollo o transformación artificial, incluyendo excavaciones de suelo o roca madre y movimientos de tierra. Esta clase se asigna a zonas modificadas por actividades humanas, que están en transición hacia superficies artificiales.

Caso particular:
También incluye zonas agrícolas que se están reconvirtiendo en áreas naturales. Durante esta transición, el terreno puede permanecer como suelo desnudo por un tiempo, al pasar de un uso agrícola a una función de conservación o restauración ecológica.",
    "Áreas con vegetación ubicadas dentro o parcialmente rodeadas por tejido urbano. Esta clase se asigna a zonas verdes urbanas con carácter recreativo u ornamental, generalmente accesibles al público.",
    "Zonas destinadas al deporte, recreación y actividades al aire libre. Incluye:

Campings, campos deportivos, parques recreativos, campos de golf, hipódromos, entre otros.

Parques formales ubicados fuera del tejido urbano.

Caso particular:
Estaciones de esquí con nieve artificial:
Muchas estaciones cuentan con infraestructura para fabricar nieve artificial, incluyendo estanques de agua y alteración del relieve (nivelación, remoción de rocas, etc.). Estas zonas pueden incluir productos químicos en la nieve para retrasar su derretimiento. Se consideran superficies artificiales por el fuerte impacto humano en el medio ambiente.",
    "Parcelas agrícolas cultivadas con cultivos anuales no permanentes, bajo un sistema de rotación y dependientes del agua de lluvia.
Incluye tierras en barbecho dentro del mismo sistema y parcelas con riego esporádico mediante dispositivos temporales (aspersores portátiles).",
    "Parcelas agrícolas con cultivos de regadío dependientes de una infraestructura permanente (canales, sistemas de drenaje, etc.).
No incluye tierras con riego esporádico o dispositivos temporales.",
    "Parcelas agrícolas preparadas para el cultivo de arroz, compuestas por superficies planas inundadas periódicamente y con canales de riego.",
    "Zonas plantadas con vides. Los viñedos cubren más del 50% de la parcela y determinan el uso principal del suelo.",
    "Parcelas agrícolas plantadas con árboles frutales o arbustos, incluidos los de frutos secos. Pueden estar organizadas con una sola especie o en mezcla, a menudo sobre superficies con césped permanente.

Caso particular:
Plantaciones de lúpulo: Cultivos con estructuras altas de soporte, utilizados para la producción de lúpulo.",
    "Parcelas agrícolas plantadas con olivos para producción de aceitunas.",
    "Áreas de pradera permanente utilizadas con fines agrícolas, afectadas por la actividad humana.
Flora dominada por gramíneas, típicamente usada para pastoreo o corte de heno.

Casos particulares:

Praderas sobre tierras abandonadas: Parcelas agrícolas sin uso durante más de tres años, convertidas en pradera. Su identificación requiere imágenes satelitales multianuales.

Parcelas sin uso entre construcciones o alrededor de zonas urbanizadas.",
    "Parcelas con cultivos anuales no permanentes (principalmente tierras de cultivo) asociados con cultivos permanentes (frutales, olivos o viñas) en la misma parcela.",
    "Mosaico de pequeñas parcelas agrícolas con diferentes tipos de cultivo: cultivos anuales, pastos y/o cultivos permanentes, a veces con casas dispersas o jardines.",
    "Zonas agrícolas con intercalado significativo de áreas naturales o seminaturales (bosques, matorrales, humedales, cuerpos de agua, afloramientos minerales), en patrón de mosaico.",
    "Parcelas con cultivos anuales o pastoreo bajo cobertura de especies forestales (uso mixto agrícola-forestal).",
    "Formaciones vegetales compuestas principalmente por árboles (incluyendo sotobosque de arbustos y matorrales), donde predominan las especies de hoja ancha (frondosas).",
    "Formaciones vegetales dominadas por árboles (con presencia de arbustos y matorrales en el sotobosque), donde predominan las especies de coníferas.",
    "Formaciones vegetales de árboles (con sotobosque de arbustos y matorrales), donde ningún tipo (frondosas o coníferas) predomina claramente.",
    "Praderas con poca o moderada influencia humana, de baja productividad, situadas frecuentemente en terrenos irregulares, con pendientes o zonas rocosas, y a menudo mezcladas con vegetación (semi)natural.

Casos particulares:

Praderas alpinas: Se ubican por encima del límite forestal en zonas montañosas, formadas por comunidades herbáceas en su estado natural de equilibrio, donde las condiciones no permiten el desarrollo de vegetación leñosa.

Praderas aluviales y llanuras costeras: Formaciones herbáceas con alta humedad del suelo e inundaciones estacionales, bajo baja influencia humana.",
    "Vegetación con cobertura baja y densa, dominada por arbustos bajos o enanos (como brezos, zarzas, aulagas, retamas, etc.) y plantas herbáceas, representando una etapa de equilibrio climático (clímax).

Caso particular:

Matorral de pino negro enano: Formaciones de pino enano en estado clímax, con copas compactas de 2–2,5m de altura, situadas a gran altitud, donde no se desarrollan árboles más altos.",
    "Vegetación arbustiva densa en etapa de clímax, típica de climas mediterráneos, como el maquis, matorral mediterráneo (matorral) y garriga. Compuesta por especies resistentes a la sequía y al fuego.",
    "Vegetación de transición con matorrales y herbáceas, y árboles dispersos. Representa etapas de:

Degradación del bosque,

Regeneración forestal o

Sucesión natural.

Caso particular:

Turberas arboladas y zonas de transición: Vegetación mixta de arbustos y hierbas, con árboles dispersos en los bordes de turberas.",
    "Extensiones naturales no vegetadas de arena, grava o guijarros, tanto en zonas costeras como continentales. Incluye lechos de ríos torrenciales. La cobertura vegetal no supera el 10%.",
    "Zonas con roquedos, acantilados, pedreras y formaciones rocosas, incluyendo áreas con erosión activa y planicies salinas interiores por encima de la línea de marea alta.",
    "Zonas con vegetación dispersa que cubre entre el 10 y el 50% de la superficie. Incluye:

Estepas, tundra, zonas con líquenes, tierras malas, áreas kársticas, y vegetación dispersa en alta montaña.",
    "Zonas de vegetación leñosa natural afectadas por incendios recientes.",
    "Zonas cubiertas por glaciares o campos de nieve permanente durante todo el año.

",
    "Zonas bajas generalmente inundadas en invierno, con suelos saturados de agua dulce durante todo el año.

Caso particular:

Turberas bajas sin árboles y turberas de transición, a veces con una capa de turba superior a 30cm.
Se encuentran en depresiones interiores, valles fluviales, zonas de manantiales o márgenes de turberas altas. Su superficie puede ser plana o cóncava, con pequeñas formaciones como montículos o matas.",
    "Humedales con acumulación significativa de materia vegetal descompuesta, especialmente musgo del género Sphagnum.
Incluye turberas naturales y explotadas.

Caso particular:

Turberas explotadas: zonas donde se extrae la turba para uso comercial o agrícola.",
    "Zonas vegetadas y bajas del litoral, situadas por encima del nivel de marea alta, susceptibles a inundaciones por agua marina.
Estas áreas están frecuentemente en proceso de relleno por sedimentos marinos (lodo, arena), lo que permite la colonización por plantas halófilas.",
    "Piscinas de evaporación para la extracción de sal del agua salada, ya sean activas o en proceso de abandono.
Incluye zonas de marismas salinas explotadas para producción de sal, claramente distinguibles por su parcelación y estructuras de diques o embalses.",
    "Zona costera bajo influencia de las mareas, ubicada entre el mar abierto y tierra firme.
Se inunda dos veces al día en un ciclo de unas 12 horas, abarcando el área entre el nivel promedio de marea baja y marea alta.
Generalmente está desprovista de vegetación y compuesta por lodo, arena o roca.",
    "Canales naturales o artificiales que funcionan como vías de drenaje, incluyendo canales.
Ancho mínimo para su inclusión: 100m.",
    "Zonas naturales o artificiales con agua estancada visible durante la mayor parte del año.
    Incluye lagos, lagunas y embalses.",
    "Extensiones de agua salobre o salada ubicadas en zonas costeras, separadas del mar por una lengua de tierra u otra formación topográfica similar.
Pueden estar conectadas al mar de forma permanente o temporal.

",
    "Desembocaduras de ríos bajo influencia de las mareas, donde el flujo de agua sube y baja según el ciclo de marea.",
    "Zona mar adentro a partir del límite inferior de la marea baja. Incluye aguas abiertas del mar y océano."
    
  )
)
#---------------------------------------------UI---------------------------------------
ui <- page_sidebar(
  
  title = "CartoAmbiente",
  
  sidebar = sidebar(
    selectInput(
      "comunidad",
      "Selecciona una comunidad autónoma:",
      choices = c("-" = "", sort(unique(CCAA_sf$ccaa.shortname.es))),
      selected = NULL
    ),
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
    input_dark_mode(id = "mode", mode = "light")
  ),
  
  div(
    id = "main-panel",
    
    conditionalPanel(
      condition = "!output.showMapPanels",
      div(
        style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
        img(
          src = "Logoshiny.png",
          style = "max-width: 200px; margin-bottom: 1em;"
        ),
        h2("Bienvenido a CartoAmbiente"),
        p("CartoAmbiente es un proyecto diseñado para generar mapas ambientales a nivel municipal en toda España. Su objetivo es facilitar el acceso a la cartografía ambiental de forma intuitiva y accesible para todo tipo de usuarios."),
        p("Este proyecto ha sido desarrollado en el marco de un Trabajo de Fin de Grado del Grado en Ciencias Ambientales, en la Universidad de Alcalá de Henares."),
        p("Para cualquier consulta o sugerencia, puedes escribir a: ", a("correo@ejemplo.com", href = "mailto:correo@ejemplo.com")),
        p("Selecciona una comunidad autónoma y un municipio para comenzar a explorar. 🔍")
      )
    ),
    
    conditionalPanel(
      condition = "output.showMapPanels",
      tabsetPanel(
        
        ####----------------------Usos del suelo------------------------------
        tabPanel("Usos del suelo",
                 tabsetPanel(
                   tabPanel("Corine nivel 1",
                            info_corine_ui,
                            withSpinner(plotOutput("Corine1"), type = 4, color = "#2c7a7b"),
                            br(),
                            info2_corine_ui,
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(plotOutput("barras_corine_1"),
                                        type = 4,
                                        color = "#2c7a7b")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(uiOutput("textocorine1"), type = 4, color = "#2c7a7b")),
                            br(),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                h4("Descargar capas"),
                                selectInput("formato_descarga_corine_n1", "Formato de descarga:",
                                            choices = c("GeoJSON" = "geojson", "SHP" = "shp", "GeoPackage" = "gpkg")),
                                downloadButton("desc_corine_n1", "Descargar capa del municipio y sus alrededores"),
                                downloadButton("desc_corine_n1_recortada", "Descargar capa del municipio")),
                            br(),
                            nota_corine_ui,
                            div(style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                                img(src = "Logoshiny.png", style = "max-width: 200px; margin-bottom: 1em;"))
                   ),
                   
                   tabPanel("Corine nivel 2",
                            info_corine_ui,
                            withSpinner(plotOutput("Corine2"), type = 4, color = "#2c7a7b"),
                            br(),
                            info2_corine_ui,
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(plotOutput("barras_corine_2"),
                                        type = 4,
                                        color = "#2c7a7b")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("selector_uso_corine_2")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("explicacion_uso_2")),
                                
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("textocorine2")),
                            br(),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                h4("Descargar capas"),
                                selectInput("formato_descarga_corine_n2", "Formato de descarga:",
                                            choices = c("GeoJSON" = "geojson", "SHP" = "shp", "GeoPackage" = "gpkg")),
                                downloadButton("desc_corine_n2", "Descargar capa del municipio y sus alrededores"),
                                downloadButton("desc_corine_n2_recortada", "Descargar capa del municipio")),
                            br(),
                            nota_corine_ui,
                            div(style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                                img(src = "Logoshiny.png", style = "max-width: 200px; margin-bottom: 1em;"))
                   ),
                   
                   tabPanel("Corine nivel 3",
                            info_corine_ui,
                            withSpinner(plotOutput("Corine3"), type = 4, color = "#2c7a7b"),
                            br(),
                            info2_corine_ui,
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(plotOutput("barras_corine_3"),
                                            type = 4,
                                            color = "#2c7a7b")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("selector_uso_corine_3")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("explicacion_uso_3")),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(uiOutput("textocorine3"),
                                            type = 4,
                                            color = "#2c7a7b")),
                            br(),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                h4("Descargar capas"),
                                selectInput("formato_descarga_corine_n3", "Formato de descarga:",
                                            choices = c("GeoJSON" = "geojson", "SHP" = "shp", "GeoPackage" = "gpkg")),
                                downloadButton("desc_corine_n3", "Descargar capa del municipio y sus alrededores"),
                                downloadButton("desc_corine_n3_recortada", "Descargar capa del municipio")),
                            br(),
                            nota_corine_ui,
                            div(style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                                img(src = "Logoshiny.png", style = "max-width: 200px; margin-bottom: 1em;"))
                   )
                 )
        ),
        
        ####------------------------------ Litología---------------------------
        tabPanel("Litología",
                 tabsetPanel(
                   tabPanel("Litología nivel 1",
                            info_litologia_ui,
                            withSpinner(plotOutput("Litologia1"), 
                                        type = 4, 
                                        color = "#2c7a7b"),
                            br(),
                            info2_litologia_ui,
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(plotOutput("barras_litologia_1"),
                                            type = 4,
                                            color = "#2c7a7b")),
                            
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                withSpinner(uiOutput("textolitologia1"),
                                            type = 4,
                                            color = "#2c7a7b")),
                            br(),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                h4("Descargar capas"),
                                selectInput("formato_descarga_litologia_n1", "Formato de descarga:",
                                            choices = c("GeoJSON" = "geojson", "SHP" = "shp", "GeoPackage" = "gpkg")),
                                downloadButton("desc_litologia_n1", "Descargar capa del municipio y sus alrededores"),
                                downloadButton("desc_litologia_n1_recortada", "Descargar capa del municipio")),
                            br(),
                            nota_litologia_ui,
                            div(style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                                img(src = "Logoshiny.png", style = "max-width: 200px; margin-bottom: 1em;"))
                   ),
                   tabPanel("Litología nivel 2",
                            info_litologia_ui,
                            withSpinner(plotOutput("Litologia2"), type = 4, color = "#2c7a7b"),
                            br(),
                            info2_litologia_ui,
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                uiOutput("textolitologia2")),
                            br(),
                            div(style = "padding: 1em; background-color: var(--bs-body-bs); border-radius: 5px; margin-bottom: 1em;",
                                h4("Descargar capas"),
                                selectInput("formato_descarga_litologia_n2", "Formato de descarga:",
                                            choices = c("GeoJSON" = "geojson", "SHP" = "shp", "GeoPackage" = "gpkg")),
                                downloadButton("desc_litologia_n2", "Descargar capa del municipio y sus alrededores"),
                                downloadButton("desc_litologia_n2_recortada", "Descargar capa del municipio")),
                            br(),
                            nota_litologia_ui,
                            div(style = "padding: 2em; text-align: center; background-color: var(--bs-body-bg);",
                                img(src = "Logoshiny.png", style = "max-width: 200px; margin-bottom: 1em;"))
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
        "Zonas industriales, comerciales y de transporte",
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
        "Zonas industriales, comerciales y de transporte",
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

  corine_n3_capa_completa <- reactive({
    
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
  
  corine_n3_capa_recortada <- reactive({
    
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
  
  litologia_n1_capa_completa <- reactive({
    
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
  
  litologia_n1_capa_recortada <- reactive({
    
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
  
  #-------------------------------Descarga de capas--------------------------
  
  ##---------------------- Función genérica de descarga ----------------------
  
  crear_handler_descarga <- function(nombre_base, obtener_capa_sf, formato_input) {
    downloadHandler(
      filename = function() {
        formato <- formato_input()
        ext <- switch(formato,
                      "geojson" = ".geojson",
                      "shp" = ".zip",
                      "gpkg" = ".gpkg")
        paste0(nombre_base(), ext)
      },
      content = function(file) {
        capa_sf <- obtener_capa_sf()
        formato <- formato_input()
        
        if (formato == "geojson") {
          sf::st_write(capa_sf, file, driver = "GeoJSON", delete_dsn = TRUE)
        } else if (formato == "gpkg") {
          sf::st_write(capa_sf, file, driver = "GPKG", delete_dsn = TRUE)
        } else if (formato == "shp") {
          shp_dir <- file.path(tempdir(), "shapefile_dir")
          dir.create(shp_dir, showWarnings = FALSE)
          
          shp_path <- file.path(shp_dir, paste0(nombre_base(), ".shp"))
          sf::st_write(capa_sf, shp_path, driver = "ESRI Shapefile", delete_layer = TRUE)
          
          shp_files <- list.files(shp_dir, full.names = TRUE)
          zip::zip(zipfile = file, files = shp_files, mode = "cherry-pick")
        }
      }
    )
  }
  
  ##---------------------- Descargas Corine Nivel 1 --------------------------
  
  output$desc_corine_n1 <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n1_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n1_capa_completa,
    formato_input = reactive(input$formato_descarga_corine_n1)
  )
  
  output$desc_corine_n1_recortada <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n1_recortada_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n1_capa_recortada,
    formato_input = reactive(input$formato_descarga_corine_n1)
  )
  
  ##---------------------- Descargas Corine Nivel 2 --------------------------
  
  output$desc_corine_n2 <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n2_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n2_capa_completa,
    formato_input = reactive(input$formato_descarga_corine_n2)
  )
  
  output$desc_corine_n2_recortada <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n2_recortada_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n2_capa_recortada,
    formato_input = reactive(input$formato_descarga_corine_n2)
  )
  
  ##---------------------- Descargas Corine Nivel 3 --------------------------
  
  output$desc_corine_n3 <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n3_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n3_capa_completa,
    formato_input = reactive(input$formato_descarga_corine_n3)
  )
  
  output$desc_corine_n3_recortada <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_corine_n3_recortada_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = corine_n3_capa_recortada,
    formato_input = reactive(input$formato_descarga_corine_n3)
  )
  
  ##---------------------- Descargas Litología Nivel 1 -----------------------
  
  output$desc_litologia_n1 <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_litologia_n1_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = litologia_n1_capa_completa,
    formato_input = reactive(input$formato_descarga_litologia_n1)
  )
  
  output$desc_litologia_n1_recortada <- crear_handler_descarga(
    nombre_base = reactive(paste0("capa_litologia_n1_recortada_", input$comunidad, "_", input$municipio)),
    obtener_capa_sf = litologia_n1_capa_recortada,  
    formato_input = reactive(input$formato_descarga_litologia_n1)
  )
  
  
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
        guides(fill = "none") + 
        ggnewscale::new_scale_fill()             
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
  
  ##----------------------------Grafico Corine nivel 1---------------------
  
  output$barras_corine_1 <- renderPlot({
    validar_municipio()
    
    corine_muni <- corine_n1_capa_recortada()
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> 
      select(Uso_n1, color_n1, area) |> 
      group_by(Uso_n1, color_n1) |> 
      summarise(area = sum(area), .groups = "drop")
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- as.numeric((uso_summary$area / total_area) * 100)
    

    uso_top10 <- uso_summary |> 
      arrange(desc(porcentaje)) |> 
      slice(1:5)
    
    
    uso_top10$Uso_envuelto <- str_wrap(uso_top10$Uso_n1, width = 40) 
    
    
    ggplot(uso_top10, aes(x = reorder(Uso_envuelto, porcentaje), y = porcentaje, fill = Uso_envuelto)) +
      geom_col() +
      scale_fill_manual(values = setNames(uso_top10$color_n1, uso_top10$Uso_envuelto)) +
      labs(
        title = paste("Distribución de usos del suelo (Nivel 1) en el municipio de", input$municipio),
        x = "Uso del suelo",
        y = as.character("Porcentaje (%)")
      ) +
      theme_minimal(base_size = 13) +
      coord_flip(ylim = c(0, 100)) + 
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 11)
      )
  })
  
  
  
  
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
        guides(fill = "none") + 
        ggnewscale::new_scale_fill()              
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
  
  
  ##----------------------------Grafico Corine nivel 2---------------------
  
  output$barras_corine_2 <- renderPlot({
    validar_municipio()
    
    corine_muni <- corine_n2_capa_recortada()
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> 
      select(Uso_n2, color_n2, area) |> 
      group_by(Uso_n2, color_n2) |> 
      summarise(area = sum(area), .groups = "drop")
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- as.numeric((uso_summary$area / total_area) * 100)
    
    
    # Limitar a los 10 usos principales
    uso_top10 <- uso_summary |> 
      arrange(desc(porcentaje)) |> 
      slice(1:15)
    
    uso_top10$Uso_envuelto <- str_wrap(uso_top10$Uso_n2, width = 40) 
    
    ggplot(uso_top10, aes(x = reorder(Uso_envuelto, porcentaje), y = porcentaje, fill = Uso_envuelto)) +
      geom_col() +
      scale_fill_manual(values = setNames(uso_top10$color_n2, uso_top10$Uso_envuelto)) +
      labs(
        title = paste("Distribución de usos del suelo (Nivel 2) en el municipio de", input$municipio),
        x = "Uso del suelo",
        y = as.character("Porcentaje (%)")
      ) +
      theme_minimal(base_size = 13) +
      coord_flip(ylim = c(0, 100)) + 
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 11)
      )
  })
  
  ##----------------------------------Explicacion uso n2-------------------------------
  # Generar el vector de usos únicos del municipio
  usos_municipio_n2 <- reactive({
    validar_municipio()
    corine_muni <- corine_n2_capa_completa()
    sort(unique(corine_muni$Uso_n2))  
  })
  
  # Renderizar el UI del selectInput solo si hay datos
  output$selector_uso_corine_2 <- renderUI({
    req(usos_municipio_n2())  # Espera a que haya datos
    selectInput("uso_seleccionado",
                "Selecciona un uso del suelo para ver su explicación:",
                choices = c("", usos_municipio_n2()),  
                selected = "")
    
  })
  
  # Mostrar la explicación del uso seleccionado
  output$explicacion_uso_2 <- renderText({
    req(input$uso_seleccionado)
    explicacion <- dicc_corine_2$Explicacion[dicc_corine_2$Uso == input$uso_seleccionado]
    
    if (length(explicacion) == 0) {
      "No hay descripción disponible para este uso."
    } else {
      explicacion
    }
  })
  
  
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
  output$Corine3 <- renderPlot({
    
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
        guides(fill = "none") +
        ggnewscale::new_scale_fill()             
    }
    
    p <- p +
      geom_sf(data = corine_n3_capa_completa(), aes(fill = Uso), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Usos del suelo nivel 3:",
                        values = setNames(corine_n3_capa_completa()$color,
                                          corine_n3_capa_completa()$Uso)) +
      ggtitle(paste("Mapa de usos del suelo del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
      
    print(p)
  }, bg = "transparent")
  
  ##----------------------------Grafico Corine nivel 3---------------------
  
  output$barras_corine_3 <- renderPlot({
    validar_municipio()
    
    corine_muni <- corine_n3_capa_recortada()
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> 
      select(Uso, color, area) |> 
      group_by(Uso, color) |> 
      summarise(area = sum(area), .groups = "drop")
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- as.numeric((uso_summary$area / total_area) * 100)
    
    

    uso_top10 <- uso_summary |> 
      arrange(desc(porcentaje)) |> 
      slice(1:50)
    
    uso_top10$Uso_envuelto <- str_wrap(uso_top10$Uso, width = 40) 
    
    
    ggplot(uso_top10, aes(x = reorder(Uso_envuelto, porcentaje), y = porcentaje, fill = Uso_envuelto)) +
      geom_col() +
      scale_fill_manual(values = setNames(uso_top10$color, uso_top10$Uso_envuelto)) +
      labs(
        title = paste("Distribución de usos del suelo (Nivel 3) en el municipio de", input$municipio),
        x = "Uso del suelo",
        y = as.character("Porcentaje (%)")
      ) +
      theme_minimal(base_size = 13) +
      coord_flip(ylim = c(0, 100)) + 
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 11)
      )
  })
  
  
  ##----------------------------------Explicacion uso n3-------------------------------
  # Generar el vector de usos únicos del municipio
  usos_municipio_n3 <- reactive({
    validar_municipio()
    corine_muni <- corine_n3_capa_completa()
    sort(unique(corine_muni$Uso))  
  })
  
  # Renderizar el UI del selectInput solo si hay datos
  output$selector_uso_corine_3 <- renderUI({
    req(usos_municipio_n3())  # Espera a que haya datos
    selectInput("uso_seleccionado",
                "Selecciona un uso del suelo para ver su explicación:",
                choices = c("", usos_municipio_n3()),  
                selected = "")
    
  })
  
  # Mostrar la explicación del uso seleccionado
  output$explicacion_uso_3 <- renderText({
    req(input$uso_seleccionado)
    explicacion <- dicc_corine_3$Explicacion[dicc_corine_3$Uso == input$uso_seleccionado]
    
    if (length(explicacion) == 0) {
      "No hay descripción disponible para este uso."
    } else {
      explicacion
    }
  })
  
  
  ##-------------------------Texto Corine nivel 3---------------------------
  
  output$textocorine3 <- renderUI({
    
    validar_municipio()
    
    corine_muni <- corine_n3_capa_recortada()
    
    corine_muni$area <- st_area(corine_muni)
    
    uso_summary <- corine_muni |> select(Uso, color, area)
    
    total_area <- sum(uso_summary$area)
    uso_summary$porcentaje <- (uso_summary$area / total_area) * 100
    
    top_usos <- uso_summary |> arrange(desc(porcentaje)) |> slice(1:3)
    
    # Texto con fondo de color, ajustando color de texto si el fondo es oscuro
    textos_top <- sapply(seq_len(nrow(top_usos)), function(i) {
      uso <- top_usos$Uso[i]
      color_fondo <- top_usos$color[i]
      
      # Verificamos si el uso corresponde al que tiene fondo oscuro
      color_texto <- if (uso == "Zonas quemadas") {  
        "white"
      } else {
        "black"
      }
      
      paste0(
        i, '. <span style="background-color:', color_fondo, 
        '; color:', color_texto, 
        '; padding: 4px 8px; border-radius: 6px;">',
        uso, 
        '</span> (', 
        round(top_usos$porcentaje[i], 2), '%)'
      )
    })
    
    HTML(paste0(
      "<b>En el municipio de ", input$municipio, ", los principales usos del suelo son:</b><br><br>",
      paste(textos_top, collapse = "<br>")
    ))
  })
  
  
  ##-----------------------------Mapa Litologia general----------------------- 
  output$Litologia1 <- renderPlot({
    
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
        geom_sf(data = visible_area(), fill = "gray80") +
        geom_sf(data = hypsobath_crop,
                aes(fill = as.factor(val_inf)),
                colour = NA) +
        scale_fill_manual(
          values = bath_tints(length(levels))) +
        guides(fill = "none") + 
        ggnewscale::new_scale_fill()               # reiniciar escala fill
    }
    
    p <- p +
      
      geom_sf(data = litologia_n1_capa_completa(), aes(fill = LITOLOGIA), color = NA) +
      geom_sf(data = municipio_sf, color = "black", fill = NA, linewidth = 1.5) +
      geom_sf(data = area_fuera_municipio, fill = "gray", alpha = 0.6) +
      scale_fill_manual(name = "Litología", 
                        values = setNames(litologia_n1_capa_completa()$color, 
                                          litologia_n1_capa_completa()$litologia)) +
      ggtitle(paste("Mapa de litologias del municipio de:\n", input$municipio)) +
      capas_gg_comunes()
    
    print(p)
  }, bg = "transparent")
  
  ##-----------------------------Texto litología nivel 1 -------------------
  
  output$textolitologia1 <- renderUI({
    
    validar_municipio()
    
    litologia_muni <- litologia_n1_capa_recortada()
    
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
  
  ##----------------------------Grafico Litología nivel 1---------------------
  
  output$barras_litologia_1 <- renderPlot({
    validar_municipio()
    
    litologia_muni <- litologia_n1_capa_recortada()
    litologia_muni$area <- st_area(litologia_muni)
    
    lit_summary <- litologia_muni |> 
      select(LITOLOGIA, color, area) |> 
      group_by(LITOLOGIA, color) |> 
      summarise(area = sum(area), .groups = "drop")
    
    total_area <- sum(lit_summary$area)
    lit_summary$porcentaje <- as.numeric((lit_summary$area / total_area) * 100)
    
    
    # Limitar a los 10 usos principales
    lit_top10 <- lit_summary |> 
      arrange(desc(porcentaje)) |> 
      slice(1:50)
    
    lit_top10$lit_envuelto <- str_wrap(lit_top10$LITOLOGIA, width = 40) 
    
    ggplot(lit_top10, aes(x = reorder(lit_envuelto, porcentaje), y = porcentaje, fill = lit_envuelto)) +
      geom_col() +
      scale_fill_manual(values = setNames(lit_top10$color, lit_top10$lit_envuelto)) +
      labs(
        title = paste("Distribución de litologías en el municipio de", input$municipio),
        x = "Litología",
        y = as.character("Porcentaje (%)")
      ) +
      theme_minimal(base_size = 13) +
      coord_flip(ylim = c(0, 100)) + 
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 11)
      )
  })
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
