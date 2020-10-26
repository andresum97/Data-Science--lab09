#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(randomcoloR)
library(plotrix)
library(leaflet)
library(plyr)

#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Proyecto-01/Mineria_proyecto_01")
#setwd("C:/Users/Ulises Soto/Desktop/Uriel/UVG/DataScience/Lab9")
setwd("C:/Users/Guillermo/Desktop/SegundoSemestre/DataScience/Lab9/")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Moticlismo en Guatemala - Una perspectiva desde dentro"),
  #DT::dataTableOutput("sample_table")
  #plotOutput("barplot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "barplotInput",
        "Selecciona la variable visualizar",
        choices = c("Marca de motocicleta","Modelo de motocicleta"),
      )
    ),
    mainPanel(
      plotOutput("barplot"),
      tags$p("Según el análisis de datos realizados de las importaciones de vehículo en Guatemala,  revela que la marca que más motos comercializa en el país es Suzuki y Honda, las cuales están muy por encima del resto.  "),
      tags$p("Ambas graficas, muestran entre las marcas de motocicletas y modelos por año de motocicletas que más se utilizan en el país")
    )
  ),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "circleplotInput",
        "Selecciona la variable visualizar",
        choices = c("Fallecidos por departamento","Lesionados por departamento"),
      )
    ),
    
    mainPanel(
      plotOutput("circleplot"),
      tags$p("Con respecto a la incidencia en motos, es que la mayor parte de accidentes en moto ocurre en la ciudad de Guatemala, seguido por departamentos como Escuintla y Alta Verapaz, pero en proporciones menores."),
      tags$p("Ambas gráficas, muestran los fallecidos y lesionados por departamento de Guatemala")
      )
  ),
  
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "tiempo",
                  label = "Anios de importacion",
                  min = 2011,
                  max = 2018,
                  value = 2014,
                  width = "250px"),
    ),
    mainPanel(
      plotOutput(outputId = "barPlot2"),
      tags$p("Con respecto a las importaciones, se puede observar que la mayor cantidad de importaciones cada anio provienen de Puerto Quetzal, seguido de la Central de Guatemala y de Express Aereo"),

    )
  ),
  
  br(),
  br(),
  h5("David Soto - 17551", align = 'right'),
  h5("Guillermo Sandoval - 17577", align = 'right'),
  h5("Andres Urizar - 17632", align = 'right'),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
  fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
  
  
  #Info para obtener la informacion del barplot
  df_sat_marca <- reactive({
    importaciones1 <- na.omit(importaciones)
    nuevoImportaciones = importaciones1[importaciones1$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Marca))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,2])
  })
  
  df_sat_marca1 <- reactive({
    importaciones2 <- na.omit(importaciones)
    nuevoImportaciones = importaciones2[importaciones2$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Marca))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,1])
  })
  
  df_marca3 <- reactive({
    importaciones3 <- na.omit(importaciones)
    nuevoImportaciones = importaciones3[importaciones3$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,2])
  })
  
  df_marca4 <- reactive({
    importaciones4 <- na.omit(importaciones)
    nuevoImportaciones = importaciones4[importaciones4$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,1])
  })
  
  
  colorsMarca <- reactive({
    importacionescol1 <- na.omit(importaciones)
    colMarca <- distinctColorPalette(length(table(importaciones$Marca)))
    return(colMarca)
  })
  
  colorsModelo <- reactive({
    importacionescol2 <- na.omit(importaciones)
    colModelo <- distinctColorPalette(length(table(importaciones$Modelo)))
    return(colModelo)
  })
  
  #====================== Fallecidos
  #Datos
  df_fallecidos <- reactive({
    fallecidos_moto = fallecidos[fallecidos$tipo_veh == "4",]
    fallecidos_f = fallecidos_moto[fallecidos_moto$fall_les == "1",]
    fallecidos_f$depto_ocu <- mapvalues(fallecidos_f$depto_ocu, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), c("Guatemala","El Progreso","SacatepÃ©quez","Chimaltenango","Escuintla","Santa Rosa","SololÃ¡","TotonicapÃ¡n","Quetzaltenango","SuchitepÃ©quez","Retalhuleu","San Marcos","Huehuetenango","QuichÃ©","Baja Verapaz","Alta Verapaz","PetÃ©n","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"))
    fall_data <- table(fallecidos_f$depto_ocu)
    return(fall_data)
  })
  
  #======================= Lesionados
  #Datos
  df_lesionados <- reactive({
    fallecidos_moto_l = fallecidos[fallecidos$tipo_veh == "4",]
    fallecidos_l = fallecidos_moto_l[fallecidos_moto_l$fall_les == "2",]
    fallecidos_l$depto_ocu <- mapvalues(fallecidos_l$depto_ocu, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), c("Guatemala","El Progreso","SacatepÃ©quez","Chimaltenango","Escuintla","Santa Rosa","SololÃ¡","TotonicapÃ¡n","Quetzaltenango","SuchitepÃ©quez","Retalhuleu","San Marcos","Huehuetenango","QuichÃ©","Baja Verapaz","Alta Verapaz","PetÃ©n","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"))
    fall_data_l <- table(fallecidos_l$depto_ocu)
    return(fall_data_l)
  })
  
  #====================== Pruebas LEAFLET
  
  
  
  #output$barplot <- renderPlot({
  #  ggplot(data=df_sat_marca())
  #})
  
  output$barplot <- renderPlot({
    if(input$barplotInput == "Marca de motocicleta"){
      barplot(df_sat_marca(), 
              names = as.vector(df_sat_marca1()), 
              col = rainbow(length(as.vector(df_sat_marca1()))),
              las = 2,
              main = "Cantidad de motocicletas por Marca en Guatemala"
      ) 
    }else{
      barplot(df_marca3(),
              names = as.vector(df_marca4()),
              col = rainbow(length(as.vector(df_marca4()))),
              las = 2,
              main = "Cantidad de motocicletas por Modelo en Guatemala"
      )
    }
    
  })
  
  output$circleplot <- renderPlot({
    if(input$circleplotInput == "Fallecidos por departamento"){
      pie(df_fallecidos(), 
              col = rainbow(length(df_fallecidos())),
              las = 2,
              main = "Proporción de fallecimientos por departamento en incidentes de moto en Guatemala"
      ) 
    }else{
      pie(df_lesionados(), 
          col = rainbow(length(df_lesionados())),
          las = 2,
          main = "Proporción de lesionados por departamento en incidentes de moto en Guatemala"
      ) 
    }
    
  })
  
  #Datos a utilizar
  
  importaciones1 <- na.omit(importaciones)
  nuevoImportaciones = importaciones1[importaciones1$Tipo.de.Vehiculo == "MOTO",]
  importaciones <- data.frame(table(nuevoImportaciones$Aduana.de.Ingreso))
  
  imp <- data.frame(table(nuevoImportaciones[,c('Aduana.de.Ingreso','Anio')]))
  
  #model <- lm(nuevoImportaciones$Anio ~ freq~freq)
  
  output$barPlot2 <- renderPlot({
    
    tiempo <- seq(min(importaciones), max(importaciones), length.out = input$tiempo)
    imp = subset.data.frame(imp, imp$Anio==input$tiempo)
    barplot((table(imp)),
            height = imp$Freq,
            col = rainbow(length(as.vector(nuevoImportaciones))),
            main='Importaciones por lugar',
            xlab='Lugar de importacion',
            ylab='Frecuencia')
    
    #barplot((table(nuevoImportaciones$Aduana.de.Ingreso)),
    #       col = rainbow(length(as.vector(nuevoImportaciones))),
    #        main='Importaciones por lugar',
    #        xlab='Lugar de importacion',
    #        ylab='Frecuencia')
    
    #hist(importaciones, col = "#75AADB", border = "black",
    #     xlab = "Lugar de importacion",
    #     ylab = "Frecuencia",
    #     main = "Histograma de importaciones")
    
  })
  
  
  output$hist <- renderPlot({
    hist(rnorm(input$time))
  })
  
  
  output$map <- renderLeaflet({
    mydata = mapData()
    leaflet(mydata) %>% 
      setView(lng = 0, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = mydata, lat = ~ Lat, lng = ~ Long, weight = 1, radius = ~sqrt(Activos)*90, color = 'orange', fillOpacity = 0.5)
  })
  
  #  output$sample_table<- DT::renderDataTable({
  #     df <- df_sat_marca()
  #    DT::datatable(df)
  #})
  
  # output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  #    x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
