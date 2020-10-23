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

#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Proyecto-01/Mineria_proyecto_01")
setwd("C:/Users/Ulises Soto/Desktop/Uriel/UVG/DataScience/Lab9")

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
    mainPanel(plotOutput("circleplot"))
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
  
  #output$barplot <- renderPlot({
  #  ggplot(data=df_sat_marca())
  #})
  
  output$barplot <- renderPlot({
    if(input$barplotInput == "Marca de motocicleta"){
      barplot(df_sat_marca(), 
              names = as.vector(df_sat_marca1()), 
              col = rainbow(length(as.vector(df_sat_marca1()))),
              las = 2,
              main = "Cantiiad de motocicletas por Marca en Guatemala"
      ) 
    }else{
      barplot(df_marca3(),
              names = as.vector(df_marca4()),
              col = rainbow(length(as.vector(df_marca4()))),
              las = 2,
              main = "Cantiadad de motocicletas por Modelo en Guatemala"
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