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

#setwd("C:/Users/Ulises Soto/Desktop/Uriel/UVG/DataScience/Lab9")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  #DT::dataTableOutput("sample_table")
  #plotOutput("barplot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "pruebaInput",
        "Selecciona la variable visualizar",
        choices = c("marca","modelo"),
      )
    ),
    mainPanel(plotOutput("barplot"))
  )
  
  # Sidebar with a slider input for number of bins 
  #sidebarLayout(
  #   sidebarPanel(
  #      sliderInput("bins",
  #                 "Number of bins:",
  #                min = 1,
  #               max = 50,
  #              value = 30)
  #),
  
  # Show a plot of the generated distribution
  #mainPanel(
  #  plotOutput("distPlot")
  #)
  #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df_fallecidos <- reactive({
    df <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
    return(df)
  })
  
  
  #Info para obtener la informacion del barplot
  df_sat_marca <- reactive({
    importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
    importaciones <- na.omit(importaciones)
    nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Marca))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,2])
  })
  
  df_sat_marca1 <- reactive({
    importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
    importaciones <- na.omit(importaciones)
    nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Marca))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,1])
  })
  
  df_marca3 <- reactive({
    importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
    importaciones <- na.omit(importaciones)
    nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,2])
  })
  
  df_marca4 <- reactive({
    importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
    importaciones <- na.omit(importaciones)
    nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
    marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
    marca <- marca[order(-marca$Freq),]
    marca2 <- marca[1:10,]
    return(marca2[,1])
  })
  
  #output$barplot <- renderPlot({
  #  ggplot(data=df_sat_marca())
  #})
  
  output$barplot <- renderPlot({
    if(input$pruebaInput == "marca"){
      barplot(df_sat_marca(), 
              names = as.vector(df_sat_marca1()), 
              las = 2,
              main = "Marca"
      ) 
    }else{
      barplot(df_marca3(),
              names = as.vector(df_marca4()),
              las = 2,
              main = "Modelo"
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