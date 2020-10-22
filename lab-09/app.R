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

setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Proyecto-01/Mineria_proyecto_01")

# Define UI for application that draws a histogram
ui <- fluidPage(
    #title = span("Laboratorio 09", styl)

    # Application title
    titlePanel("Laboratorio 09"),
    #DT::dataTableOutput("sample_table")
    #plotOutput("barplot"),
    
    sidebarLayout(
      mainPanel(
        h3("David Soto - 17"),
        h3("Guillermo Sandoval - 17"),
        h3("Andres Urizar - 17"),
        br(),
        br(),
        plotOutput("barplot",width = "100%")
        # fluidRow(
        #   column(12, align="center",
        # )
      ),
      sidebarPanel(
        helpText("Puedes visualizar entre modelos o marcas de motocicletas"),
        selectInput(
          "pruebaInput",
          "Selecciona la variable visualizar",
          choices = c("marca","modelo"),
        )
      )
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
        marca2 <- marca[order(marca[, 2], decreasing=TRUE), ]
        marca2 <- filter(marca2, marca2$Freq > 10000)
        return(marca2[,2])
    })
    
    df_sat_marca1 <- reactive({
      importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
      importaciones <- na.omit(importaciones)
      nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
      marca <- data.frame(table(nuevoImportaciones$Marca))
      marca2 <- marca[order(marca[, 2], decreasing=TRUE), ]
      marca2 <- filter(marca2, marca2$Freq > 10000)
      return(marca2[,1])
    })
    
    df_marca3 <- reactive({
      importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
      importaciones <- na.omit(importaciones)
      nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
      marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
      marca2 <- marca[order(marca[, 2], decreasing=TRUE), ]
      marca2 <- filter(marca2, marca2$Freq > 10000)
      return(marca2[,2])
    })
    
    df_marca4 <- reactive({
      importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
      importaciones <- na.omit(importaciones)
      nuevoImportaciones = importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
      marca <- data.frame(table(nuevoImportaciones$Modelo.del.Vehiculo))
      marca2 <- marca[order(marca[, 2], decreasing=TRUE), ]
      marca2 <- filter(marca2, marca2$Freq > 10000)
      return(marca2[,1])
    })
    
    colorsMarca <- reactive({
      importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
      importaciones <- na.omit(importaciones)
      colMarca <- distinctColorPalette(length(table(importaciones$Marca)))
      return(colMarca)
    })
    
    colorsModelo <- reactive({
      importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
      importaciones <- na.omit(importaciones)
      colMarca <- distinctColorPalette(length(table(importaciones$Modelo)))
      return(colMarca)
    })
    #output$barplot <- renderPlot({
     #  ggplot(data=df_sat_marca())
    #})
    
    output$barplot <- renderPlot({
      if(input$pruebaInput == "marca"){
        barplot(df_sat_marca(), 
                names = as.vector(df_sat_marca1()), 
                col = colorsMarca(),
                las = 2,
                main = "Marca de motocicletas",
        ) 
      }else{
        barplot(df_marca3(),
                names = as.vector(df_marca4()),
                col = colorsModelo(),
                las = 2,
                main = "Modelo de motocicletas"
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
