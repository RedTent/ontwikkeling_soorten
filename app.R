
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)

meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv")
data <- import_bio("data/biologie.csv")
taxatypen <- read_csv2("data/taxatype.csv", col_types = "cc") %>% df_to_named_list()


# UI
ui <- fluidPage(
   
   
   titlePanel("Ontwikkeling soorten"),
   
   
   sidebarLayout(
      sidebarPanel(
        selectInput("taxatype_sel", "Kies een taxontype", choices = taxatypen, selected = "MACFT"),
        selectInput("taxon_sel", "Kies een taxon", choices = taxatypen ), 
        sliderInput("jaar_sel","Geselecteerd jaar", min = min(data$jaar), max = max(data$jaar), value = min(data$jaar), animate = TRUE, step = 1, sep = "")
      ), # end side bar
      
      mainPanel(
         leafletOutput("kaart")
      )
   )
) # end of UI

# SERVER
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$kaart <- renderLeaflet({
     leaflet() %>% addTiles() %>% addCircleMarkers(data=meetpuntendf, label = ~mp)
     
   }) # end kaart
   
   #leafletProxy(mapId = "kaart")
   
   observe({
     taxalijst <- data %>% filter(taxatype == input$taxatype_sel) %>% select(naam) %>% unique() %>% c(recursive=FALSE)
     print(taxalijst)
     updateSelectInput(session, inputId = "taxon_sel", choices = taxalijst)
   })
   
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

