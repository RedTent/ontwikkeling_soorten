
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
         leafletOutput("kaart", height = 800)
      )
   )
) # end of UI

# SERVER
server <- function(input, output, session) {
   

   
   output$kaart <- renderLeaflet({
     leaflet() %>% addTiles() %>% addCircleMarkers(data=meetpuntendf, label = ~mp)
     
   }) # end kaart
   
   observe({
     mp_selectie <- mp_sel_taxon()
     leafletProxy(mapId = "kaart") %>% clearMarkers()
     if(nrow(mp_selectie)>0){leafletProxy(mapId = "kaart") %>% addCircleMarkers(data = mp_selectie, label = ~mp, lng = ~long, lat = ~lat)}
     
   }) 
   
   #update taxa keuzelijst
   observe({
     taxalijst <- data %>% filter(taxatype == input$taxatype_sel) %>% select(naam) %>% unique() %>% c(recursive=FALSE)
     updateSelectInput(session, inputId = "taxon_sel", choices = taxalijst)
   })
   
   mp_sel_taxon <- reactive({
     data_sel <- data %>% filter(taxatype == input$taxatype_sel, naam == input$taxon_sel, jaar == input$jaar_sel)
     mp_sel_taxon <- meetpuntendf %>% semi_join(data_sel, by = "mp")
     print(mp_sel_taxon)
     mp_sel_taxon
   })
   
   #observe({print(mp_sel()) })
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

