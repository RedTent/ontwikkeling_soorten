
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)


HHSKthema()
meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv")
data <- import_bio("data/biologie.csv")
taxatypen <- read_csv2("data/taxatype.csv", col_types = "cc") %>% df_to_named_list()


# UI
ui <- fluidPage(
   
   
   titlePanel("Ontwikkeling soorten"),
   
   
   sidebarLayout(
      sidebarPanel(
        selectInput("taxatype_sel", "Kies een taxontype", choices = taxatypen, selected = "MACFT"),
        selectInput("taxon_sel", "Kies een taxon", choices = taxatypen, multiple = TRUE), 
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
     mp_jaar <- mp_sel_jaar()
     leafletProxy(mapId = "kaart") %>% clearMarkers()
     if(nrow(mp_jaar)>0){leafletProxy(mapId = "kaart") %>% addCircleMarkers(data = mp_jaar, label = ~mp, lng = ~long, lat = ~lat, color = "grey", radius = 3, fill = FALSE)}
     if(nrow(mp_selectie)>0){leafletProxy(mapId = "kaart") %>% addCircleMarkers(data = mp_selectie, label = ~mp, lng = ~long, lat = ~lat, opacity = 1, fillOpacity = 1, radius = 8, color = hhskgroen)}
   }) 
   
   #update taxa keuzelijst
   observe({
     taxalijst <- data %>% filter(taxatype == input$taxatype_sel) %>% select(naam) %>% arrange(naam) %>%  unique() %>% c(recursive=FALSE)
     updateSelectInput(session, inputId = "taxon_sel", choices = taxalijst)
   })
   
   mp_sel_taxon <- reactive({
     data_sel <- data %>% filter(taxatype == input$taxatype_sel, naam %in% input$taxon_sel, jaar == input$jaar_sel)
     mp_sel_taxon <- meetpuntendf %>% semi_join(data_sel, by = "mp")
     #print(mp_sel_taxon)
     mp_sel_taxon
   })
   
   mp_sel_jaar <- reactive({
     data_sel_jaar <- data %>% filter(taxatype == input$taxatype_sel, jaar == input$jaar_sel)
     mp_sel_jaar <- meetpuntendf %>% semi_join(data_sel_jaar, by = "mp")
     #print(mp_sel_jaar)
     mp_sel_jaar
   })
   
   #observe({print(mp_sel()) })
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

