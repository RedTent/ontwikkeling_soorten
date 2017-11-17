
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)
library(rgdal)


HHSKthema()
#meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv")
meetpuntendf <- import_meetpunten("data/meetpunten2.csv")
data <- import_bio("data/biologie.csv")
taxatypen <- read_csv2("data/taxatype.csv", col_types = "cc") %>% filter(taxatype_code %in% c("MACFT","MACEV","VISSN","FYTPT","DIATM","ZOOPT")) %>% arrange(taxatype_naam) %>% df_to_named_list()

waterschapsgrens <- readOGR(dsn='data/shape/wsgrens2.shp', stringsAsFactors = FALSE)
#addPolylines(data = waterschapsgrens, color = "red", weight = "3")


# UI
ui <- fluidPage(theme = "shiny_ORIG_JT.css",
   
   
   titlePanel(title = p(img(src = "logo website.png", id="HHSK_logo", height=80), "ONTWIKKELING SOORTEN", align="center",style="padding-bottom:40px"), windowTitle = "HHSK - Ontwikkeling soorten"),
   
   
   sidebarLayout(
      sidebarPanel(
        selectInput("taxatype_sel", "Kies een taxontype", choices = taxatypen, selected = "MACFT"),
        selectInput("taxon_sel", "Kies een taxon", choices = taxatypen, multiple = TRUE, selected = "Stratiotes aloides"), 
        checkboxInput("ned_namen", "Gebruik Nederlandse namen"),
        htmlOutput("opm_ned_namen"),
        HTML("</br>"),
        sliderInput("jaar_sel","Geselecteerd jaar", min = min(data$jaar), max = max(data$jaar), value = min(data$jaar), animate = TRUE, step = 1, sep = ""),
        HTML("</br>"),
        htmlOutput("samvat")
      ), # end side bar
      
      mainPanel(
         leafletOutput("kaart", height = 800)
      )
   )
) # end of UI

# SERVER
server <- function(input, output, session) {
   
  #basiskaart
   output$kaart <- renderLeaflet({
     leaflet() %>% addTiles() %>% 
       addPolylines(data = waterschapsgrens, color = "red", weight = "3") %>% 
       #addCircleMarkers(data=meetpuntendf, label = ~mp) %>% 
       addLegend(colors= c(hhskgroen,"grey"), labels = c("Aangetroffen", "Niet aangetroffen")) 
     
   }) # end kaart
   
   #update kaart
   observe({
     mp_selectie <- mp_sel_taxon()
     mp_jaar <- mp_sel_jaar()
     leafletProxy(mapId = "kaart") %>% clearMarkers()
     if(nrow(mp_jaar)>0){leafletProxy(mapId = "kaart") %>% addCircleMarkers(data = mp_jaar, label = ~mp, lng = ~long, lat = ~lat, color = "grey", radius = 3, fill = FALSE)}
     if(nrow(mp_selectie)>0){leafletProxy(mapId = "kaart") %>% addCircleMarkers(data = mp_selectie, label = ~mp, lng = ~long, lat = ~lat, opacity = 1, fillOpacity = 1, radius = 8, color = hhskgroen)}
   }) 
   
   
   #update taxa keuzelijst
   observe({
     if(input$ned_namen){
       taxalijst <- data %>% filter(taxatype == input$taxatype_sel, !is.na(nednaam)) %>% select(naam,nednaam) %>% 
       unique() %>% arrange(nednaam) %>% df_to_named_list() }
     else{     
     taxalijst <- data %>% filter(taxatype == input$taxatype_sel) %>% select(naam) %>% arrange(naam) %>%  unique() %>% c(recursive=FALSE)} # end of else = Latijnse namen
     updateSelectInput(session, inputId = "taxon_sel", choices = taxalijst) 
   })
   
   #update jaar_sel
   observe({
     jaren <- data %>% filter(taxatype == input$taxatype_sel) %>% select(jaar)
     minjaar <- min(jaren, na.rm=TRUE)
     maxjaar <- max(jaren, na.rm=TRUE)
     updateSliderInput(session, inputId = "jaar_sel", min = minjaar, max = maxjaar, value = minjaar)
    })
   
   #update ned_namen
   observe({
     if(input$taxatype_sel %in% c("DIATM","FYTPT","ZOOPT","MACEV")){updateCheckboxInput(session, inputId = "ned_namen", value = FALSE )}
   })
   
   mp_sel_taxon <- reactive({
     data_sel <- data %>% filter(taxatype == input$taxatype_sel, naam %in% input$taxon_sel, jaar == input$jaar_sel)
     mp_sel_taxon <- meetpuntendf %>% semi_join(data_sel, by = "mp")
     mp_sel_taxon
   })
   
   mp_sel_jaar <- reactive({
     data_sel_jaar <- data %>% filter(taxatype == input$taxatype_sel, jaar == input$jaar_sel)
     mp_sel_jaar <- meetpuntendf %>% semi_join(data_sel_jaar, by = "mp")
     mp_sel_jaar
   })
   
   output$opm_ned_namen <- renderText({
     if(input$ned_namen){"<b>N.B.</b> Alleen taxa met een Nederlandse naam worden weergegeven. Veel taxa hebben alleen een Latijnse naam. </br></br>
       Planten en vissen hebben gewoonlijk een Nederlandse naam."}else{""}
   })
   
   output$samvat <- renderText({
     taxastring <- paste(as.vector(input$taxon_sel, mode = "character"), collapse = ", ")
     paste0("In het jaar <b>", input$jaar_sel, "</b> is/zijn de geselecteerde soort(en) op <b>", nrow(mp_sel_taxon()),"</b> van de <b>",nrow(mp_sel_jaar()), "</b> locaties aangetroffen")
     
   })
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

