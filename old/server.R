######

# load packages
library(shiny)
library(leaflet)
require(RCurl)
require(RJSONIO)
require(plyr)
library(rgdal)
library(rgeos)
library(dplyr)
library(gtools)
library(jsonlite)
library(mongolite)
library(rgdal)
library(DT)
library(leaflet.extras)

# load functions
BING <- function(str){
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG"))
  d <- getURL(u)
  j <- RJSONIO::fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  data<-c(lat,lng)
  #data[3]<-"BING"
  data[4]<- str
  data2<-data.frame(address=c(data[4]), lat=c(data[1]), long=c(data[2]))
  return(data2)
}  

# server file
shinyServer(function(input, output) {
  
  
dataorig <- reactiveVal(
    tibble(address=character() 
           #lat=NA, 
           #long=NA,
           #datefrom=NA,
           #dateto=NA
           ))

observeEvent(input$goButton,{

data<-BING(input$str)

output$table<-renderTable({
  dataorig() %>%
    add_row(
      address = input$str
      #lat = data$lat,
      #long = data$long,
      #datefrom = input$date3,
      #dateto = input$date4
    ) %>%
    dataorig()
})

})

  
observe({
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
            addProviderTiles("Stamen.Toner", group = "Stamen Toner") %>%
            addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
            addLayersControl(
              baseGroups = c("CartoDB Positron", "OSM", "Toner"),
              options = layersControlOptions(collapsed = TRUE)) %>%
            addLegend("bottomright", 
                      colors=c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"),
                      title = "Level of Change (number)",
                      labels= c("Loss: >20", "Loss: 10-19", "Loss: 5-9", "Loss: <5","No change",  "Gain: <5", "Gain: 5-9", "Gain: 10-19", "Gain: >20"),
                      opacity =  0.7
            ) %>%
            addScaleBar(position = c("bottomleft"))%>%
            addFullscreenControl() 
    
    
    

    ###################################################################################################################    
    
    # this ones for the Shinyserver  
  })
  
  # this ones for the map view options
})
  
  
  
  
  
})