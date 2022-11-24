library(shiny)
library(tidyverse)
library(leaflet)
library(data.table)
library(sf)
library(shinyjs)
library(DT)
library(leaflet.extras)
library(RCurl)

# use haggis for historical?
BING <- function(str){
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=", Sys.getenv(c("BINGKEY"))))
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
  data
}  

#ui.r
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  # Fonts
  tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
  tags$head(
    tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))),
  # Header panel
  headerPanel(
    fluidRow(
      column(11, h1("LifeWorkPlaces",style = "font-family: 'Roboto Slab', cursive;font-weight: bold; font-size: 39px"))),
    windowTitle = "Lifeplaces"),

  
  sidebarPanel(
    div(style="display:inline-block", textInput("str", label =("Enter Address"), value = "")),
    selectInput("type", "Type of Address:", choices = c("Home", "Work", "Other")),
    dateInput("date3", "Date from:", value = Sys.Date(), format = "mm/dd/yy"),
    dateInput("date4", "Date to:", value = Sys.Date(), format = "mm/dd/yy"),
    textInput("comment", "Address Comments:"),
    # maybe occupation? have to change to bring in if work selected above
    textInput("jobtitle:", "Job title:"),
    #textInput("SIC", "SIC:"),
    textInput("jobtask", "Job tasks:"),
    div(style="display:inline-block", actionButton("goButton", "Enter")),
    downloadButton("downloadData", "Download data"),
    actionButton("button", "Donate data")
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
       tabPanel("Map", leafletOutput("map")),
       tabPanel("Table", DTOutput("xy_Table"))
    )
  )
)



#server.R
server <- function(input, output) {

# create reactive table
  xyTable <- reactiveValues(
    table1 = tibble(Address = character(),
                    Latitude = numeric(),
                    Longitude = numeric(),
                    Type = character(),
                    `Date from` = character(), 
                    `Date to` = character(),
                    Comment = character())
  )

# base map
output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addScaleBar(position = c("bottomleft"))%>%
      addFullscreenControl() %>%
      setView(lng = -2, lat = 55, zoom = 6)
  })
  

# This is what happens evertime a new address is added
observeEvent(input$goButton, {
  # create the reactive map
  mapit<-leafletProxy("map")
  
    #geocode the address
    data<-BING(input$str)
  
    # update the table fields
    xyTable$table1 <- xyTable$table1 %>% 
      add_row(Address = as.character(input$str),
              Latitude = as.numeric(data[1]),
              Longitude = as.numeric(data[2]),
              Type = as.character(input$type),
              `Date from` = as.character(input$date3), 
              `Date to` = as.character(input$date4),
              Comment = as.character(input$comment))
    # create the spatial points file
    DT = data.frame(
      lat=as.numeric(data[1]),
      lng=as.numeric(data[2])
    )
    DT = st_as_sf(DT, coords = c("lng","lat"), remove = FALSE, crs = 4326)
    # check we have a spatial point - delete
    print(DT)
    
observe({
      # update the map
      # create the reactive map
      mapit %>%
        addMarkers(data=DT) 
    })
  },  ignoreNULL=T) 


  
  output$comment <- renderText({ input$comment })
  
  output$xy_Table <- renderDataTable({
    xyTable$table1
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Lifeplaces.csv", sep = "")
    },
    content = function(file) {
      write.csv(xyTable$table1, file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)