# Packages
library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
library(DT)


#Fluid page setup 
shinyUI(fluidPage(
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
      column(11, h1("Lifeplaces",style = "font-family: 'Roboto Slab', cursive;font-weight: bold; font-size: 39px"))),
    windowTitle = "Lifeplaces"),

# Sidebar panel
sidebarPanel( 
    strong("Description"),
    helpText(""),
    div(style="display:inline-block", textInput("str", label =("Enter a previous address"), value = "")),
    dateInput("date3", "Date from:", value = Sys.Date(), format = "mm/dd/yy"),
    dateInput("date4", "Date to:", value = Sys.Date(), format = "mm/dd/yy"),
    div(style="display:inline-block", actionButton("goButton", "Enter"))),

# Main Panel
mainPanel(
  tabsetPanel(type = "tabs",
  tabPanel("Table", tableOutput("table")),
  tabPanel("Map", leafletOutput("map")))
)
))