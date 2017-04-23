######################################################################################################
# Initialization                                                                                     #
######################################################################################################

# Turn off scientific notation -----------------------------------------------------------------------
options(scipen = 999)

# Packages -------------------------------------------------------------------------------------------
library(DT)
library(foreach)
library(jsonlite)
library(leaflet)
library(lubridate)
library(plotly)
library(reshape2)
library(shiny)
library(shinyBS)
library(stringi)

######################################################################################################
# Shiny                                                                                              #
######################################################################################################

shinyUI(
    navbarPage(title = "Near Earth Object (NEO) and Fireball Tracking", theme = "bootstrap.css", collapsible = TRUE,
               tabPanel("Near Earth Object (NEO)",
                        fluidRow(
                            column(2, radioButtons("diam_velo_unit", label = strong("Show diameter / velocity in"),
                                                   choices = list("Meters / Kilometers" = "5,6,11",
                                                                  "Feet / Miles" = "7,8,12"),
                                                   selected = "5,6,11"),
                                   br(),
                                   radioButtons("miss_dist_unit", label = strong("Show miss distance in"),
                                                choices = list("Kilometers" = "15",
                                                               "Miles" = "16",
                                                               "Lunar" = "14",
                                                               "Astronomical" = "13"))),
                            column(10, plotlyOutput("neo_plot", height = "300px"))
                        ),
                        
                        fluidRow(
                            br(),
                            DT::dataTableOutput("neo_table")
                        )
               ),
               
               tabPanel("Fireball",
                        tags$style(type = "text/css", "#Map {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("Map"),
                        absolutePanel(top = 82, left = 69, 
                                      actionButton("sat_button", "Satellite Image", icon = icon("search")),
                                      strong(htmlOutput("sat_error")))
               ),
               
               tabPanel("Datasets",
                        strong("Download raw datasets (csv format)"),
                        br(),
                        br(),
                        downloadButton("neo_data.csv", "Near Earth Object"),
                        downloadButton("fireball_data.csv", "Fireball")
               ),
               
               tabPanel("About", align = "center",
                        strong("This application is using data from:"),
                        br(),
                        br(),
                        tags$a(href="https://open.nasa.gov/", target="_blank", 
                               img(src = "OpenNASA.jpg", width = "600px", height = "222px")),
                        br(),
                        br(),
                        tags$a(href="https://api.nasa.gov/api.html#NeoWS", target="_blank", "NeoWs (Near Earth Object Web Service)"),
                        br(),
                        tags$a(href="https://api.nasa.gov/api.html#Fireball", target="_blank", "SSD/CNEOS (Fireball)"),
                        br(),
                        tags$a(href="https://api.nasa.gov/api.html#earth", target="_blank", "Landsat 8 image"),
                        hr(),
                        strong("The source code can be found on:"),
                        br(),
                        br(),
                        tags$a(href="https://github.com/kvistrup/neo_fireball_tracking/", target="_blank",
                               img(src = "github.png", width = "221px", height = "96px"))
               )
    )
)


