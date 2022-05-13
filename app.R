library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(stringr)
library(purrr)
library(shinyWidgets)
library(tidytable)
library(leaflet)

source("agri_data.R")

countries <- joined |> 
  filter.(Element == "Area harvested") |> 
  pull.(Area) |> 
  str_replace(c("C\xf4", "\xe9"), c("Co", "e")) |> 
  unique()

commodities <- joined |> 
  filter.(Element == "Area harvested") |> 
  pull.(Item) |> 
  str_replace(c("C\xf4", "\xe9"), c("Co", "e")) |> 
  unique()

body <- dashboardBody(
  
  fluidRow(
    column(8,
           tabBox(
             id = "display",
             width = 12,
             height = "400px",
             tabPanel("Map",
               leafletOutput("map")
             ),
             tabPanel("History",
                      plotOutput("history"))
           )),
    column(4,
           tabBox(
             id = "selection",
             width = 12,
             tabPanel("Commodity",
                      multiInput(
                        inputId = "commodities",
                        label = "Commodities :", 
                        choices = NULL,
                        choiceNames = map(seq_along(commodities), 
                                          function(x) commodities[x]),
                        choiceValues = commodities,
                        width = "100%"
                      )),
             tabPanel("Country",
                      multiInput(
                        inputId = "countries",
                        label = "Countries :", 
                        choices = NULL,
                        choiceNames = map(seq_along(countries), 
                                             function(x) countries[x]),
                        choiceValues = countries, 
                        width = "100%"
                      )),
           ),
           fluidRow(
             sliderInput("year",
                         label = "Year",
                         min = 1961,
                         max = 2020,
                         value = 2020,
                         animate = TRUE,
                         sep = "",
                         width = "410px")),
           fluidRow(
             verbatimTextOutput("text"))
           ) 

  )
)

ui <- dashboardPage(
    dashboardHeader(
      title = "Hello"
    ),
    dashboardSidebar(),
    body
)

server <- function(input, output, session) {
  
  markers <- reactive(

    mapData(data = joined, 
            countries = input$countries, 
            commodities = input$commodities, 
            years  = paste0("Y",input$year))
    )
    
  output$map <- renderLeaflet({
    
    data <- markers() |> drop_na.()
    
    #Only draw if map tab is in focus
    if(input$display == "Map") {
    
    m <- leaflet(data= data) |> addTiles()
    
    if(length(input$countries) < 20 | length(input$commodities) < 20) {
      m |> addCircleMarkers(data |> 
                              unique(), 
                            radius = ~ (data[[paste0("Y", input$years)]] / max(data[[paste0("Y", input$years)]])) * 10,
                            lng = ~ X, lat = ~ Y,
                            stroke = FALSE, 
                            fillOpacity = 0.6)
    } else { m }
    }
  }) |> bindCache(input$commodities, input$countries)

  #observe input$commodities is not null, update input when value selected
  observe({
    
    area <- isolate(countries[
      countries %in%  
        mapData(data = joined, commodities = input$commodities, years  = paste0("Y",input$year))$Area
      ])
    
    if(!is.null(input$commodities)) {
      updateMultiInput(session = session, 
                       inputId = "countries", 
                       choices = area,
                       selected = input$countries)
                       
    } 
    
  #observe input$commodities is not null, update input when value selected  
  }) |> bindEvent(input$countries)
  
  observe({
    
    item <- isolate(commodities[
      commodities %in%  
        mapData(data = joined, countries = input$countries, years  = paste0("Y",input$year))$Item
    ]) 
    
    if(!is.null(input$countries)) {
      updateMultiInput(session = session, 
                       inputId = "commodities", 
                       choices = item,
                       selected = input$commodities)
      
    } 
    
  }) |> bindEvent(input$commodities)
  

  output$history <- renderPlot(
    
    #Only draw if history tab is in focus
    if(input$display == "History") {
    linePlot(markers())
    }
  ) |> bindCache(input$commodities, input$countries)
  
  output$text <- renderPrint({
     
    markers() |> drop_na.()
  }) 
}

shinyApp(ui, server)
