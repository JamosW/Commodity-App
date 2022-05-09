library(shiny)
library(shinydashboard)
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
             width = 12,
             height = "400px",
             tabPanel("Map",
               leafletOutput("map")
             ),
             tabPanel("org")
           )),
    column(4,
           tabBox(
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
                         width = "410px"))
           ), 

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
    m <- leaflet(data= markers()) |> addTiles()
    
    if(length(input$countries) < 20 | length(input$commodities) < 20) {
      m |> addCircleMarkers(markers() |>
                              unique(), 
                            lng = ~ X, lat = ~ Y, 
                            radius = ~ (markers()[[paste0("Y", input$year)]] * 0.0001))
    } else { m }
  })
  
  #observe input$commodities is not null, update input when value selected
  observe({
    
    area <- countries[
      countries %in%  
        mapData(data = joined, commodities = input$commodities, years  = paste0("Y",input$year))$Area
      ]
    
    if(!is.null(input$commodities)) {
      updateMultiInput(session = session, 
                       inputId = "countries", 
                       choices = area |> unique(),
                       selected = input$countries)
                       
    } 
    
  #observe input$commodities is not null, update input when value selected  
  }) |> bindEvent(input$commodities)
  
  observe({
    
    item <- commodities[commodities %in% markers()$Item]
    
    if(!is.null(input$countries)) {
      updateMultiInput(session = session, 
                       inputId = "commoditites", 
                       choices = item |> unique(),
                       selected = input$commodities)
      
    } 
    
  }) |> bindEvent(input$countries)
  
  output$text <- renderPrint({
    markers()
  })

}

shinyApp(ui, server)



bench::mark(vroom::vroom("agricultural_commodities.csv"),
            read_csv("agricultural_commodities.csv"),
            data.table::fread("agricultural_commodities.csv"))


