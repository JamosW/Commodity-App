library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(stringr)
library(purrr)
library(shinyWidgets)
library(tidytable)
library(leaflet)
library(htmltools)
source("agri_data.R")

countries <- joined |> 
  pull.(Area) |> 
  str_replace(c("C\xf4", "\xe9"), c("Co", "e")) |> 
  unique()

commodities <- joined |> 
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
                      plotOutput("history")
                      ),
             tabPanel("Treemap",
                      plotOutput("treemap"))
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
                        choiceNames = map_chr.(seq_along(commodities), 
                                          function(x) commodities[x]) |> sort(),
                        choiceValues = commodities,
                        width = "100%"
                      )),
             tabPanel("Country",
                      multiInput(
                        inputId = "countries",
                        label = "Countries :", 
                        choices = NULL,
                        choiceNames = map_chr.(seq_along(countries), 
                                               function(x) countries[x]) |> sort(),
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

    )),
  fluidRow(
    column(12,
          box(id = "Financial",
              width = 12,
              height = "10%",
              collapsed = TRUE,
              collapsible = TRUE,
              tabPanel("Time Series",
                        plotOutput("ts",
                                         
                           )
                           
                  ))),
    fluidRow(
      verbatimTextOutput("text")
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
                            popup = ~ data |> pull.(Area),
                            label = ~ htmlEscape(Area),
                            stroke = FALSE, 
                            fillOpacity = 0.6)
    } else { m }
    }
  }) |> bindCache(input$commodities, input$countries)

  #observe input$commodities is not null, update input when value selected
  observe({
    
    all_areas <- cleaned()$area |> unique()  |> sort()
    
    available_areas <- mapData(data = joined,
                               commodities = input$commodities, 
                               years  = paste0("Y",input$year)) |> 
      drop_na.()
    
    selected_areas <- countries[countries %in% (available_areas)$Area]
    
    isolate(if(!is.null(input$commodities)) {
      updateMultiInput(session = session, 
                       inputId = "countries", 
                       choices = selected_areas |>  sort(),
                       selected = input$countries)
                       
    } else {
      updateMultiInput(session = session, 
                       inputId = "countries", 
                       choices = all_areas,
                       selected = input$countries)
    })
    
  #observe input$commodities is not null, update input when value selected  
  })
  
  observe({
    
    all_items <- cleaned()$item |> unique() |> sort()
    
    available_items <- mapData(data = joined,
                               countries = input$countries, years  = paste0("Y",input$year)) |> 
      drop_na.()
    
    selected_items <- commodities[commodities %in% (available_items)$Item] 
    
    isolate(if(!is.null(input$countries)) {
      updateMultiInput(session = session, 
                       inputId = "commodities", 
                       choices = selected_items |>  sort(),
                       selected = input$commodities)
      
    } else {
      updateMultiInput(session = session, 
                       inputId = "commodities", 
                       choices = all_items,
                       selected = input$commodities)
    })
    
  })
  

  output$history <- renderPlot(
    
    #Only draw if history tab is in focus
    if(input$display == "History") {
    linePlot(markers(), year = input$year)
    }
  ) |> bindCache(input$commodities, input$countries, input$year)
  
  output$treemap <- renderPlot(
    
    #Only draw if Treemap tab is in focus
    if(input$display == "Treemap") {
      
      treemp(markers(), year = input$year)
    }
  ) |> bindCache(input$commodities, input$countries, input$year)
  
  
  output$ts <- renderPlot(
    
    if(any(str_detect(input$commodities, names(commods)))) {
      
      
      #s is the input value needed
      plot_ts(
        tsData = ts_data(type = commods[str_detect(input$commodities, names(commods))]),
        name = names(commods)[str_detect(input$commodities, names(commods))]
        )}

  )
  
  output$text <- renderPrint({
    label = ~ htmlEscape(joined["Area"])
    
  }) 
}

shinyApp(ui, server)
