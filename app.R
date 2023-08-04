library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(collapse)
library(ggplot2)
library(stringr)
library(echarts4r)
source("agri_data.R")
source("leaflet_logic.R")

#joined data read in from agri_data file
countries <- funique(joined$Area)

subset <- funique(joined$Element)

commodities <- funique(sbt(joined, Element == "Area harvested")$Item)

body <- shinydashboard::dashboardBody(
  
  fluidRow(
    column(8,
           tabBox(
             id = "display",
             width = 12,
             height = "800px",
             tabPanel("Map",
               leafletOutput("map", height = "600px")
               ),
             #possibly create modules for repetition
             tabPanel("History",
                      withSpinner(plotOutput("history", height = "600px"))
             ),
             tabPanel("Treemap",
                      withSpinner(plotOutput("treemap", height = "600px"))
             ),
             tabPanel("Percentages",
                      withSpinner(plotOutput("bargraph", height = "600px"))
             ),
             tabPanel("Interactive area Chart",
                      withSpinner(echarts4rOutput("areachart", height = "600px"))
             )
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
                        choiceNames = sapply(seq_along(commodities), 
                                          function(x) commodities[x] |>  sort()),
                        choiceValues = commodities,
                        width = "100%"
                      )),
             tabPanel("Country",
                      multiInput(
                        inputId = "countries",
                        label = "Countries :", 
                        choices = NULL,
                        choiceNames = sapply(seq_along(countries), 
                                               function(x) countries[x] |> sort()),
                        choiceValues = countries, 
                        width = "100%"
                      )),
             tabPanel("Subset",
                      awesomeRadio(
                        inputId = "subset",
                        label = "Element subset:", 
                        choices = subset,
                        selected = "Area harvested",
                        status = "warning"
                      ))
             
           ),
           fluidRow(
             sliderInput("year",
                         label = "Year",
                         min = 1961,
                         max = 2020,
                         value = 2020,
                         animate = TRUE,
                         sep = "",
                         width = "410px"),
             verbatimTextOutput(outputId = "text")
             )

    )),
  br(), br(), br(), br(),br(),br(),br(), br(), br(),br(),br(),br(),
  fluidRow(
    column(5, offset = 0.99,
    # textInput("dataSource", label = NULL, "Data Source: Food and Agricultural Organization", width = "350px")
    )
  ))

ui <- dashboardPage(
    dashboardHeader(
      title = "Agricultural Commodities",
      titleWidth = "400px"
    ),
    dashboardSidebar(disable = TRUE),
    body
)

server <- function(input, output, session) {
  
  
  markers <- reactive(

    mapData(data = joined, 
            countries = input$countries, 
            commodities = input$commodities, 
            years  = paste0("Y",input$year),
            .subset = input$subset) |> 
      na_omit()
    )
  
  values <- substitute(sapply(markers()$Area, \(x) 
                   sum((markers()[[paste0("Y",input$year)]])[markers()$Area %in% x], na.rm = TRUE)))
  
  output$map <- renderLeaflet({
    
    #Only draw if map tab is in focus
    if(input$display == "Map") {
      
      pal <- colorBin(c("viridis"), eval(values), bins = 7)
    
    m <- leaflet(data= markers()) |>
      addTiles(options = tileOptions(minZoom = 1, maxZoom = 5))
    
    if(between(length(input$countries),1, 20) || between(length(input$commodities), 1,20)) {
      
      if(length(funique(markers()$Item)) == 1) {
        m |> leaf_extra(tit = paste(funique(markers()$Item), "(ha)"), dat = markers(), val = values, pall = pal)
      } else {
        m |> leaf_extra(tit = "Area Harvested (ha)", dat = markers(), val = values, pall = pal)
      }
      
    } else { m }

    }
    
  }) |> bindCache(input$commodities, input$countries, input$year, input$subset)

  #observe input$commodities is not null, update input when value selected
  observe({
    
    year = input$year
    
    available_areas <- funique(markers()$Area)
    available_items <- funique(markers()$Item)
    
    selected_areas <- sbt(joined, Item %in% available_items & Element == input$subset)$Area |> sort() |> funique()
    selected_items <- sbt(joined, Area %in% available_areas & Element == input$subset)$Item |> sort() |> funique()
    
    

    
    if(!is.null(input$commodities)) {
      updateMultiInput(session = session, 
                       inputId = "countries", 
                       choices = selected_areas,
                       selected = input$countries)
      print(available_items)
      
    } else {
      updateMultiInput(session = session, 
                       inputId = "countries",
                       choices = countries,
                       selected = input$countries)
      
    }
    
    
    if(!is.null(input$countries)) {
      updateMultiInput(session = session, 
                       inputId = "commodities", 
                       choices = selected_items,
                       selected = input$commodities)
    } else {
      updateMultiInput(session = session, 
                       inputId = "commodities", 
                       choices = commodities,
                       selected = input$commodities)
      
    }

  }) |> 
    bindEvent(input$commodities, input$countries, input$year, input$subset)
  

  output$history <- renderPlot(
    
    #Only draw if history tab is in focus
    if(input$display == "History") {
    linePlot(markers(), .subset = input$subset, year = input$year, country = input$countries, commodity = input$commodities)
    }
  ) |> bindCache(input$commodities, input$countries, input$year, input$subset)
  
  output$treemap <- renderPlot(
    
    #Only draw if Treemap tab is in focus
    if(input$display == "Treemap") {
      
      treemp(markers(), year = input$year, .subset = input$subset)
    }
  ) |> bindCache(input$commodities, input$countries, input$year, input$subset)
  
  output$bargraph <- renderPlot(

    #Only draw if bargraph tab is in focus
    if(input$display == "Percentages") {
      barPlot(markers())
    }
  ) |> bindCache(input$commodities, input$countries, input$year, input$subset)
  
  output$areachart <- renderEcharts4r(
    
    #Only draw if areachart tab is in focus
    if(input$display == "Interactive area Chart") {
      areaPlot(df = joined, country = input$countries, commodity = input$commodities, subset = input$subset, year = input$year)
    }
  ) |> bindCache(input$commodities, input$countries, input$year, input$subset)
  
  
  output$source = renderText({ input$dataSource })
    
  # output$text = renderPrint({
  #   
  #   sbt(joined, commodities %in% funique(markers()$Item) & Element == input$subset)$Area |> funique()
  # 
  # 
  # })
  
  
  
}


shinyApp(ui, server)


# CO2 |>
#   group_by(Plant) |>
#   e_charts(conc) |>
#   e_area(uptake) |>
#   e_tooltip(trigger = "axis")
# 
# CO2
