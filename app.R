library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(htmltools)
source("agri_data.R")


countries <- funique(joined$Area)

commodities <- funique(joined$Item)

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
                      plotOutput("treemap")
                      ),
             tabPanel("Bargraph",
                      plotOutput("bargraph")
             ),
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
      title = "Commodities"
    ),
    dashboardSidebar(disable = TRUE),
    body
)

server <- function(input, output, session) {
  
  markers <- reactive(

    mapData(data = joined, 
            countries = input$countries, 
            commodities = input$commodities, 
            years  = paste0("Y",input$year)) |> 
      na_omit()
    )
  
  
  
    
  output$map <- renderLeaflet({
    
    pal <- colorBin(c("viridis"), markers()[[paste0("Y",input$year)]], bins = 7)
    
    #Only draw if map tab is in focus
    if(input$display == "Map") {
    
    m <- leaflet(data= markers()) |>
      addTiles(options = tileOptions(minZoom = 1, maxZoom = 5))
    
    leaf_extra <- function(m, tit, dat) {
      #function for map, to reduce repitition
      newM <- m |> addCircleMarkers(dat,
                                 radius = 6,
                                 lng = ~ X, lat = ~ Y,
                                 label = ~ htmlEscape(paste(Area, dat[[paste0("Y",input$year)]])),
                                 color = ~ pal(dat[[paste0("Y",input$year)]]),
                                 stroke = FALSE, 
                                 fillOpacity = 0.6) |>
        leaflet::addLegend(pal = pal,
                           values = ~ dat[[paste0("Y",input$year)]],
                           title = tit)
      
      return(newM)
    }
    
    if(between.(length(input$countries),1, 20) || between.(length(input$commodities), 1,20)) {
      
      if(length(funique(markers()$Item)) == 1) {
        m |> leaf_extra(tit = paste(funique(markers()$Item), "(ha)"), dat = markers())
      } else {
        m |> leaf_extra(tit = "Area Harvested (ha)", dat = markers())
      }
      
    } else { m }
    }
  }) |> bindCache(input$commodities, input$countries, input$year)
  

  #observe input$commodities is not null, update input when value selected
  observe({
    
    years = input$year
    
    all_areas <- joined$area |> funique()  |> sort()
    
    available_areas <- mapData(data = joined,
                               commodities = input$commodities, 
                               years  = paste0("Y",years)) |> 
      na_omit()
    
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
    
    years = input$year
    
    all_items <- joined$item |> funique() |> sort()
    
    available_items <- mapData(data = joined,
                               countries = input$countries, years  = paste0("Y", years)) |> 
      na_omit()
    
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
  
  output$bargraph <- renderPlot(
    
    #Only draw if bargraph tab is in focus
    if(input$display == "Bargraph") {
      barPlot(markers(), year = input$year)
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
}


shinyApp(ui, server)

