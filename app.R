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

commodities <- funique(sbt(joined, Element == "Area harvested")$Item)

subsets <- funique(joined$Element)

body <- shinydashboard::dashboardBody(
  
  fluidRow(
    column(8,
           tabBox(
             id = "display",
             width = 12,
             height = "800px",
             tabPanel("Map",
               leafletOutput("map", height = 800)
               ),
             tabPanel("History",
                      withSpinner(plotOutput("history", height = "800px"))
             ),
             tabPanel("Treemap",
                      withSpinner(plotOutput("treemap", height = "800px"))
             ),
             tabPanel("Percentages",
                      withSpinner(plotOutput("bargraph", height = "800px"))
             ),
             tabPanel("Interactive area Chart",
                      withSpinner(girafeOutput("areachart", height = "800px"))
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
                        choices = subsets,
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
             verbatimTextOutput(outputId = "text"))

    )),
  br(), br(), br(), br(),br(),br(),br(), br(), br(),br(),br(),br(),
  fluidRow(
    column(5, offset = 0.99,
    textInput("dataSource", label = NULL, "Data Source: Food and Agricultural Organization", width = "350px"))
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
    
    all_areas <- joined$area |> funique()  |> sort()
    
    available_areas <- mapData(data = joined,
                               commodities = input$commodities, 
                               years  = paste0("Y",year),
                               .subset = input$subset) |> 
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
    
    
  })
  
  #observe input$commodities is not null, update input when value selected
  
  observe({
    
    year = input$year
    
    all_items <- joined$item |> funique() |> sort()
    
    available_items <- mapData(data = joined,
                               countries = input$countries, 
                               years  = paste0("Y", year),
                               .subset = input$subset) |> 
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
  
  output$areachart <- renderGirafe(
    
    #Only draw if areachart tab is in focus
    if(input$display == "Interactive area Chart") {
      areaPlot(markers(), country = input$countries, commodity = input$commodities, subset = input$subset, year = input$year)
    }
  ) |> bindCache(input$commodities, input$countries, input$year, input$subset)
  
  
  output$source = renderText({ input$dataSource })
    
  output$text = renderPrint({
    
    values = limiter(joined, areaLimit = 7, itemLimit = 7, fillPos = "bottom")
    
    areaToUse = values[[1]]
    showFill = values[[2]]

    years <- as.numeric(na_omit(str_extract(names(joined), "1.*|2.*")))
    
    my_df = pivoted_data(mapData(data = joined, 
                                 years = str_c("Y",years[years <= as.numeric(str_replace(input$year, "Y", ""))]),
                                 .subset = input$subset,
                                 countries = input$countries, 
                                 commodities = input$commodities)) |>
      sbt(Area %in% joined$Area & Item %in% joined$Item) |>
      gby(eval(areaToUse), Year) |>
      smr(value = fsum(value)) |> 
      fungroup()
    
    valueChoice <- function(df, choice){
      value = df|> gby(areaToUse) |> 
        mtt(value = eval(call(choice, value)))
      
      return(value)
    }
    
    base <-ggplot(my_df, aes(x=Year, fill = areaToUse))
    
    
    
    interactive <- base + 
      geom_area_interactive(aes(y = value), alpha=0.6 , size=.5, colour="white", tooltip = "hey")
    
    interactive
    
  })
  
  
  
}


shinyApp(ui, server)



CO2 |>
  group_by(Plant) |>
  e_charts(conc) |>
  e_area(uptake) |>
  e_tooltip(trigger = "axis")

