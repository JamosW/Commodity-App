library(tidyverse)
library(sf)
library(fuzzyjoin)



#take geometry from world sf object, get the X and Y Coordinates and append the columns
worldXY <- rnaturalearthdata::countries50 |>
  st_as_sf() |> 
  st_make_valid() |> 
  st_geometry() |>
  st_centroid() |>
  unlist() |>
  matrix(ncol = 2, byrow = T) |> 
  data.frame(name = rnaturalearthdata::countries50$name) |> 
  set_names("X", "Y", "name")

joined <- vroom::vroom("agricultural_commodities.csv") |> 
  fuzzy_left_join(worldXY , match_fun = list(str_detect), by = c("Area" = "name")) |> 
  filter.(Element %in% c("Area harvested", "Yield", "Production"))


mainplot <- function(data = joined, type = "Area harvested") {
    
    data <- data |> filter(Element == type)
  
  leaf <- leaflet(data = data) |> addTiles() |> addCircles(lng = ~X, lat = ~Y, radius = ~Y2020 * 0.1)
  
  return(leaf)
}

mapData <- function(data, countries = NULL, commodities = NULL, years = NULL){
  
  ndata <- data |> 
    select.(Area, Item, Element, years, X,Y)
  
  ndata$Item <- str_replace(joined$Item,c("\xe9","C\xf4"), c("e", "Co"))
  ndata$Area <- str_replace(joined$Area,c("\xe9","C\xf4"), c("e", "Co"))
  
  if(is.null(commodities) & !is.null(countries)) {
    ndata <- ndata |>
      filter.(Area %in% countries, Element == "Area harvested", Item %in% Item)
  } else if (is.null(countries) & !is.null(commodities)) {
    ndata <- ndata |> 
      filter.(Area %in% Area, Item %in% commodities, Element == "Area harvested")
  } else {
    ndata <- ndata |> 
      filter.(Area %in% countries, Item %in% commodities, Element == "Area harvested")
  }
    
  return(ndata)
    
}


