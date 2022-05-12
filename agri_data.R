library(sf)
library(fuzzyjoin)


cleaned <- function(x) {
  item <- str_replace(joined$Item,c("\xe9","C\xf4"), c("e", "Co"))
  area <- str_replace(joined$Area,c("\xe9","C\xf4"), c("e", "Co"))
  
  l <- list(item, area) |> purrr::set_names(c("item", "area"))
  
  return(l)
}


#take geometry from world sf object, get the X and Y Coordinates and append the columns
worldXY <- rnaturalearthdata::countries50 |>
  st_as_sf() |> 
  st_make_valid() |> 
  st_geometry() |>
  st_centroid() |>
  unlist() |>
  matrix(ncol = 2, byrow = T) |> 
  data.frame(name = rnaturalearthdata::countries50$name) |> 
  purrr::set_names("X", "Y", "name")

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
    select(Area, Item, Element, years, X,Y)
  
  ndata$Item <- cleaned()$item
  ndata$Area <- cleaned()$area

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



#######################################################Plot Data ##########################################################


linePlot <- function(data) {
  
  #lineplot
  lp <- function(x, group) 
  {
    ggplot(x) + 
      geom_smooth(aes(x = name, y = value, group = group, color = group), size = 1, na.rm = TRUE, se = FALSE) +
      scale_y_log10() + labs(x = "Year",
                             y = "Production",
                             color = "Commodity") + theme_bw()
    }
  
  #only pivot columns that are years
  pivot <- function(x)
  {
    select(x, Area, Item, Element, contains("Y")) |>  
      pivot_longer.(contains("Y")) |> 
      filter.(value != "A") |> 
      mutate.(value = as.numeric(value), name = str_remove(name, c("Y")) |> as.numeric())
    }
  
  
  if(all(data$Area == "World")) {
    p <- joined |> 
      filter.(Area %in% c("Africa", "Europe", "Asia", "Americas", "Oceania"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area)
  } else if(all(data$Area == "Africa")) {
   p <-  joined |> 
      filter.(Area %in% c("Northern Africa", "Southern Africa", "Western Africa"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area)
  } else if(all(data$Area == "Asia")) {
    p <-  joined |> 
      filter.(Area %in% c("Central Asia", "Southern Asia", "Western Asia"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area)
  } else if(all(data$Area == "Americas")) {
    p <- joined |> 
      filter.(Area %in% c("Northern America", "Central America", "South America"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area)
  } else if (all(data$Area == "Europe")) {
    p <- joined |> 
      filter.(Area %in% c("Northern Europe", "Southern Europe", "Western Europe", "Eastern Europe"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area)
    #only when on area is selected and less than 10 items, more than 10 is too much clutter
  } else if((data$Item |> unique() |> length()) <= 10 & (data$Area |> unique() |> length()) == 1){
    p <- joined |> 
      filter.(Area %in% Area, Element == "Area harvested", Item %in% data$Item) |>
      pivot() %>% 
      lp(.$Item)
  } else if( (data$Item |> unique() |> length()) <= 10 & between.((data$Area |> unique() |> length()), 2, 10) ){
    p <- joined |> 
      filter.(Area %in% data$Area, Element == "Area harvested", Item %in% data$Item) |>
      pivot() %>% 
      lp(.$Item) + facet_wrap(vars(Area))
  }
    
   return(p)
}



