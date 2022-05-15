library(sf)
library(fuzzyjoin)



load_and_join <- function(x){
  
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

#join XY data to data frame that is used for mapping
joined <- vroom::vroom("agricultural_commodities.csv") |> 
  fuzzy_left_join(worldXY , match_fun = list(str_detect), by = c("Area" = "name")) |> 
  filter.(Element %in% c("Area harvested", "Yield", "Production"))

#find the rows that are unneeded
unmatched <- joined |> filter.(is.na(X), !Area %in% c("Africa", "Asia", "Europe", "Americas", "Oceania"))

joined <- joined |> filter.(!Area %in% unmatched$Area)

return(joined)

}

joined <- load_and_join() |> filter.(Element %in% "Area harvested")


cleaned <- function(x) {
  item <- str_replace(joined$Item,c("\xe9","C\xf4"), c("e", "Co"))
  area <- str_replace(joined$Area,c("\xe9","C\xf4"), c("e", "Co"))
  
  l <- list(item, area) |> purrr::set_names(c("item", "area"))
  
  return(l)
}

mainplot <- function(data = joined, type = "Area harvested") {
    
    data <- data |> filter(Element == type)
  
  leaf <- leaflet(data = data) |> addTiles() |> addCircles(lng = ~X, lat = ~Y, radius = ~Y2020 * 0.1)
  
  return(leaf)
}

mapData <- function(data, countries = NULL, commodities = NULL, years = NULL){
  
  ndata <- data |> 
    select.(Area, Item, Element, years, X,Y)
  
  ndata$Item <- cleaned()$item
  ndata$Area <- cleaned()$area

  if(is.null(commodities) && !is.null(countries)) {
    ndata <- ndata |>
      filter.(Area %in% countries, Element == "Area harvested", Item %in% Item)
  } else if (is.null(countries) && !is.null(commodities)) {
    ndata <- ndata |> 
      filter.(Area %in% Area, Item %in% commodities, Element == "Area harvested")
  } else {
    ndata <- ndata |> 
      filter.(Area %in% countries, Item %in% commodities, Element == "Area harvested")
  }
    
  return(ndata)
    
}



##################################################Line Plot Data ##########################################################


linePlot <- function(data, year = 2020) {
  
  #lineplot
  lp <- function(x, group, clr) 
  {
    ggplot(x) + 
      geom_smooth(aes(x = name, y = value, group = group, color = group), size = 1, na.rm = TRUE, se = FALSE) +
      scale_y_log10() + labs(x = "Year",
                             y = "Production",
                             color = clr) + theme_bw()
    }
  
  #only pivot columns that are years
  pivot <- function(x)
  {
    select.(x, Area, Item, Element, paste0("Y",1961:year)) |>  
      pivot_longer.(contains("Y")) |> 
      filter.(value != "A") |> 
      mutate.(value = as.numeric(value), name = str_remove(name, c("Y")) |> as.numeric())
    }
  
  
  if(all(data$Area == "World")) {
    p <- joined |> 
      filter.(Area %in% c("Africa", "Europe", "Asia", "Americas", "Oceania"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area, data$Area)
  } else if(all(data$Area == "Africa")) {
   p <-  joined |> 
      filter.(Area %in% c("Northern Africa", "Southern Africa", "Western Africa"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area, data$Area )
  } else if(all(data$Area == "Asia")) {
    p <-  joined |> 
      filter.(Area %in% c("Central Asia", "Southern Asia", "Western Asia"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area, data$Area)
  } else if(all(data$Area == "Americas")) {
    p <- joined |> 
      filter.(Area %in% c("Northern America", "Central America", "South America"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area, data$Area)
  } else if (all(data$Area == "Europe")) {
    p <- joined |> 
      filter.(Area %in% c("Northern Europe", "Southern Europe", "Western Europe", "Eastern Europe"), 
              Element == "Area harvested") |>
      pivot() %>% 
      lp(.$Area, data$Area)
    #only when on area is selected and less than 10 items, more than 10 is too much clutter
  } else if( between.((data$Item |> unique() |> length()), 1,12) && between.((data$Area |> unique() |> length()), 1, 12)){
    p <- joined |> 
      filter.(Area %in% data$Area, Element == "Area harvested", Item %in% data$Item) |>
      pivot() %>% 
      lp(.$Item, "Commodity") + facet_wrap(vars(Area))
  } else if((data$Item |> unique() |> length()) > 12 && (data$Area |> unique() |> length()) <= 12){
    p <- joined |> 
      filter.(Area %in% data$Area, Element == "Area harvested", Item %in% data$Item) |>
      pivot() %>% 
      lp(.$Area, "Country") + facet_wrap(vars(Area))
  } else if( between.((data$Item |> unique() |> length()), 1,12) && (data$Area |> unique() |> length()) > 12 ){
    p <- joined |> 
      filter.(Area %in% data$Area, Element == "Area harvested", Item %in% data$Item) |>
      pivot() %>% 
      lp(.$Item, "Commodity") + facet_wrap(vars(Item))
  }
    
   return(p)
}

####################################################Tree Map###############################################################
library(treemapify)
#library(ggplot2)

treemp <-function(df, year) {
  
  df <- df |> drop_na.()
  
  #base treemap, adjusted based on codition
  basemap <- function(df, year, label, lyout = "squarified") 
    {
    ggplot(data = df, aes(area = df[[paste0("Y",year)]], fill = Area, label = df[[label]], subgroup = Area)) + 
      geom_treemap(na.rm = TRUE, start = "topleft", layout = lyout) +
      scale_fill_hue(c = 80, h = c(0,285)) +
      geom_treemap_subgroup_text(place = "centre",alpha = 0.4, start = "topleft", layout = lyout)
    }

  
  if(length(df$Item |> unique())  ==  1 && length(df$Area) |> unique() < 10) {
    bm <- basemap(df, year, "Item")
  } else if (length(df$Item |> unique())  >  1 && length(df$Area) |> unique() < 10) {
    bm <- basemap(df, year, label = "Item")  + 
      geom_treemap_text(place = "top", start = "topleft", layout = "squarified")
  } else if (length(df$Item |> unique())  >  1 && length(df$Area) |> unique() > 10) {
    bm <- basemap(df, year, "Item") +
      theme(legend.position = "none")  + 
      geom_treemap_text(place = "top", start = "topleft", layout = "squarified")
  } else if (length(df$Item |> unique())  ==  1 && length(df$Area) |> unique() > 10) {
    bm <- basemap(df, year, "Item") +
      theme(legend.position = "none") 
  } else {
    bm <- basemap(df, year, "Item")
  }

  return(bm)
}


#------------------------------------------------------Time Series---------------------------------------------------------
library(xts)
#load data

ts_data <- function(type, years = NULL) {
  
  #load raw external data
  rawd <- vroom::vroom(paste0("https://query1.finance.yahoo.com/v7/finance/download/", type, "?period1=1305331200&period2=1652486400&interval=1d&events=history&includeAdjustedClose=true"))
  
  #convert to xts, order by date, remove the date
  xts::xts(rawd[-1], order.by = rawd$Date) 
}

#commoditites
commods <- list("Sugar" = "SB=F", "Cotton" = "CT=F", "Cocoa" = "CC=F", "Soybean" = "ZS=F", "Oat" = "ZO=F",
                     "Corn" = "ZO=F", "Coffee" = "KC=F")
s <- "Coffee, green"

#takes the time series data and it's name to be plotted
plot_ts <- function(tsData, name) {
  
  ggplot(data = tsData) + geom_line(aes(x = tsData |> index(), y = `Adj Close` )) +
    labs(x = "Year",
         y = "Adjusted Closing Price",
         title = paste(name, "prices from 2010 to current date (USD)"))
}


