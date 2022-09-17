library(sf)
library(collapse)
library(stringr)
library(tidytable)

#join XY data to data frame that is used for mapping
joined <- data.table::fread("agricultural_commodities.csv")
countriesData <- rnaturalearthdata::countries50

#take geometry from world sf object, get the X and Y Coordinates and append the columns
worldXY<-  countriesData |>
    st_as_sf() |>
    st_geometry() |>
    st_make_valid() |>
    st_centroid() |>
    unlist() |>
    matrix(ncol = 2, byrow = T) |>
    as.data.frame() |>
    setNames(c("X", "Y")) |>
    cbind(name = countriesData$name, economy = countriesData$economy)

  joined <- joined |>
  fuzzyjoin::fuzzy_left_join(worldXY , match_fun = list(stringr::str_detect), by = c("Area" = "name")) %>%
    ss(.$Element %in% c("Area harvested") & 
         !.$Area %in% c("Africa", "Asia", "Europe", "Americas", "Oceania") & 
         !is.na(.$Area),
        !str_detect(names(.), "F$|\\sCode|Unit|name"))

joined[,c("Item", "Area") ] <- mapply(\(x,y,z) str_replace_all(x, y, z),list(joined$Item, joined$Area),c("\xe9","C\xf4"),c("e", "Co"))

mapData <- function(data, countries = NULL, commodities = NULL, years = NULL){
  
  cols <- c("Area", "Item", "Element", "X", "Y", years)
  
  if(is.null(commodities) && !is.null(countries)) {
    ndata <- data %>%
      ss(data$Area %in% countries, 
         colnames(data) %in% cols)
  } else if (is.null(countries) && !is.null(commodities)) {
    ndata <- data %>%
      ss(data$Item %in% commodities, 
         colnames(data) %in% cols)
  } else {
    ndata <- data %>%
      ss(data$Area %in% countries & data$Item %in% commodities, 
         colnames(data) %in% cols)
  }
  
  return(ndata)
  
}


  pivoted <- joined |>  
  pivot_longer.(cols = na.omit(str_extract(names(joined), "Y\\d+")), 
                      names_to = "Year", values_to = "value", fast_pivot = TRUE) |> 
  mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  if_else.(value == 0 | is.na(value), 1, value))
  
  
  
##################################################Line Plot Data ##########################################################

linePlot <- function(data, year) {
  
  #lineplot
  lp <- function(x, group, clr) 
  {
    ggplot(x) + 
      geom_smooth(aes(x = Year, y = value, color = .data[[group]]), 
                  na.rm = TRUE, se = FALSE, size = 1) +
      scale_y_log10() + labs(x = "Year",
                             y = "Production",
                             color = clr) + theme_bw()
  }
  
  
  if(all(data$Area == "World")) {
    p <- pivoted |> 
      sbt(Area %in% c("Africa", "Europe", "Asia", "Americas", "Oceania") & !is.na(value)) %>%
      lp("Area", "Country")
  } else if(all(data$Area == "Africa")) {
   p <-  pivoted |> 
      sbt(Area %in% c("Northern Africa", "Southern Africa", "Western Africa") &
          !is.na(value)) %>%
      lp("Area", "Country" )
   
  } else if(all(data$Area == "Asia")) {
    p <-  pivoted|> 
      sbt(Area %in% c("Central Asia", "Southern Asia", "Western Asia") & !is.na(value)) %>%
      lp("Area", "Country")
  } else if(all(data$Area == "Americas")) {
    p <- pivoted |> 
      sbt(Area %in% c("Northern America", "Central America", "South America")
          & !is.na(value)) %>%
      lp("Area", "Country")
  } else if (all(data$Area == "Europe")) {
    p <- pivoted |> 
      sbt(Area %in% c("Northern Europe", "Southern Europe", "Western Europe", "Eastern Europe")
          & !is.na(value)) %>%
      lp("Area", "Country")
    
    #only when on area is selected and less than 10 items, more than 10 is too much clutter
  } else if(between.((data$Item |> funique() |> length()), 1,12) && 
             between.((data$Area |> funique() |> length()), 1, 12)){
    p <- pivoted |> 
      sbt(Area %in% data$Area & Item %in% data$Item  & !is.na(value) & Year <= year ) %>%
      lp("Item", "Commodity") + facet_wrap(vars(Area))
  } else if((data$Item |> funique() |> length()) > 12 && (data$Area |> funique() |> length()) <= 12){
    p <- pivoted |> 
      sbt(Area %in% data$Area & Item %in% data$Item  & !is.na(value) & Year <= year) %>%
      lp("Area", "Country") + facet_wrap(vars(Area))
  } else if(between.((data$Item |> funique() |> length()), 1,12) &&
             (data$Area |> funique() |> length()) > 12 ){
    p <- pivoted |> 
      sbt(Area %in% data$Area & Item %in% data$Item  & !is.na(value) & Year <= year) %>%
      lp("Item", "Commodity") + facet_wrap(vars(Item))
  }
    
   return(p)
}

####################################################Tree Map###############################################################
library(treemapify)
#library(ggplot2)

treemp <-function(df, year) {
  
  df <- df |> na_omit()
  
  #base treemap, adjusted based on codition
  basemap <- function(df, year, label, lyout = "squarified") 
    {
    ggplot(data = df, aes(area = .data[[paste0("Y",year)]], fill = Area, label = .data[[label]], subgroup = Area)) + 
      geom_treemap(na.rm = TRUE, start = "topleft", layout = lyout) +
      scale_fill_hue(c = 80, h = c(0,285)) +
      geom_treemap_subgroup_text(place = "centre",alpha = 0.4, start = "topleft", layout = lyout)
    }

  
  if(length(df$Item |> funique())  ==  1 && length(df$Area) |> funique() < 10) {
    bm <- basemap(df, year, "Item")
  } else if (length(df$Item |> funique())  >  1 && length(df$Area) |> funique() < 10) {
    bm <- basemap(df, year, label = "Item")  + 
      geom_treemap_text(place = "top", start = "topleft", layout = "squarified")
  } else if (length(df$Item |> funique())  >  1 && length(df$Area) |> funique() > 10) {
    bm <- basemap(df, year, "Item") +
      theme(legend.position = "none")  + 
      geom_treemap_text(place = "top", start = "topleft", layout = "squarified")
  } else if (length(df$Item |> funique())  ==  1 && length(df$Area) |> funique() > 10) {
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


#commoditites
commods <- list("Sugar" = "SB=F", "Cotton" = "CT=F", "Cocoa" = "CC=F", "Soybean" = "ZS=F", "Oat" = "ZO=F",
                "Corn" = "ZO=F", "Coffee" = "KC=F")

ts_data <- function(type, years = NULL) {
  
  #load raw external data
  rawd <- data.table::fread(paste0("https://query1.finance.yahoo.com/v7/finance/download/", type, "?period1=1305331200&period2=1652486400&interval=1d&events=history&includeAdjustedClose=true"))
  
  #convert to xts, order by date, remove the date
  xts::xts(rawd[-1], order.by = rawd$Date) 
}


#takes the time series data and it's name to be plotted
plot_ts <- function(tsData, name) {
  
  ggplot(data = tsData) + geom_line(aes(x = tsData |> index(), y = `Adj Close` )) +
    labs(x = "Year",
         y = "Adjusted Closing Price",
         title = paste(name, "prices from 2010 to current date (USD)"))
}

#-------------------------------------------------Bar_Graph------------------------------------------------
library(scales)
#ordered by region
barPlot <- function(data, year) {
    
    areaToUse = c()
    showFill = c()
    
    if(fndistinct(data$Area) > 20) {
      areaToUse = sym("economy")
    } else {
      areaToUse = sym("Area")
    }
    
    if(fndistinct(data$Item) > 16) {
      showFill = "none"
    } else {
      showFill = "right"
    }

  
  pivoted |> 
    sbt(Area %in% data$Area & Item %in% data$Item  & !is.na(value) & Year <= year) |> 
    ggplot( aes(x=eval(areaToUse), y=value)) +
    scale_y_sqrt(labels = label_number(scale_cut = cut_short_scale())) +
    geom_bar(stat="identity", alpha=.6, width=.4, aes(fill = Item)) +
    scale_colour_brewer(type = "div", palette = "Spectral") +
    coord_flip() +
    xlab("") +
    theme_bw() +
    theme(legend.position = showFill)
  
}
