library(data.table)
library(dplyr)

agri_data <- fread("cleanedFrame2.csv")

mapData <- function(data, countries = NULL, commodities = NULL, years, .subset){
  
  cols <- c("Area", "Item", "Element", "LON", "LAT", "economy", years)
  
  data <- data |> ss(, colnames(data) %in% cols)
  
  if(is.null(commodities) && !is.null(countries) && !is.null(.subset)) {
    ndata <- data %>%
      ss(data$Area %in% countries & data$Element %in% .subset)
  } else if (is.null(countries) && !is.null(commodities) && !is.null(.subset)) {
    ndata <- data %>%
      ss(data$Item %in% commodities & data$Element %in% .subset)
  } else {
    ndata <- data %>%
      ss(data$Area %in% countries & data$Item %in% commodities & data$Element %in% .subset)
  }
  
  return(ndata)
  
}

pivoted_data <- function(data) {
  
  pivoted <- data %>% 
    tidytable::pivot_longer(cols = na_omit(str_extract(names(.), "Y\\d+")), 
                    names_to = "Year", values_to = "value") |> 
      mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  ifelse(value == 0 | is.na(value), 0, value))
  
  return(pivoted)
  
}
  
  #limiter function for how many values can be displayed before using generalization
  limiter <-function(data, areaLimit, itemLimit, fillPos){
    
    areaToUse = c()
    showFill = c()

    #if more than 7 countries
    if(fndistinct(data$Area) > areaLimit) {
      areaToUse = parse(text = "economy")
    } else {
      areaToUse = parse(text = "Area")
    }
    
    if(fndistinct(data$Item) > itemLimit) {
      showFill = "none"
    } else {
      showFill = fillPos
    }
    
    return(list(areaToUse, showFill))
  }

##################################################Line Plot Data ##########################################################

linePlot <- function(data, .subset, year, country, commodity) {
  
  #lineplot 
  lp <- function(x, group = NULL, clr = NULL) 
  {
    
    tryCatch({plot <- ggplot(x) + 
      geom_smooth(aes(x = Year, y = value, col = "red"), 
                  na.rm = TRUE, se = FALSE, size = 1) +
      scale_y_continuous(labels = label_number(scale_cut = cut_long_scale())) + 
      labs(x = "Year",
           y = .subset,
           col = NULL) + theme_bw()},
      warning = function(e) e,
      finally = ""
    )
    
    return(plot)
  }
  
  years <- as.numeric(na_omit(str_extract(names(joined), "1.*|2.*")))
  
  subset <- pivoted_data(mapData(joined, countries = country,
                                 commodities = commodity, 
                                 years = str_c("Y",years[years <= as.numeric(str_replace(year, "Y", ""))]),
                                 .subset = .subset))
  
    
    #only when on area is selected and less than 10 items, more than 10 is too much clutter
  if(between((commodity |>  fndistinct()), 1,12) &
     between((country |> fndistinct()), 1, 12)){
    p <- subset |>
      lp("Item", "Commodity") + facet_wrap(vars(Area))
  } else if((commodity |> fndistinct()) > 12 & (country |> fndistinct()) <= 12){
    p <- subset |>
      lp() + theme(legend.position = "none")
  } else {
    p <- subset |>
      lp() + theme(legend.position = "none")
  }
  
   return(p)
}

####################################################Tree Map###############################################################
library(treemapify)
#library(ggplot2)

treemp <-function(df, year, .subset) {
  
  df <- ss(df, df$Element == .subset) |> na_omit()
  
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

#-------------------------------------------------Bar_Graph------------------------------------------------
library(scales)
#ordered by region
barPlot <- function(data) {
  
  itemLimit = 11
  values = limiter(data, areaLimit = 15, itemLimit = itemLimit, fillPos = "right")
  
  areaToUse = values[[1]]
  showFill = values[[2]]
  
  df <- pivoted_data(data = data) |> 
    sbt(Area %in% data$Area & Item %in% data$Item)

    df |> 
    ggplot(aes(x=eval(areaToUse), y=value)) +
    geom_col( width=.2, aes(fill = Item), position = position_fill()) +
    coord_flip() +
    labs(x = "Region", y = "Percent") +
    theme_grey() +
    theme(legend.position = showFill,
          axis.text = element_text(size = 12)) +
    if(fndistinct(df$Item) <= itemLimit) {
        scale_fill_brewer(type = "div", palette = "PRGn")
    } else {
      scale_fill_manual(values = rep("grey", length(df$Item)))
     }
   
}



#---------------------------------------Interactive Area-Chart-------------------------------------------------
areaPlot <- function(df, subset, commodity, country, year){
  
  pivoted_df <- pivoted_data(mapData(data = df,
                                     years = paste0("Y", 1961:year),
                                     .subset = subset,
                                     countries = country,
                                     commodities = commodity))
  
  #limit the amount of countries to plot at 7, if more, we use general continent data
  values = limiter(pivoted_df, areaLimit = 7, itemLimit = 7, fillPos = "bottom")
  
  areaToUse = values[[1]]
  showFill = values[[2]]
    
  new_df = pivoted_df  |>
    sbt(Area %in% df$Area & Item %in% df$Item) |>
    gby(eval(areaToUse), Year)|>
    smr(value = fsum(value)) |> 
    sbt(areaToUse != "") |> 
    fungroup()
  
    chart = new_df |> 
      group_by(areaToUse) |>
      e_charts(Year) |>
      e_area(value, stack = "stack") |>
      e_x_axis(type = "category") |>
      e_tooltip(trigger = "axis")
    
    return(chart)
}
 
#-------------------------------------------------------------------------------------------------------


