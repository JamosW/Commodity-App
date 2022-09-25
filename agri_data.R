library(sf)
library(collapse)
library(stringr)
library(tidytable)
library(promises)
library(future)

plan(multisession)

#join XY data to data frame that is used for mapping
joined %<-% {data.table::fread("agricultural_commodities.csv")}
countriesData %<-% {rnaturalearthdata::countries50}

#take geometry from world sf object, get the X and Y Coordinates and append the columns
worldXY %<-%  {countriesData |>
    st_as_sf() |>
    st_geometry() |>
    st_make_valid() |>
    st_centroid() |>
    unlist() |>
    matrix(ncol = 2, byrow = T) |>
    as.data.frame() |>
    setNames(c("X", "Y")) |>
    cbind(name = countriesData$name, economy = countriesData$economy)}

  joined %<-% {joined |>
  fuzzyjoin::fuzzy_left_join(worldXY , match_fun = list(stringr::str_detect), by = c("Area" = "name")) %>%
    ss(!.$Area %in% c("Africa", "Asia", "Europe", "Americas", "Oceania") & 
         .$Element %in% c("Area harvested","Yield","Production") &
         !is.na(.$Area),
        !str_detect(names(.), "F$|\\sCode|Unit|name"))}
  
  countriesData <- NULL
  
joined[,c("Item", "Area") ] <- mapply(\(x,y,z) str_replace_all(x, y, z),list(joined$Item, joined$Area),c("\xe9","C\xf4"),c("e", "Co"))

mapData <- function(data, countries = NULL, commodities = NULL, years, .subset){
  
  cols <- c("Area", "Item", "Element", "X", "Y", "economy", years)
  
  data %<-% {data |> ss(, colnames(data) %in% cols)}
  
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
  
  pivoted %<-% {data %>% 
      pivot_longer.(cols = na_omit(str_extract(names(.), "Y\\d+")), 
                    names_to = "Year", values_to = "value") |> 
      mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  if_else.(value == 0 | is.na(value), 0, value))}
  
  return(pivoted)
  
}
  
  #limiter function for how many values can be displayed before using generalization
  limiter <-function(data, areaLimit, itemLimit, fillPos){
    
    areaToUse = c()
    showFill = c()
    
    if(fndistinct(data$Area) > areaLimit) {
      areaToUse = sym("economy")
    } else {
      areaToUse = sym("Area")
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
    
    
    plot <- ggplot(x) + 
      geom_smooth(aes(x = Year, y = value, color = ifelse(is.null(group),"red",.data[[group]])), 
                  na.rm = TRUE, se = FALSE, size = 1) +
      scale_y_continuous(labels = label_number(scale_cut = cut_long_scale())) + 
      labs(x = "Year",
           y = .subset,
           color = NULL) + theme_bw()
    
    return(plot)
  }
  
  years <- as.numeric(na_omit(str_extract(names(joined), "1.*|2.*")))
  
  subset <- pivoted_data(mapData(joined, countries = country,
                                 commodities = commodity, 
                                 years = str_c("Y",years[years <= as.numeric(str_replace(year, "Y", ""))]),
                                 .subset = .subset))
  
    
    #only when on area is selected and less than 10 items, more than 10 is too much clutter
  if(between.((commodity |>  fndistinct()), 1,12) &
     between.((country |> fndistinct()), 1, 12)){
    p <- subset |>
      lp("Item", "Commodity") + facet_wrap(vars(Area))
  } else if((commodity |> fndistinct()) > 12 & (country |> fndistinct()) <= 12){
    p <- subset |>
      lp()
  } else {
    p <- subset |>
      lp()
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
    xlab("Percent") +
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
library(viridis)
library(ggiraph)

areaPlot <- function(data, subset, commodity, country, year){
  
  values = limiter(data, areaLimit = 7, itemLimit = 7, fillPos = "bottom")
  
  areaToUse = values[[1]]
  showFill = values[[2]]
  
  years <- as.numeric(na_omit(str_extract(names(joined), "1.*|2.*")))
    
  my_df = pivoted_data(mapData(data = joined, 
                               years = str_c("Y",years[years <= as.numeric(str_replace(year, "Y", ""))]),
                               .subset = subset,
                       countries = country, 
                       commodities = commodity)) |>
    sbt(Area %in% data$Area & Item %in% data$Item) |>
    gby(eval(areaToUse), Year) |>
    smr(value = fsum(value)) |> 
    fungroup()
  
  valueChoice <- function(df, choice){
    value = df|> gby(areaToUse) |> 
      mtt(value = eval(call(choice, value)))
    
    return(value)
  }
  
  gg_area <- my_df |> 
    ggplot(aes(x=Year, y=value, fill = eval(areaToUse))) +
    geom_area_interactive(alpha=0.6 , size=.5, colour="white", 
                          tooltip = paste0(
                            my_df$areaToUse,
                            " 1961", ": ",
                            tryCatch(expr = valueChoice(my_df, "first")$value, error = function(e) e, finally = ""), 
                            " ", year, ": ",
                            tryCatch(expr = valueChoice(my_df, "last")$value, error = function(e) e, finally = ""))) +
    scale_fill_viridis(discrete = T) +
    labs(x = "", fill = NULL) +
    theme(legend.position = showFill,
          axis.text = element_text(size = 6)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_long_scale()), trans = "sqrt") +
    theme_bw()

  return(girafe(ggobj = gg_area, width_svg = 8, height_svg = 4))
}
 
#-------------------------------------------------------------------------------------------------------

# pivoted_data(mapData(joined, .subset = "Yield", years = "Y2020", commodities = "Oranges"))
# 
# areaPlot(mapData(joined, .subset = "Yield", years = "Y2020", commodities = "Oranges") |>
#            na_omit(),
#         year = "Y2020", commodity = "Oranges", country = "Canada", .subset = "Yield")
# 
# 
# pivoted <- joined %>%
#   pivot_longer.(cols = na.omit(str_extract(names(.), "Y\\d+")),
#                 names_to = "Year", values_to = "value") |>
#   mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  if_else.(value == 0 | is.na(value), 0, value))
# 
# 
# 
# 
# 
# 
# bP <- function(data, year, .subset) {
# 
#   itemLimit = 11
#   values = limiter(data, areaLimit = 15, itemLimit = itemLimit, fillPos = "right")
# 
#   areaToUse = values[[1]]
#   showFill = values[[2]]
# 
#   df <- pivoted |>
#     sbt(Area %in% data$Area & Item %in% data$Item  &
#           Year == year & Element == .subset)
# 
#   df |>
#     ggplot(aes(x=eval(areaToUse), y=value)) +
#     geom_col( width=.2, aes(fill = Item), position = position_fill()) +
#     coord_flip() +
#     xlab("Percent") +
#     theme_grey() +
#     theme(legend.position = showFill,
#           axis.text = element_text(size = 12)) +
#     if(fndistinct(df$Item) <= itemLimit) {
#       scale_fill_brewer(type = "div", palette = "PRGn")
#     } else {
#       scale_fill_manual(values = rep("grey", length(df$Item)))
#     }
#   return(df)
# }
# areaPlot(data = mapData(data = joined, countries = "Canada", .subset = "Yield", years = c("Y2020")))
# 
# pivoted_data(mapData(data = joined, countries = "Canada", .subset = "Yield", years = c("Y2020")))
# # |>
# #   sbt(Area %in% data$Area & Item %in% data$Item  &
# #         Year == year & Element == .subset)
# 
# mapData(data = joined, countries = "Canada", .subset = "Yield", years = c("Y2020")) %>%
#     pivot_longer.(cols = na_omit(str_extract(names(.), "Y\\d+")),
#                   names_to = "Year", values_to = "value") |>
#     mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  if_else.(value == 0 | is.na(value), 0, value))
# 
# 
# 
# 
# values = limiter(md, areaLimit = 7, itemLimit = 7, fillPos = "bottom")
# 
# areaToUse = values[[1]]
# showFill = values[[2]]
# 
# md
# 
# my_df = pivoted_data(md) |>
#   sbt(Area %in% md$Area & Item %in% md$Item) |>
#   gby(eval(areaToUse), Year) |>
#   smr(value = fsum(value)) |>
#   fungroup()
# 
# pivoted_data(md) |>
#   sbt(Area %in% md$Area & Item %in% md$Item) |>
#   gby(eval(areaToUse), Year) |>
#   smr(value = fsum(value)) |> 
#   fungroup()
# my_df
# 
# valueChoice <- function(df, choice){
#   value = df|> gby(areaToUse) |> 
#     mtt(value = eval(call(choice, value)))
#   
#   return(value)
# }
# 
# year <- str_remove_all(names(my_df)[str_detect(names(my_df), "Y2|Y1")], "Y")
# 
# gg_area <- my_df |> 
#   ggplot(aes(x=Year, y=value, fill = eval(areaToUse))) +
#   geom_area_interactive(alpha=0.6 , size=.5, colour="white", 
#                         tooltip = paste0(
#                           my_df$areaToUse,
#                           " 1961", ": ",
#                           tryCatch(expr = valueChoice(my_df, "first")$value, error = function(e) e, finally = ""), 
#                           " ", year, ": ",
#                           tryCatch(expr = valueChoice(my_df, "last")$value, error = function(e) e, finally = ""))) +
#   scale_fill_viridis(discrete = T) +
#   labs(x = "", fill = NULL) +
#   theme(legend.position = showFill,
#         axis.text = element_text(size = 6)) +
#   scale_y_continuous(labels = label_number(scale_cut = cut_long_scale()), trans = "sqrt") +
#   theme_bw()
# 
# return(girafe(ggobj = gg_area, width_svg = 8, height_svg = 4))
# 
# pivoted_data(md)
# md
# 
# mapData(data = joined, 
#         years = na_omit(str_extract(names(joined), "Y1.*|Y2.*")),
#         .subset = "Yield")
# 
# lp <- function(x, group, clr)
# {
#   ggplot(x) +
#     geom_smooth(aes(x = Year, y = value, color = .data[[group]]),
#                 na.rm = TRUE, se = FALSE, size = 1) +
#     scale_y_continuous(labels = label_number(scale_cut = cut_long_scale())) +
#     labs(x = "Year",
#          y = "fire",
#          color = clr) + theme_bw()
# }
# 
# years <- as.numeric(na_omit(str_extract(names(joined), "1.*|2.*")))
# 
# subset <- pivoted_data(mapData(joined, countries = sample(joined$Area, 30),
#                                commodities = sample(joined$Item, 12),
#                                years = str_c("Y",years[years <= as.numeric(str_replace("Y2020", "Y", ""))]),
#                                .subset = "Yield"))
# 
# subset
# 
# country <- subset$Area
# commodity <- subset$Item
# 
# #only when on area is selected and less than 10 items, more than 10 is too much clutter
# if(between.((commodity |>  fndistinct()), 1,12) &&
#    between.((country |> fndistinct()), 1, 12)){
#   p <- subset |>
#     lp("Item", "Commodity") + facet_wrap(vars(Area))
# } else if((commodity |> fndistinct()) > 12 && (country |> fndistinct()) <= 12){
#   p <- subset |>
#     lp("Area", "Country") + facet_wrap(vars(Area))
# } else {
#   p <- subset |>
#     lp("Item", "Commodity")
# }
# 
# p
# 
# subset$Item |> fndistinct()
