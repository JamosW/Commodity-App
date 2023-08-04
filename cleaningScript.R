library(sf)
library(collapse)
library(stringr)
library(ggplot2)

#join XY data to data frame that is used for mapping
joined <- data.table::fread("agricultural_commodities.csv")
countriesData <- rnaturalearthdata::countries50

#take geometry from world sf object, get the X and Y Coordinates and append the columns
worldXY <- countriesData |>
  st_as_sf() |>
  st_geometry() |>
  st_make_valid() |>
  st_centroid() |>
  unlist() |>
  matrix(ncol = 2, byrow = T) |>
  as.data.frame() |>
  setNames(c("X", "Y")) |>
  cbind(name = countriesData$name, economy = countriesData$economy)

newJoined <- joined|>
  fuzzyjoin::fuzzy_left_join(worldXY , match_fun = list(stringr::str_detect), by = c("Area" = "name")) |> 
  (\(x){
    x |> 
      ss(!x$Area %in% c("Africa", "Asia", "Europe", "Americas", "Oceania", "World", "South-eastern Asia") & 
               x$Element %in% c("Area harvested","Yield","Production") & !is.na(x$Area), 
          !str_detect(names(x), "F$|\\sCode|Unit|name"))
  })()


newJoined[, c("Item", "Area") ] <- mapply(\(x,y,z) str_replace_all(x, y, z),list(newJoined$Item, newJoined$Area),c("\xe9","C\xf4"),c("e", "Co"))

pivotedJoined <- newJoined %>% 
    tidytable::pivot_longer(cols = na_omit(str_extract(names(.), "Y\\d+")), 
                            names_to = "Year", values_to = "value") |> 
  mtt(Year = as.numeric(str_replace_all(Year, "Y", "")), value =  ifelse(value == 0 | is.na(value), 0, value)) |>
  sbt(Area %in% joined$Area & Item %in% joined$Item)
  

Map(fwrite, list(newJoined, pivotedJoined), list("cleanedFrame.csv", "pivotedCleaned.csv"))
