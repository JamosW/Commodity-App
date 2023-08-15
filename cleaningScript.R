library(sf)
library(collapse)
library(stringr)
library(ggplot2)

#join XY data to data frame that is used for mapping
agri_data <- data.table::fread("new_agri_data.csv")
world_shape <- read_sf("world_shapefile") |> st_drop_geometry()

#convert to integer for easy binding of the similarities of the data with world_shape
agri_data$`Area Code (M49)` <- as.integer(sub("'", "", agri_data$`Area Code (M49)`))

#get rid of area's that don't have lat or are not actually countries
agri_data <- agri_data |> 
  left_join(world_shape[, c("UN", "LON", "LAT")], by = c("Area Code (M49)" = "UN")) |>
  sbt(!is.na(LAT) & Element %in% c("Area harvested","Yield","Production"))
  

 

# #take geometry from world sf object, get the X and Y Coordinates and append the columns
# worldXY <- countriesData |>
#   st_as_sf() |>
#   st_geometry() |>
#   st_make_valid() |>
#   st_centroid() |>
#   unlist() |>
#   matrix(ncol = 2, byrow = T) |>
#   as.data.frame() |>
#   setNames(c("X", "Y")) |>
#   cbind(name = countriesData$name, economy = countriesData$economy)
# 
# 
# newJoined <- joined|>
#   fuzzyjoin::fuzzy_left_join(worldXY , match_fun = list(stringr::str_detect), by = c("Area" = "name")) |> 
#   (\(x){
#     x |> 
#       ss(!x$Area %in% c("Africa", "Asia", "Europe", "Americas", "Oceania", "World", "South-eastern Asia") & 
#                x$Element %in% c("Area harvested","Yield","Production") & !is.na(x$Area), 
#           !str_detect(names(x), "F$|\\sCode|Unit|name"))
#   })()


agri_data[, c("Item", "Area")] <- Map(\(x,y,z) str_replace_all(x, y, z),list(agri_data$Item, agri_data$Area),c("\xe9","C\xf4"),c("e", "Co"))


fwrite(agri_data,"cleanedFrame2.csv")
  