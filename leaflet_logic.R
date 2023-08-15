library(leaflet)

leaf_extra <- function(m, tit, dat, val, pall) {
  #function for map, to reduce repitition
  newM <- m |> addCircleMarkers(dat,
                                radius = 6,
                                lng = ~ LON, lat = ~ LAT,
                                label = ~ htmlEscape(paste(Area, eval(val))),
                                color = ~ pall(eval(val)),
                                stroke = FALSE, 
                                fillOpacity = 0.6) |>
    addLegend(pal = pall,
              values = ~ eval(val),
              title = tit)
  
  return(newM)
}
