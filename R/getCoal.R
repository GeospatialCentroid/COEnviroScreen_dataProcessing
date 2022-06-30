


getCoal <- function(){
  # defination for what counties meet conditions  ---------------------------
  coal <- str_to_title(c("MOFFAT","ROUTT","MORGAN","EL PASO","PUEBLO"))
  # add condition to spatial object  ----------------------------------------
  geometry <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()
  # define rural features
  geometry$coal = ifelse(geometry$NAME %in% coal, "Yes", "No")
  # select columns of interest
  geom <- geometry %>%
    dplyr::select(
      "GEOID","NAME","NAMELSAD" ,"LSAD" ,"geometry" ,"coal"
    )

  geom2 <- geom %>%
    dplyr::filter(coal == "Yes")%>%
    patternLayer(pattern = "vertical",mode = "sfc", density = 4)

  # export
  saveRDS(object = geom, file = "data/shinyContent/coalCommunity.rds")
  saveRDS(object = geom2, file = "data/shinyContent/coalVis.rds")
  return(paste0("The Coal community spatial data was writen to data/shinyContent/coalVis.rds"))
}
