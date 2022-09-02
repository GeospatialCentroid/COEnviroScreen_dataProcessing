


getOilGas <- function(){
  # defination for what counties meet conditions  ---------------------------
  og_counties <- str_to_title(c("ADAMS","ARAPAHOE","ARCHULETA","BACA","BENT","BOULDER","BROOMFIELD","CHEYENNE","DELTA",
                                "DOLORES","ELBERT","FREMONT","GARFIELD","GUNNISON","HUERFANO","JACKSON","KIOWA","KIT CARSON",
                                "LA PLATA","LARIMER","LAS ANIMAS","LINCOLN","LOGAN","MESA","MOFFAT","MONTEZUMA","MORGAN",
                                "PHILLIPS","PROWERS","RIO BLANCO","ROUTT","SAN MIGUEL","SEDGWICK","WASHINGTON","WELD","YUMA"
  ))
  # add condition to spatial object  ----------------------------------------
  geometry <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify(keep_shapes = TRUE)
  # define rural features
  geometry$oilGas = ifelse(geometry$NAME %in% og_counties, "Yes", "No")
  # select columns of interest
  geom <- geometry %>%
    dplyr::select(
      "GEOID","NAME","NAMELSAD" ,"LSAD" ,"geometry" ,"oilGas"
    )

  geom2 <- geom %>%
    dplyr::filter(oilGas == "Yes")%>%
    patternLayer(pattern = "horizontal",
                 mode = "sfc",
                 density = 4)

  # export
  saveRDS(object = geom, file = "data/shinyContent/oilgasCommunity.rds")
  saveRDS(object = geom2, file = "data/shinyContent/oilgasVis.rds")
  return(paste0("The Oil and Gas community spatial data was writen to data/shinyContent/oilgasVis.rds"))
}
