


getJustice40 <- function(filePath, removeNativeLand, overwrite){

  pathToData <- "data/shinyContent/justice40.rds"

  if(file.exists(pathToData) & overwrite == FALSE){
    return(paste0("The DI community spatial data exists and can be found ", pathToData))
  }else{
    # filter justice40 to colorado
    d1 <- read_csv(filePath)%>%
      dplyr::filter(`State/Territory` == "Colorado")
    # join with spatial data
    county <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")%>%
      st_drop_geometry()%>%
      dplyr::select("County_Name" = "NAME", "COUNTYFP")

    ct <- sf::st_read("data/output/spatialLayers/censusTracts/coloradoCensusTracts.geojson") %>%
      dplyr::left_join(y = county, by = ("COUNTYFP")) %>%
      dplyr::left_join(d1, by = c("GEOID" = "Census tract ID"))%>%
      dplyr::select("GEOID","County_Name" , "Total threshold criteria exceeded", "Identified as disadvantaged")%>%
      dplyr::filter(`Identified as disadvantaged` == TRUE)%>%
      rmapshaper::ms_simplify()

    if(removeNativeLand == TRUE){
      censusTractsNative <- c("08083941100","08067940400", "08067940300", "08007940400")
      features <- c()
      for(i in seq_along(censusTractsNative)){
        features <- c(features, grep(pattern =  censusTractsNative[i], x = ct$GEOID))
      }
      ct <- ct[-features, ]
    }

    # export result
    saveRDS(object = ct, pathToData)
  }
}
