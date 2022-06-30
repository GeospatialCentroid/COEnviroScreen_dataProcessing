#' Set spatial data layer
#'
#' @param processingLevel : character describing the processling level. required "county", "censusTract", or"censusBlockGroup"
#'
#' @return sf object of colorado geography

setSpatialData <- function(processingLevel){

  if(processingLevel == "censusBlockGroup"){
    geom <- sf::st_read("data/output/spatialLayers/censusBlockGroup/coloradoCensusBlockGroups.geojson")
  }
  if(processingLevel == "censusTract"){
    geom <- sf::st_read("data/output/spatialLayers/censusTract/coloradoCensusTracts.geojson")
  }
  if(processingLevel == "county"){
    geom <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")
  }
  if(class(geom)[1]== "sf"){
    return(geom)
  }else{
    stop("No spatial object was created. Please check your input parameter")
  }
}


