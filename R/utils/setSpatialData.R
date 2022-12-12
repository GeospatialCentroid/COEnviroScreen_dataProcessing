#' Set Spatial Data
#'
#' @param processingLevel : county, censusTract, or censusBlockGroups.
#'
#' @description : Defines the spatial feature returned and the processinglevel
#'  used for the data processing.
#'
#' @return : an sf object

setSpatialData <- function(processingLevel){
  if(processingLevel == "censusBlockGroup"){
    data <- sf::st_read("data/output/spatialLayers/censusBlockGroups/coloradoCensusBlockGroups.geojson")
  }
  if(processingLevel == "censusTract"){
    data <- sf::st_read("data/output/spatialLayers/censusTracts/coloradoCensusTracts.geojson")
  }
  if(processingLevel == "county"){
    data <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")
  }
  if(class(data)[1]== "sf"){
    return(data)
  }else{
    stop("No spatial object was created. Please check your input parameter")
  }
}


