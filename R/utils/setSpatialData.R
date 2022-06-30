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
    data <- sf::st_read("data/output/censusBlockGroup/coloradoCensusBlockGroups.geojson")
  }
  if(processingLevel == "censusTract"){
    data <- sf::st_read("data/output/censusTract/coloradoCensusTracts.geojson")
  }
  if(processingLevel == "county"){
    data <- sf::st_read("data/output/county/coloradoCounties.geojson")
  }
  if(class(data)[1]== "sf"){
    return(data)
  }else{
    stop("No spatial object was created. Please check your input parameter")
  }
}


