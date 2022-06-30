#' Set Spatial Scale
#'
#' Grabs the spatial feature for specific geographic scale.
#'
#' @param fileFolder File path to where `getGeometry` had save content
#' @param geometry One of the following three characters stings ("county","censusTract", "censusBlockGroup")
#'
#' @return A sf object of the geographic scale of interest.
#' @export
#'
setSpatialData <- function(fileFolder, geometry){
  if(geometry == "censusBlockGroup"){
    data <- sf::st_read(paste0(dataFolder,"/censusBlockGroup/coloradoCensusBlockGroups.geojson"))
  }
  if(geometry == "censusTract"){
    data <- sf::st_read(paste0(dataFolder,"/censusTract/coloradoCensusTracts.geojson"))
  }
  if(geometry == "county"){
    data <- sf::st_read(paste0(dataFolder,"/county/coloradoCounties.geojson"))
  }
  if(class(data)[1]== "sf"){
    return(data)
  }else{
    stop("No spatial object was created. Please check your input parameter")
  }
}
