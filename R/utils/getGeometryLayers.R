#' Get Geometry Layers
#'
#' @param overwrite : determines if you want to pull content a new and overwrite existing
#'  features
#'
#' @description : grabs spatial date from 2019 census for the state, county, census tract,
#' and census block groups of Colorado. Saves features as geojsons in established folder
#' structure
#'

getGeometryLayers <- function(overwrite=FALSE){
  # State of Colorado layer
  # Federal Information Processing Standards(FIPS) code for Colorado
  fips <- "08"

  ### State Data
  # test for existing file folder structure
  path1 <- "data/output/spatialLayers/state"

  print("Downloading and saving state level spatial data")
  if(!file.exists(paste0(path1,"/colorado.geojson")) | overwrite == TRUE){
    # call all state data and filter to colorado
    states <- tigris::states() %>%
      dplyr::filter(STATEFP  == fips)
    # write object as a geojson
    sf::st_write(obj = states,
                 dsn = paste0(path1,"/colorado.geojson"),
                 driver = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change overwrite to TRUE to force Download")
  }

  ### County Data
  # test for existing file folder structure
  path2 <-  "data/output/spatialLayers/county"


  print("Downloading and saving county level spatial data")
  if(!file.exists(paste0(path2,"/coloradoCounties.geojson")) | overwrite == TRUE){

    # Counties within the state of Colorado
    counties <- tigris::counties(state = fips, year = 2019)
    # write out spatial data
    sf::st_write(obj = counties,
                 dsn = paste0(path2,"/coloradoCounties.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change overwrite to TRUE to force Download")
  }

  ### Census Tract
  # test for existing file folder structure
  path3 <-  "data/output/spatialLayers/censusTracts"

  cat("Downloading and saving census tract level spatial data")
  if(!file.exists(paste0(path3,"/coloradoCensusTracts.geojson")) | overwrite == TRUE){
    # Census Tracts within the state of Colorado
    tracts <- tigris::tracts(state = fips, year = 2019)
    # writing census tract data
    sf::st_write(obj = tracts,
                 dsn = paste0(path3,"/coloradoCensusTracts.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change overwrite to TRUE to force Download")
  }

  ### Census Block Group
  # test for existing file folder structure
  path4 <-  "data/output/spatialLayers/censusBlockGroups"

  cat("Downloading and saving census block group level spatial data")
  if(!file.exists(paste0(path4,"/coloradoCensusBlockGroups.geojson")) | overwrite == TRUE){
    # census block groups within the state of Colorado
    blockGroups <- tigris::block_groups(state = fips, year = 2019)
    sf::st_write(obj = blockGroups,
                 dsn = paste0(path4,"/coloradoCensusBlockGroups.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change overwrite to TRUE to force Download")

  }
  print("All files have been downloaded.")
}



