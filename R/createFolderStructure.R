#' Create Folder Structure
#'
#' @param fileLocation : full path to where you will be running processing from
#'
#'@description : Generate a standard file structure for the project.
#'
#'
createFolderStructure <- function(fileLocation = NULL){
  if(is.null(fileLocation)){
    fileLocation <- getwd()
  }
  # data --------------------------------------------------------------------
  dir <- paste0(fileLocation, "/data")
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # input -------------------------------------------------------------------
  dir1 <- paste0(dir, "/input")
  if(!dir.exists(dir1)){
      dir.create(dir1)
    }
  # individual file folders
  ## indicator datasets
  inputs <- c(
    "asthma"
    ,"CDC_places"
    ,"lifeExpectency"
    ,"drinkingWater"
    ,"drought"
    ,"EJScreen"
    ,"floodPlains"
    ,"haps"
    ,"heartDisease"
    ,"heatDays"
    ,"lowBirthWeight"
    ,"mining"
    ,"noise"
    ,"oilAndGas"
    ,"surfaceWater"
    ,"wildfire")
  for(i in inputs){
    dir1a <- paste0(dir1, "/", i)
    if(!dir.exists(dir1a)){
      dir.create(dir1a)
    }
  }
  # spatial data
  spatialLayers <- c("justice40")
  # generate folders
  dir1b <- paste0(dir1, "/spatialLayers")
  if(!dir.exists(dir1b)){
    dir.create(dir1b)
  }
  for(i in spatialLayers){
    dir1b <- paste0(dir1, "/spatialLayers/", i)
    if(!dir.exists(dir1b)){
      dir.create(dir1b)
    }
  }

  # outputs -----------------------------------------------------------------
  dir2 <- paste0(dir, "/output")
  if(!dir.exists(dir2)){
    dir.create(dir2)
  }
  #indicators
  outputs <- c(
    "acs"
    ,"asthma"
    ,"CDC_places"
    ,"lifeExpectency"
    ,"drinkingWater"
    ,"drought"
    ,"EJScreen"
    ,"pm25"
    ,"floodPlains"
    ,"haps"
    ,"heartDisease"
    ,"heatDays"
    ,"houseBurden"
    ,"lowBirthWeight"
    ,"mining"
    ,"noise"
    ,"oilAndGas"
    ,"ozone"
    ,"surfaceWater"
    ,"wildfire"
    ,"enviroscreenScore"
    ,"enviroscreenScore/county"
    ,"enviroscreenScore/censusTract"
    ,"enviroscreenScore/censusBlockGroup"
  )
  # generate folders
  for(i in outputs){
    dir2a <- paste0(dir2, "/", i)
    if(!dir.exists(dir2a)){
      dir.create(dir2a)
    }
  }
  # spatial data
  spatialLayers <- c(
    "state"
    ,"county"
    ,"censusTracts"
    ,"censusBlockGroups"
    ,"justice40"
    ,"ruralCommunity"
    ,"oilAndGasCommunity"
    ,"diCommunity"
    ,"coalCommunity"
  )
  dir2b <- paste0(dir2, "/spatialLayers")
  if(!dir.exists(dir2b)){
    dir.create(dir2b)
  }
  for(i in spatialLayers){
    dir2b <- paste0(dir2, "/spatialLayers/", i)
    if(!dir.exists(dir2b)){
      dir.create(dir2b)
    }
  }
  # shiny -------------------------------------------------------------------
  dir3 <- paste0(dir, "/shinyContent")
  if(!dir.exists(dir3)){
    dir.create(dir3)
  }

}
