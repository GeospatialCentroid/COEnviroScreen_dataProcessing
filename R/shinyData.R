

shinyData <- function(removeNativeLand,removeZeroPop,version,spanish, overwrite){
  # individual layers
  # rural communities
  getRural()
  # coal communities
  getCoal()
  # oil and gas communities
  getOilGas()
  # justice 40
  getJustice40(filePath = "data/input/spatialLayers/justice40/1.0-communities.csv",
               removeNativeLand = removeNativeLand,
               overwrite = overwrite)
  # di communities
  getDI(removeNativeLand =removeNativeLand,
        overwrite = overwrite)

  # data for shiny app
  getShinyData(removeNativeLand = removeNativeLand,
               removeZeroPop = removeZeroPop,
               version = version,
               spanish = spanish)

}
