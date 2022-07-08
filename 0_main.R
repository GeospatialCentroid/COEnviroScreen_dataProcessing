###
# general workflow structure
###

# download data processing code base
# download data from https://github.com/GeospatialCentroid/COEnviroScreen_dataInput
#


# setup -------------------------------------------------------------------
# load required libraries
# install.packages("pacman")
pacman::p_load(tigris,tidycensus,dplyr,sf,stringr,tictoc,vroom,terra,arcpullr,purrr,tidyr,rmapshaper,readr,lubridate)

# source loadFunctions then load functions
source("R/utils/loadFunctions.R")
loadFunctions()

# create the file folder structure
createFolderStructure()

# copy content from the data repo into the "data/input file folder
# you will be rewriting over the existing folder structure.

# pull geometry layers
getGeometryLayers()

#set version
version <- 1

# set census API key
# tidycensus::census_api_key(key = "your key")



# process data  -----------------------------------------------------------
## run single or multiple geometries
geoms <- c("county","censusTract","censusBlockGroup")

# running single component
tic()
processData(processingLevel=geoms[1],
            version = version,
            overwrite = FALSE)
toc()

#   running all components
for(i in geoms){
  print(i)
  processData(processingLevel=i,
              version = version,
              overwrite = FALSE)
}

# shiny Elements ------------------------------------------------
shinyData(removeNativeLand = TRUE,
          removeZeroPop = TRUE,
          version = version,
          spanish = TRUE,
          overwrite = FALSE)
