# COEnviroScreen_dataProcessing

Colorado EnviroScreen is a data processing code base with a public facing shiny application that highlights the spatial variability of specific Environmental Health and Environmental Justice challenges throughout Colorado. The repository contain the code for generating the mertrics used to calculate the EnviroScreen score.

For a detailed description of the how to utilize this code base please see the full tutorial here *add link to webpage on landing page of the site*

## Quick Start 

1. Download repository
2. open R Project file
3. Open 0_main.R
4. run the code listed below
```r
# setup -------------------------------------------------------------------
# load required libraries
# install.packages("pacman")
pacman::p_load(tigris,tidycensus,dplyr,sf,stringr,tictoc,vroom,terra,arcpullr,purrr,tidyr,rmapshaper,readr,lubridate)

# source loadFunctions then load functions
source("R/utils/loadFunctions.R")
loadFunctions()

# create the file folder structure
createFolderStructure()
```

5. download the input dataset from https://github.com/GeospatialCentroid/COEnviroScreen_dataInputs and copy them into `~data/input` folder within your R Project director.
6. Extract the follow datasets

- "data/input/EJScreen/EJScreen.7z"
- "data/input/floodPlains/floodPlains.7z"
- "data/input/noise/CONUS_L50dBA_sumDay_exi.7z"
- "data/input/spatialLayers/justice40.7z"

7. Run the following Code

```r
# pull geometry layers
getGeometryLayers()

#set version
version <- 1

# set census API key
tidycensus::census_api_key(key = "your key")



# process data  -----------------------------------------------------------
## run single or multiple geometries
  geoms <- c("county","censusTract","censusBlockGroup")

# running single component
processData(processingLevel=geoms[1],
            version = version,
            overwrite = FALSE)
```
> Running the processingData function on the county level first will allow you quickly troubleshoot any relative path issues. The processing time for the census tract and census block groups is significantly long so you want to make sure everything is running as expected before attempting to render those datasets.

8. Run the process for all geometries.

```r
# running all components
for(i in geoms){
  print(i)
  processData(processingLevel=i,
              version = version,
              overwrite = FALSE)
}

```
> After this step you will have successfully recreated the EnviroScreen score values for each geography. Your output values should match the content that can be downloaded directly from the shiny applications.

9. Generate results for the shiny application

This is optional, but does create some new content. *Note*; This function will not run unless you have successfully rendered the EnviroScreen score values at each of the defined geographies.

```r
# shiny Elements ------------------------------------------------
shinyData(removeNativeLand = TRUE,
          removeZeroPop = TRUE,
          version = version,
          spanish = TRUE,
          overwrite = FALSE)
```
