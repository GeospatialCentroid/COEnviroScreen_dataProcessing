# getDI_MHP - create new layer for Mobile Home Parks to be included alongside
# other DI community information.


getDI_MHC <- function(overwrite){
  # overwrite defines if you want to force the file to be recreated.

  pathToData <- "data/shinyContent/diCommunities_MHC.rds"

  if(file.exists(pathToData) & overwrite == FALSE){
    return(paste0("The DI community MHP data exists and can be found ", pathToData))
  }else{

    #import MHP point data and select vars of interest

    MHC <- read.csv("data/input/spatialLayers/MFH/FINAL_MFH_DOLA_23.3.30.csv")
    MHCvalid <- MHC %>%
      dplyr::filter(Latitude > 0)

    #create spatial layer

    geom <- MHCvalid %>%
      st_as_sf(.,coords=c("Longitude", "Latitude"),crs=4269)


    #write feature
    saveRDS(object = geom, file = pathToData)
    return(paste0("The DI community MHC data was writen ", pathToData))
  }
}
