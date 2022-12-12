

enviromentalEffects <- function(geometry, processingLevel, ejscreen, version, overwrite = overwrite){
  file <- paste0("data/output/enviroscreenScore/", processingLevel,"/enviromentalEffects_",version,".csv")

  if(!file.exists(file) || overwrite == TRUE){
  # run functions
  cat("EJscreen")
  d1 <- ejscreen %>%
    dplyr::select("GEOID","waterDischarge","nplProx","rmpProx","tsdfProx")

  cat("Surface Water")
  d2 <- getSurfaceWater(filePath = "data/input/surfaceWater/Streams303dLayerFinal.shp",
                        processingLevel = processingLevel,
                        geometry = geometry,
                        version = version,
                        overwrite = overwrite)
  cat("Mining")
  d3 <- getmines(geometry,
                 processingLevel,
                 version = version,
                 overwrite = overwrite)

  cat("Oil and Gas")
  d4 <- getProxyOilGas(geometry = geometry,
                       processingLevel = processingLevel,
                       version = version,
                       overwrite = overwrite)

  # combine datasets
  dataframes <- list(d1,d2,d3,d4)
  df <- joinDataFrames(dataframes)


  # determine the average value across all features
  df$envEff <- df %>%
    select(contains("_pcntl"))%>%
    apply(MARGIN = 1, FUN = gm_mean)

  # write out content
  write_csv(df, file = file)
  }else{
    df <- read_csv(file)
  }
  return(df)
}
