

enviromentalExposures <- function(geometry, processingLevel, ejscreen, version, overwrite){

  file <- paste0("data/output/enviroscreenScore/", processingLevel,"/enviromentalExposures_",version,".csv")

  if(!file.exists(file)||overwrite == TRUE){

    cat("Ozone")
    d1 <- getOzone(filePath = "data/input/epa_cmaq/2017_ozone_daily_8hour_maximum.txt.gz" ,
                   geometry = geometry,
                   processingLevel = processingLevel,
                   version = version,
                   overwrite = overwrite)

    cat("pm25")
    d2 <- getPM25(filePath = "data/input/epa_cmaq/2017_pm25_daily_average.txt.gz",
                  geometry = geometry,
                  processingLevel = processingLevel,
                  version = version,
                  overwrite = overwrite)

    cat("ejscreen")
    d3 <- ejscreen %>%
      dplyr::select("GEOID","leadPaint","deiselPM", "trafficeProx")

    cat("haps")
    d4 <- getHAPS(filePath = "data/input/haps/APENs 8_24_2021.csv",
                  geometry = geometry,
                  processingLevel = processingLevel,
                  version = version,
                  overwrite = overwrite)

    cat("other air Pollutants")
    d5 <- getOtherHAPS(filePath = "data/input/haps/APENs 8_24_2021.csv",
                       geometry = geometry,
                       processingLevel = processingLevel,
                       version = version,
                       overwrite = overwrite)

    cat("drinking water")
    d6 <- getDrinkingWater(geometry = geometry,
                           processingLevel = processingLevel,
                           version = version,
                           overwrite = overwrite)
    cat("noise")
    d7 <- getNoise(filePath = "data/input/noise/CONUS_L50dBA_sumDay_exi.tif",
                   geometry = geometry,
                   processingLevel = processingLevel,
                   version = version,
                   overwrite = overwrite)

    # combine datasets
    dataframes <- list(d1,d2,d3,d4,d5,d6,d7)
    df <- joinDataFrames(dataframes)


    # determine the average value across all features
    df$envExp <- df %>%
      select(contains("_pcntl"))%>%
      apply(MARGIN = 1, FUN = gm_mean)

    #write out content
    write_csv(df, file = file)
  }else{
    df <- read_csv(file)
  }
  return(df)
}
