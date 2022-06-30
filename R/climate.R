
climate <- function(geometry, processingLevel,version, overwrite){
  file <- paste0("data/output/enviroscreenScore/",processingLevel,"/climate_",version,".csv")

  if(!file.exists(file)|| overwrite == overwrite){
    cat("wildfire")
    d1 <- getWildfire(filePath = "data/input/wildfire/whp2020_cnt_conus.tif" ,
                      processingLevel = processingLevel,
                      geometry= geometry,
                      version = version,
                      overwrite = overwrite)

    cat("floodplain")
    d2 <- getFloodplain(filePath = "data/input/floodPlains/floodHazard.shp",
                        processingLevel = processingLevel,
                        geometry= geometry,
                        version = version,
                        overwrite = overwrite)

    cat("Heat Days")
    d3 <- getHeatDays(filePath = "data/input/heatDays/data_172346.csv",
                      processingLevel = processingLevel,
                      geometry= geometry,
                      version = version,
                      overwrite = overwrite)

    cat("drought")
    d4 <- getDrought(filePath = "data/input/drought/dm_export_20150101_20291231.csv",
                     processingLevel = processingLevel,
                     geometry= geometry,
                     version = version,
                     overwrite = overwrite)

    # combine datasets
    dataframes <- list(d1,d2,d3,d4)
    df <- joinDataFrames(dataframes)

    # determine the average value across all features
    df$climate <- df %>%
      select(contains("_pcntl"))%>%
      apply(MARGIN = 1, FUN = gm_mean)

    #write out content
    write_csv(df, file = file)
  }else{
    df <- read.csv(file)
  }
  return(df)
}
