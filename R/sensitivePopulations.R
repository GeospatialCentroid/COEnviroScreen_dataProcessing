

sensitivePopulations <- function(geometry, processingLevel, ejscreen, version, overwrite){

  file <- paste0("data/output/enviroscreenScore/", processingLevel,"/sensitivePopulations_",version,".csv")

  if(!file.exists(file) || overwrite == TRUE){

    cat("ejscreen")
    d1 <- ejscreen %>%
      dplyr::select("GEOID","under5","over64")

    cat("heart Disease")
    d2 <- getHeartDisease(filePath = "data/input/heartDisease/Heart_Disease_in_Adults_-_CDPHE_Community_Level_Estimates_(Census_Tracts).csv",
                          geometry = geometry,
                          processingLevel =processingLevel,
                          version = version,
                          overwrite = overwrite)

    cat("asthma")
    d3 <- getAsthma(filePath = "data/input/asthma/Asthma_Hospitalization_Rate_(Census_Tracts).csv",
                    geometry = geometry,
                    processingLevel =processingLevel,
                    version = version,
                    overwrite = overwrite)

    cat("life Expectancy")
    d4 <- getLifeExpectancy(filePath = "data/input/lifeExpectency/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv",
                            geometry = geometry,
                            processingLevel =processingLevel,
                            version = version,
                            overwrite = overwrite)

    cat("low birth weight")
    d5 <- getLowBirthWeight(filePath = "data/input/lowBirthWeight/Low_Weight_Birth_Rate_(Census_Tracts).csv",
                            geometry = geometry,
                            processingLevel =processingLevel,
                            version = version,
                            overwrite = overwrite)

    cat("places health data")
    d6 <- getPlacesData(filePath = "data/input/CDC_places/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2021_release.csv",
                        geometry = geometry,
                        processingLevel =processingLevel,
                        version = version,
                        overwrite = overwrite)

    ## to account for the positive nature of life expectancy
    d4$lifeExpectancy <- -1 * d4$lifeExpectancy


    # combine all dataframes
    dataframes <- list(d1,d2,d3,d4,d5,d6)

    df <- joinDataFrames(dataframes)

    df$lifeExpectancy <- -1 * df$lifeExpectancy

    # determine the average value across all features
    df$senPop <- df %>%
      select(contains("_pcntl"))%>%
      apply(MARGIN = 1, FUN = gm_mean)
    #write out content
  }else{
    df <- read.csv(file)
  }
    return(df)
}
