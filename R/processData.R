#' Primary data processing script
#'
#' @description : Primary processing script for creating the data associated with
#' the enviroscreen project. the overwrite p
#'
#' @param processingLevel : character description of the processing level
#' @param version :  character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#'
#'
#' @return : dataframe of the cumulative content generate at each group component score
#' @export : csv data for ejscreen, acsData, environmental exposures, environmental effects, climate, senpop, soceco, final component scores
#'
processData <- function(processingLevel, version, overwrite){
  # call in spatial object at give extent
  geometry <- setSpatialData(processingLevel = processingLevel)

  ### EJScreen and ACS data contributes to multiple components run it here then split out
  ejscreen <- ej_screen(filePath = "data/input/EJScreen/EJSCREEN_2021_StatePctile.csv",
                              geometry = geometry,
                              processingLevel = processingLevel,
                              version = version,
                              overwrite = overwrite)
  ### add condition to test for the existence of a specific file based on geom
  acsData <- acs(processingLevel = processingLevel,
                 version = version,
                 overwrite = overwrite)


  # Exposures ---------------------------------------------------------------
  envExposures <- enviromentalExposures(geometry = geometry,
                                          version = version,
                                          ejscreen = ejscreen,
                                          processingLevel = processingLevel,
                                          overwrite = overwrite)


  # Environmental Effects ---------------------------------------------------
  envEffects <- enviromentalEffects(geometry = geometry,
                                      version = version,
                                      processingLevel = processingLevel,
                                      ejscreen = ejscreen,
                                      overwrite = overwrite)


  # Climate Impacts ---------------------------------------------------------
  climateData <- climate(geometry = geometry,
                           version = version,
                          processingLevel = processingLevel,
                          overwrite = overwrite)



  # Sensitive populations Factors ---------------------------------------------------
  senPop <- sensitivePopulations(geometry = geometry,
                                   ejscreen = ejscreen,
                                   version = version,
                                   processingLevel = processingLevel,
                                   overwrite = overwrite)

  # Socioeconomic Factors ---------------------------------------------------
  socEco <- socioEconomicFactors(geometry = geometry,
                                    version = version,
                                   acsData = acsData,
                                   ejscreen = ejscreen,
                                    processingLevel = processingLevel,
                                    overwrite = overwrite)


  # merge all datasets on geoid
  # apply function across all features
  dataframes <- list(envExposures,envEffects,climateData,senPop,socEco)

  # compile final component scores
  df <- finalComponentScore(dataframes)

  # write output ------------------------------------------------------------
  write_csv(df,file = paste0("data/output/enviroscreenScore/",processingLevel,"_",version,".csv"))
  return(df)
}
