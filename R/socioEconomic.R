

socioEconomicFactors <- function(geometry, acsData, ejscreen, processingLevel, version, overwrite = FALSE){
  file <- paste0("data/output/enviroscreenScore/", processingLevel,"/socioEconomic_",version,".csv")

  if(!file.exists(file) || overwrite == TRUE){

  # run functions
    cat("ejscreen")
    d1 <- ejscreen %>%
    dplyr::select("GEOID","peopleOfColor","highSchool")

    cat("acsData")
    d2 <- acsData %>%
    dplyr::select("GEOID","percent_lowincome","percent_lingiso","percent_disability")

    cat("housingBurden")
    d3 <- getHousingBurden(processingLevel = processingLevel,
                           geometry = geometry,
                           version = version,
                           overwrite = overwrite)

  # there is not percent disability at census block group, so we need to pull in
  # from the census tract level data
  if(processingLevel == "censusBlockGroup"){
    acs2 <- acs(processingLevel = "censusTract", version = version, overwrite = overwrite)%>%
      dplyr::select(GEOID2 = GEOID, percent_disability)
    d2 <- d2 %>%
      mutate(GEOID2 = substr(GEOID, start = 1, stop = 11))%>%
      dplyr::left_join(acs2, by = c("GEOID2"))%>%
      dplyr::select("GEOID","percent_lowincome","percent_lingiso","percent_disability" ="percent_disability.y")
  }

  #combine datasets
  dataframes <- list(d1,d2,d3)
  df <- joinDataFrames( dataframes)

  # determine the average value across all features
  df$socEco <- df %>%
    select(contains("_pcntl"))%>%
    apply(MARGIN = 1, FUN = gm_mean)
  # write out features
  write_csv(df, file)
  }else{
    df <- read_csv(file)
  }
  return(df)
}
