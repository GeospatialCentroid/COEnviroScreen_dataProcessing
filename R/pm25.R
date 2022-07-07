#' Generate PM2.5 values
#'
#' @param filePath : location of pm 2.5 data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

getPM25 <- function(filePath, geometry, processingLevel,version, overwrite){
  # create version dir
  dir <- paste0("data/output/pm25/",version)
  if(!dir.exists(dir)){
  dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir, "/",processingLevel,"_pm25.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{

  #read in and clean data
  d1 <- vroom::vroom(filePath) %>%
    #filter just colorado tracts
    dplyr::filter(str_starts(FIPS, "08"))%>%
    #concentration was read in as character
    dplyr::mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`)) %>%
    dplyr::group_by(FIPS) %>%
    dplyr::summarise(pm25_mean = mean(Conc)) %>%
    #rename as tract for calculation below
    dplyr::select(tract = FIPS, pm25_mean)


  #when geometry is tract or block level...
  if(nchar(geometry$GEOID[1]) >= nchar(d1$tract[1])) {
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry)%>%
      dplyr::mutate(tract = str_sub(GEOID, start = 1, end = 11))%>%
      dplyr::left_join(d1, by = "tract")%>%
      dplyr::select(GEOID, pm25 = pm25_mean)
  } else {
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(tract, start = 1, end = 5))%>%
      dplyr::group_by(GEOID)%>%
      dplyr::summarise(pm25 = mean(pm25_mean))
  }
  write_csv(x = geom,file = file)
  }
  return(geom)
}
