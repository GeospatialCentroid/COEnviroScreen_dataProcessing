#' Generate lowBirthWeight values
#'
#' @param filePath : location of lowBirthWeight data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data


getLowBirthWeight <- function(filePath, geometry, processingLevel, version, overwrite){
  # create version dir
  dir <- paste0("data/output/lowBirthWeight/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir, "/",processingLevel,"_lowBirthWeight.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{


  # process birthweight data
  d1 <- read_csv(filePath) %>%
    # # set leading zero to match geoID in Geom object
    # dplyr::mutate(GEOID = paste0("0",TRACT_FIPS))%>%
    dplyr::select(GEOID = TRACT_FIPS, LWB_ADJRATE)

  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- st_drop_geometry(geometry) %>%
      mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) %>%
      left_join(d1, by =  c("GEOID2" = "GEOID")) %>%
      dplyr::select(GEOID, lowBirthWeight = LWB_ADJRATE)
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>%
      group_by(GEOID) %>%
      summarise(lowBirthWeight = mean(LWB_ADJRATE, na.rm = TRUE))
  }

  write_csv(x = geom,file = file)
  }
  return(geom)
}


