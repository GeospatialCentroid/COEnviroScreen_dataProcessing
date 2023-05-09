#' Generate ozone values
#'
#' @param filePath : location of ozone data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

getOzone <- function(filePath, geometry, processingLevel,version, overwrite){
  # create version dir
  dir <- paste0("data/output/ozone/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_ozone.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{

  # read in dataset - fread for large files
  d1 <- vroom::vroom(filePath) %>%
    dplyr::filter(str_starts(FIPS, "08"))%>%
    dplyr::mutate(Conc = as.numeric(`ozone_daily_8hour_maximum(ppb)`))%>%
    dplyr::group_by(FIPS) %>%
    summarise(ozone_mean = mean(Conc)) %>%
    #rename as tract for calculation below
    dplyr::select(tract = FIPS, ozone_mean)

  #when geometry is tract or block level...
  if(nchar(geometry$GEOID[1]) >= nchar(d1$tract[1])) {
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry) %>%
      dplyr::mutate(tract = str_sub(GEOID, start = 1, end = 11)) %>%
      dplyr::left_join(d1, by = "tract") %>%
      dplyr::select(GEOID, ozone = ozone_mean)
  } else {
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(tract, start = 1, end = 5)) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(ozone = mean(ozone_mean))
  }
  write.csv(x = geom,file = file, row.names=FALSE)
  }
  return(geom)
}
