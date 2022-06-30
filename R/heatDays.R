#' Generate heat day values
#'
#' @param filePath : location of hearday data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

getHeatDays <- function(filePath, geometry, processingLevel, version, overwrite){
  # create version dir
  dir <- paste0("data/output/heatDays/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_heatDays.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{
    # calculate the average number of extreme heat days between 2015 and 2019
    d1 <- read_csv(filePath)%>%
      dplyr::group_by(CensusTract)%>%
      dplyr::summarise(aveHeatDays = mean(Value))

    # apply measured values to geometry
    ### select processing level by comparing length of GEOID between objects
    if(nchar(geometry$GEOID[1]) >= nchar(d1$CensusTract[1])){
      #add tract-level column to use as join then keep original geoid (tract or block)
      geom <- st_drop_geometry(geometry)%>%
        dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) %>%
        dplyr::left_join(d1, by = c("GEOID2" = "CensusTract"))%>%
        dplyr::select(GEOID, aveHeatDays = aveHeatDays)
    }else{
      # when geometry is county level.. just cut FIPS to county level and group by that
      geom <-  d1 %>%
        dplyr::mutate(GEOID = str_sub(CensusTract, start = 1, end = 5)) %>%
        dplyr::group_by(GEOID) %>%
        dplyr::summarise(aveHeatDays = mean(aveHeatDays, na.rm = TRUE))
    }
    write_csv(x = geom,file = file)
  }
  return(geom)
}
