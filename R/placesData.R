#' Generate placesData values
#'
#' @param filePath : location of placesData data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

getPlacesData <- function(filePath, processingLevel, geometry, version, overwrite = overwrite){

  # create version dir
  dir <- paste0("data/output/CDC_places/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_placesData.csv")

  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{
  d1 <- vroom::vroom(filePath)%>%
    dplyr::filter(StateAbbr == "CO")%>%
    dplyr::filter(
      Measure %in% c("Cancer (excluding skin cancer) among adults aged >=18 years",
                     "Diagnosed diabetes among adults aged >=18 years",
                     "Mental health not good for >=14 days among adults aged >=18 years"))%>%
    dplyr::select(
      GEOID = LocationName,
      Measure,
      Data_Value
    )%>%
    tidyr::spread(key = Measure, Data_Value)
  # process based on geometry level
  i <- nchar(geometry$GEOID[1])
  # processing based on the length of GEOID object
  geom <-  d1 %>%
    dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = i)) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarise(
      cancer = mean(`Cancer (excluding skin cancer) among adults aged >=18 years`,na.rm=TRUE),
      mentalHealth = mean(`Diagnosed diabetes among adults aged >=18 years`,na.rm=TRUE),
      diabetes = mean(`Mental health not good for >=14 days among adults aged >=18 years`,na.rm=TRUE)
    )

  # format for census block group
  if(i == 12){
    geom <- st_drop_geometry(geometry)%>%
      dplyr::mutate(geoid2 = str_sub(GEOID, 1,11))%>%
      dplyr::left_join(geom, by = c("geoid2"= "GEOID"))%>%
      dplyr::select(GEOID, cancer, mentalHealth, diabetes)
  }
  write_csv(x = geom,file = file)
  }
  return(geom)
}


