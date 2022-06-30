#' Generate Drought values
#'
#' @param filePath : location of Drought data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data


getDrought <- function(filePath, processingLevel, geometry, version, overwrite){
  # create version dir
  dir <- paste0("data/output/drought/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_drought.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{

    d1 <- read_csv(filePath)
    # fitler to specific 2016-2020 to ensure full year is present in data
    d2 <- d1 %>%
      dplyr::filter(ValidStart  > as.Date("2016-01-01") & ValidStart  < as.Date("2020-01-01"))%>%
      rowwise() %>%
      mutate(percentArea = sum(D2,D3,D4))
    # calculate average area in drought
    d3 <- d2  %>%
      dplyr::select(FIPS,percentArea)  %>%
      dplyr::group_by(FIPS)%>%
      dplyr::summarise(averageAreaInDrought = mean(percentArea), sumAreaInDrought = sum(percentArea), totalWeeks = n())
    # calculate number of weeks with some drought
    d4 <- d2 %>%
      dplyr::filter(percentArea != 0)%>%
      dplyr::select(FIPS,percentArea)  %>%
      dplyr::group_by(FIPS)%>%
      dplyr::summarise(weeksWithDrought = n())
    # sum values in cat d2 d3 adn d4
    d5 <- dplyr::left_join(d3, d4, by ="FIPS")%>%
      dplyr::mutate(percentTimeInDrought = (weeksWithDrought/totalWeeks)*100)

    # join to goemetry elements
    geom <- st_drop_geometry(geometry)%>%
      dplyr::mutate("FIPS" = str_sub(GEOID, start = 1, end = 5))%>%
      dplyr::left_join(d5, by ="FIPS")%>%
      dplyr::select("GEOID", "averageAreaInDrought", "sumAreaInDrought", "weeksWithDrought", "percentTimeInDrought")

    # utilizing sum area in drought as the indicator
    geom <- geom %>%
      dplyr::select(GEOID, drought = sumAreaInDrought)

    write_csv(x = geom,file = file)
    }
  return(geom)

}
