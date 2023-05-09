#' Generate wildfire values
#'
#' @param filePath : location of wildfire data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

getWildfire <- function(filePath, geometry, processingLevel, version, overwrite){
  # create version dir
  dir <- paste0("data/output/wildfire/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_wildfire.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{
    # terra implementation
    d1 <- terra::rast(filePath)

    # read in geometry features
    g1 <- geometry %>%
      sf::st_transform(crs = terra::crs(d1))%>%
      dplyr::select(GEOID)

    # crop-
    d1 <- d1 %>%
      terra::crop(g1)

    # extract values
    # assigning sf as spatvect for function, spatvect does not support same transformation as sf objects.
    g2 <- terra::extract(d1, terra::vect(g1),fun = mean, na.rm = TRUE)
    # assign values to GEOID
    geom <- g1 %>%
      dplyr::mutate(
        wildfire = g2$whp2020_cnt_conus
      )%>%
      as.data.frame()%>%
      dplyr::select(GEOID, wildfire)
    # write
    write_csv(x = geom,file = file)
  }
  return(geom)
}
