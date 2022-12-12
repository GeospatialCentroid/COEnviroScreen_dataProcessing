#' Generate noise measure
#'
#' @param filePath : location of noise raster data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data
getNoise <- function(filePath, geometry, processingLevel,version, overwrite){
    # create version dir
  dir <- paste0("data/output/noise/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  # relates to the output file
  file <- paste0(dir, "/",processingLevel,"_noise.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
    }else{
    # extract average value to each features
    r1 <- terra::rast(filePath)
    g2 <- geometry %>%
      dplyr::select("GEOID")

    # convert to terra object
    g3 <- vect(g2)%>%
      terra::project(r1)

    # grab values
    r2 <- terra::extract(r1, g3, mean, na.rm = TRUE)

    # attached GEOID to datasets
    geom <- dplyr::bind_cols(st_drop_geometry(g2), r2) %>%
      dplyr::select(GEOID, noiseLevel = CONUS_L50dBA_sumDay_exi)

    write_csv(x = geom, file = file)
    }
  #output the object
  return(geom)
}
