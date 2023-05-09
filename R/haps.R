#' Generate HAPS values
#'
#' @param filePath : location of HAPS data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data
#'
getHAPS <- function(filePath, geometry, processingLevel, version, overwrite = FALSE){
    # create version dir
  dir <- paste0("data/output/haps/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_haps.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{


    # read in dataset and drop locations with no coordinates
    d1 <- read.csv(filePath)%>%
      dplyr::filter(SITE_X_COORDINATE != 0)

    ### Generate 5 year mean values for all pollutants
    # select columns of interest
    d2 <- d1 %>%
      dplyr::select(APCD_SITE_ID                ,SITE_100414_ESTIM
                    ,SITE_106467_ESTIM                ,SITE_106990_ESTIM
                    ,SITE_18540299_ESTIM                ,SITE_50000_ESTIM
                    ,SITE_56235_ESTIM                ,SITE_71432_ESTIM
                    ,SITE_75070_ESTIM                ,SITE_75218_ESTIM
                    ,SITE_7782505_ESTIM                ,SITE_822060_ESTIM
                    ,SITE_91203_ESTIM                ,SITE_ASC_ESTIM
                    ,SITE_CE_ESTIM
      )%>%
      dplyr::group_by(APCD_SITE_ID)%>%
      dplyr::summarise_all(mean ,na.rm = TRUE)

    ### normalize data based on volume of emission
    d2[,2:15] <- apply(d2[,2:15], MARGIN = 2, FUN = normalizeVector)
    ### calculate total
    d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)
    ### drop all non poluting sites
    d2 <- d2[d2$total != 0, ]


    ### create spatial feature based on sites of interest
    sp1 <- d1 %>%
      dplyr::left_join(d2, by = "APCD_SITE_ID")%>%
      dplyr::filter(APCD_SITE_ID %in% d2$APCD_SITE_ID)%>%
      distinct(APCD_SITE_ID, .keep_all = TRUE)%>%
      dplyr::select("APCD_SITE_ID",   # rename for input into buffer process
                    "SITE_X_COORDINATE",
                    "SITE_Y_COORDINATE")%>%
      st_as_sf(.,coords=c("SITE_X_COORDINATE","SITE_Y_COORDINATE"),crs=4269)%>%
      sf::st_transform(crs = 5070) %>%
      st_make_valid()

    # reproject data to the espg:5070
    geom <- geometry %>%
      st_transform(crs = 5070) %>%
      dplyr::select(GEOID)

    # running buffering process.
    b1 <- bufferObjects(bufferFeature = sp1,
                        g2 = geom,
                        dist = seq(250, 1000, by = 250),
                        weight = c(1, 0.5, 0.2, 0.1))

    # select total score
    d3 <- d2 %>%
      select(APCD_SITE_ID, total)

    # select the top score value only, combine with site score, and summarize
    bufferFeature_scores <- b1 %>%
      arrange(desc(weight)) %>% # ensures highest score is kept
      dplyr::distinct(GEOID, APCD_SITE_ID, .keep_all = TRUE) %>%
      dplyr::left_join(y = d3, by = "APCD_SITE_ID")%>%
      dplyr::mutate(value = weight * total)%>%
      group_by(GEOID) %>%
      summarise(bufferFeature_score = sum(value, na.rm = TRUE)) %>%
      as.data.frame() %>%
      dplyr::select(GEOID, bufferFeature_score)

    # join back to geometry object to get full list of features

    ### attached GEOID to all points
    geom <- st_drop_geometry(geometry) %>%
      dplyr::left_join(y = bufferFeature_scores,by = "GEOID")%>%
      dplyr::mutate(
        HAPS = case_when(
          is.na(bufferFeature_score) ~ 0,
          TRUE ~ bufferFeature_score
        )
      )%>%
      dplyr::select(GEOID, HAPS)
    write_csv(x = geom,file = file)
  }
  return(geom)
}
