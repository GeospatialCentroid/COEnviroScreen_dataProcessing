#' Generate other HAPS values
#'
#' @param filePath : location of haps data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data
#'
getOtherHAPS <- function(filePath, geometry, processingLevel,version,overwrite){
  # create version dir
  dir <- paste0("data/output/haps/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_otherHaps.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{

    # read in dataset and drop locations with no coordinates
    d1 <- read_csv(filePath)%>%
      dplyr::filter(SITE_X_COORDINATE != 0)

    ### Generate 5 year median values for all pollutants
    # select columns of interest
    d2 <- d1 %>%
      dplyr::select(
        APCD_SITE_ID, SITE_SO2_ESTIM, SITE_NO2_ESTIM,
        SITE_NOX_ESTIM, SITE_CO_ESTIM, SITE_PM10_ESTIM
      )%>%
      dplyr::group_by(APCD_SITE_ID)%>%
      dplyr::summarise_all(mean ,na.rm = TRUE)

    ### normalize data based on volume of emission
    d2[,2:6] <- apply(d2[,2:6], MARGIN = 2, FUN = normalizeVector)
    ### calculate total
    d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)
    ### drop all non poluting sites
    d2 <- d2[d2$total != 0, ]

    ### create spatial feature based on sites of interest
    sp1 <- d1 %>%
      dplyr::left_join(d2, by = "APCD_SITE_ID")%>%
      dplyr::filter(APCD_SITE_ID %in% d2$APCD_SITE_ID)%>%
      distinct(APCD_SITE_ID, .keep_all = TRUE)%>%
      dplyr::select("APCD_SITE_ID",
                    "total",
                    "SITE_X_COORDINATE",
                    "SITE_Y_COORDINATE")%>%
      st_as_sf(.,coords=c("SITE_X_COORDINATE","SITE_Y_COORDINATE"),crs=4269)

    # Intersection and buffer process -----------------------------------------
    geom2 <- st_transform(geometry, crs = st_crs(5070))%>% select(GEOID)
    d5 <- st_transform(sp1, crs = st_crs(5070))

    if(processingLevel == "censusBlockGroup"){
      # run process with 100 features at a time
      seq1 <- c(seq(100, nrow(d5), 100), nrow(d5))
      for(i in seq_along(seq1)){
        print(i)
        if(i == length(seq1)){
          # accounting for the last position which will have less then 100 features.
          diff <- seq1[i]-seq1[i-1]
          start <- seq1[i] - (diff-1)
        }else{
          start <- seq1[i]-99
        }

        end <- seq1[i]
        b1 <- bufferObjects(bufferFeature = d5[start:end,],
                            g2 = geom2,
                            dist = seq(250, 1000, by = 250),# reversing the order
                            weight = c(1,0.5,0.2,0.1 )
        )
        # select the top score value only, combine with site score, and summarize
        metrics <- b1 %>%
          arrange(desc(weight)) %>% # ensures highest score is kept
          dplyr::distinct(GEOID, APCD_SITE_ID, .keep_all = TRUE)%>%
          mutate(score = total * weight)%>%
          group_by(GEOID) %>%
          summarise(bufferFeature_score = sum(score, na.rm = TRUE)) %>%
          dplyr::select(GEOID, otherHAPS = bufferFeature_score)
        #compile Dataframe
        if(i == 1){
          m2 <- metrics
        }else{
          m2 <- bind_rows(m2, metrics)
        }
      }
      geom <- left_join(st_drop_geometry(geom2), m2, by = "GEOID")%>%
        dplyr::mutate(
          otherHAPS = case_when(
            is.na(otherHAPS) ~ 0,
            TRUE ~ otherHAPS
          )
        )%>% dplyr::group_by(GEOID)%>%
        dplyr::summarise(otherHAPS = sum(otherHAPS))


      write_csv(geom, file = file)
    }else{
      b1 <- bufferObjects(bufferFeature = d5,
                          g2 = geom2,
                          dist = seq(250, 1000, by = 250),# reversing the order
                          weight = c(1,0.5,0.2,0.1 ))

      # select the top score value only, combine with site score, and summarize
      metrics <- b1 %>%
        arrange(desc(weight)) %>% # ensures highest score is kept
        dplyr::distinct(GEOID, APCD_SITE_ID, .keep_all = TRUE)%>%
        mutate(score = total * weight)%>%
        group_by(GEOID) %>%
        summarise(bufferFeature_score = sum(score, na.rm = TRUE)) %>%
        dplyr::select(GEOID, otherHAPS = bufferFeature_score)

      geom <- left_join(st_drop_geometry(geom2), metrics, by = "GEOID")%>%
        dplyr::mutate(
          otherHAPS = case_when(
            is.na(otherHAPS) ~ 0,
            TRUE ~ otherHAPS
          )
        )%>%
        dplyr::group_by(GEOID)%>%
        dplyr::summarise(otherHAPS = sum(otherHAPS))
      write_csv(geom, file = file)
    }
  }
  return(geom)
}
