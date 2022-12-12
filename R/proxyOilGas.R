#' Generate oil and gas values
#'
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data
#'
getProxyOilGas <- function(geometry, processingLevel,version, overwrite){

  # create version dir
  dir <- paste0("data/output/oilAndGas/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  file <- paste0(dir, "/",processingLevel,"_proxyOilGas.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{

    # aggregate multiple features into a single object
    # oil and gas locations
    d1 <- st_read("data/input/oilAndGas/Oil_and_Gas_Locations.shp")%>%
      dplyr::filter(fac_status == "AC")%>%
      dplyr::mutate(class = "oilGasLocs", id = loc_id )%>%
      dplyr::select(class, id)
    #pits
    d2 <- st_read("data/input/oilAndGas/Pits.shp")%>%
      dplyr::filter(Facil_Stat == "AC")%>%
      dplyr::mutate(class = "pits", id = Facil_Id )%>%
      dplyr::select(class, id)
    # tanks
    d3 <- st_read("data/input/oilAndGas/Tank_Batteries.shp")%>%
      dplyr::filter(fac_status == "AC") %>%
      dplyr::mutate(class = "tanks", id = fac_id  )%>%
      dplyr::select(class, id)
    # well spots
    d4 <- st_read("data/input/oilAndGas/Wells.shp")%>%
      dplyr::filter(Facil_Stat %in% c("AC","CM","DG","IJ", "PR", "RC","SI","TA","WO"))%>%
      dplyr::mutate(class = "wells", id = Facil_Id )%>%
      dplyr::select(class, id)
    # spills
    d5a <- st_read("data/input/oilAndGas/Spills.csv", options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"))%>%
      dplyr::filter(!is.na(Date.of.Discovery))

    d5 <- d5a %>%
      dplyr::mutate(date = mdy(Date.of.Discovery), class = "spills", id = seq(1, nrow(d5a), 1))%>%
      dplyr::filter(date >= mdy("01/01/2016") & date <= mdy("12/31/2021"))%>%
      dplyr::select(class, id)

    st_crs(d5) <- st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
    d5 <- st_transform(x = d5, crs = st_crs(d4))

    # combine into single feature
    sp <- bind_rows(d1,d2,d3,d4,d5)

    # Intersection and buffer process -----------------------------------------
    geom2 <- st_transform(geometry, crs = st_crs(5070))%>% select(GEOID)
    d6 <- st_transform(sp, crs = st_crs(5070))
    # running buffering process.
    ## need to process through for loop for  memories concerns for census block group
    if(processingLevel == "censusBlockGroup"){
      # run process with 100 features at a time
      seq1 <- c(seq(100, nrow(d6), 100), nrow(d6))
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
        b1 <- bufferObjects(bufferFeature = d6[start:end,],
                            g2 = geom2,
                            dist = seq(250, 1000, by = 250),# reversing the order
                            weight = c(1,0.5,0.2,0.1 )
        )
        # select the top score value only, combine with site score, and summarize
        metrics <- b1 %>%
          arrange(desc(weight)) %>% # ensures highest score is kept
          dplyr::distinct(GEOID, id, .keep_all = TRUE)%>%
          group_by(GEOID) %>%
          summarise(bufferFeature_score = sum(weight, na.rm = TRUE)) %>%
          dplyr::select(GEOID, proxyOilGas = bufferFeature_score)
        #compile Dataframe
        if(i == 1){
          m2 <- metrics
        }else{
          m2 <- bind_rows(m2, metrics)
        }
      }
      geom <- left_join(st_drop_geometry(geom2), m2, by = "GEOID")%>%
        dplyr::mutate(
          proxyOilGas = case_when(
            is.na(proxyOilGas) ~ 0,
            TRUE ~ proxyOilGas
          )
        )%>% dplyr::group_by(GEOID)%>%
        dplyr::summarise(proxyOilGas = sum(proxyOilGas))


      write_csv(geom, file = file)
    }else{

      b1 <- bufferObjects(bufferFeature = d6,
                          g2 = geom2,
                          dist = seq(250, 1000, by = 250),# reversing the order
                          weight = c(1,0.5,0.2,0.1 )
      )

      # select the top score value only, combine with site score, and summarize
      metrics <- b1 %>%
        arrange(desc(weight)) %>% # ensures highest score is kept
        dplyr::distinct(GEOID, id, .keep_all = TRUE)%>%
        group_by(GEOID) %>%
        summarise(bufferFeature_score = sum(weight, na.rm = TRUE)) %>%
        dplyr::select(GEOID, proxyOilGas = bufferFeature_score)
      rm(b1)

      geom <- left_join(st_drop_geometry(geom2), metrics, by = "GEOID")%>%
        dplyr::mutate(
          proxyOilGas = case_when(
            is.na(proxyOilGas) ~ 0,
            TRUE ~ proxyOilGas
          )
        )
      write_csv(geom, file = file)
    }
  }
  return(geom)
}
