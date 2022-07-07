#' Generate surface water values
#'
#' @param filePath : character vector referencing input file location
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data


getSurfaceWater <- function(filePath,processingLevel, geometry, version, overwrite){

  # create version dir
  dir <- paste0("data/output/surfaceWater/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  file <- paste0(dir,"/",processingLevel,"_surfaceWater.csv")
  # run the process
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{
    ### Import data
    # 303(d) stream data layer
    streams <- st_read(filePath) %>%
      filter(substr(AUID, 1, 2) == "CO") # removing 4 "Lake" obs

    ### Data wrangling ----
    #   - create new variables for use assignments, assessment status,
    #     impairment status, fully supported status, etc.


    stream_uses <- streams %>%
      mutate( # Uses
        AgUse = ifelse(Ag == "NA", 0, 1),
        AQLifeUse = ifelse(AQLife == "NA", 0, 1),
        RecUse = ifelse(Rec == "NA", 0, 1),
        WSUse = ifelse(WS == "NA", 0, 1),
        TotalUses = AgUse+AQLifeUse+RecUse+WSUse,
        # Impairment
        ImpairedUse = ifelse(X303d_Uses_ > 0, 1, 0),
        ImpairedUse_char = as.character(ImpairedUse),
        PercentUsesImpaired = 100*X303d_Uses_/TotalUses,
        # Assessment status
        AgAssessed = ifelse(Ag == "X"| Ag == "NA", 0, 1),
        AQLifeAssessed = ifelse(AQLife == "X"| AQLife == "NA", 0, 1),
        RecAssessed = ifelse(Rec == "X"| Rec == "NA", 0, 1),
        WSAssessed = ifelse(WS == "X"| WS == "NA", 0, 1),
        TotalAssessed = AgAssessed+AQLifeAssessed+RecAssessed+WSAssessed,
        Assessed = ifelse(TotalAssessed > 0, 1, 0),
        Assessed_char = as.character(Assessed))

    #### Overlay streams and geographic boundaries ----
    geometry <- geometry %>%
      st_transform(crs = st_crs(stream_uses)) %>%
      select("GEOID")


    geom <- data.frame(matrix(nrow = nrow(geometry), ncol = 3))
    names(geom) <- c("GEOID", "AvgPercentImpaired", "PcntUnassessed")
    for(i in seq_along(geometry$GEOID)){
      print(i)
      g1 <- geometry[i, ]
      geom$GEOID[i] <- g1$GEOID

      overlay <- st_intersection(stream_uses, g1) # very slow
      if(nrow(overlay)==0){
        geom$AvgPercentImpaired[i] <- NA
        geom$PcntUnassessed[i] <- NA

      }else{
        overlay$seglength <- st_length(overlay)

        d1 <-  overlay %>%
          st_drop_geometry()%>% # drop stream segment geometry for faster processing.
          mutate(
            #convert segment length in meters to miles
            stream_mi = as.numeric(seglength)*0.000621,

            # Calculate the numerator for average percent impaired:
            # Stream segment length multiplied by the percent of uses impaired.
            # These will be added together for the entire county in the
            # "summarise" step below.
            numerator_impaired = stream_mi*(PercentUsesImpaired/100),

            # Calculate the numerator for percent unassessed
            # Stream segment length for completely unassessed streams.
            # These will be added together for the entire county in the
            # "summarise" step below.
            numerator_completelyunassessed = ifelse(Assessed == 0, stream_mi, 0))%>%
          dplyr::summarise(TotalStreamLengthMi = sum(stream_mi),
                           numerator_impaired = sum(numerator_impaired),
                           numerator_completelyunassessed = sum(numerator_completelyunassessed))%>%
          ## because we are working on single counties we can not apply percent rank function until alfter all geometries
          ## have been resolved.
          mutate(AvgPercentImpaired = numerator_impaired/TotalStreamLengthMi,
                 PcntUnassessed = 100*numerator_completelyunassessed/TotalStreamLengthMi
          )

        geom$AvgPercentImpaired[i] <- d1$AvgPercentImpaired
        geom$PcntUnassessed[i] <- d1$PcntUnassessed

      }
    }

    geom <- geom %>%
      dplyr::mutate(
        ImpairedPctl = percent_rank(AvgPercentImpaired)*100,
        UnassessedPctl = percent_rank(PcntUnassessed)*100,
        CombinedMetric = ImpairedPctl + UnassessedPctl/2) %>%
      dplyr::select(
        "GEOID", "surfaceWater"= "CombinedMetric"
      )

    write_csv(geom, file = file)
  }
  return(geom)
}
