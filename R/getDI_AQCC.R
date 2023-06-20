# AQCC DI Community Definition
#
# When you click, display “Socioeconomically Vulnerable Community”
#   40% people of color
#   40% low-income
#   50% housing cost-burden
#   20% linguistic isolation
# When you click, display “Cumulatively Impacted Community”
#   Greater than (not greater than or equal to) 80th percentile Colorado EnviroScreen score

getDI_AQCC <- function(removeNativeLand, overwrite){
  # overwrite defines if you want to force the file to be recreated.

  pathToData <- "data/shinyContent/diCommunities_AQCC.rds"

  if(file.exists(pathToData) & overwrite == FALSE){
    return(paste0("The DI community spatial data exists and can be found ", pathToData))
  }else{

    bgscores <- read.csv(paste0("data/output/enviroscreenScore/censusBlockGroup_", version, ".csv")) %>%
      mutate(GEOID = paste0("0", as.character(GEOID)))%>%
      select(GEOID, EnviroScreen_Pctl = finalScore_Pctl)

    bgscores80 <- bgscores %>%
      filter(EnviroScreen_Pctl > 80)


    bg_co <- get_acs(geography = "block group",
                     variables = c("B01001_001", # total pop, age&sex
                                   "C17002_001", # total pop whose income to poverty ratio was determined
                                   "C17002_002", # total pop under 200% FPL (next few rows)
                                   "C17002_003",
                                   "C17002_004",
                                   "C17002_005",
                                   "C17002_006",
                                   "C17002_007",
                                   "C17002_008", # total pop at or above 200% FLP
                                   "B03002_001", # total pop, race
                                   "B03002_003", # population - white alone, non-Hispanic
                                   "B25070_001", # Total Renters
                                   "B25070_007", # 30 to 34.9%
                                   "B25070_008", # 35 to 39.9%
                                   "B25070_009", # 40 to 49.9%
                                   "B25070_010", # 50% or more
                                   "B25091_001", # total owner-occupied,
                                   # "B25003_002", # confirmation of previous var - total owner occupied,
                                   "B25091_008", # 30 to 34.9% - mortgaged
                                   "B25091_009", # 35 to 39.9% - mortgaged
                                   "B25091_010", # 40 to 49.9% - mortgaged
                                   "B25091_011", # 50% or more - mortgaged
                                   "B25091_019", # 30 to 34.9% - not mortgaged
                                   "B25091_020", # 35 to 39.9% - not mortgaged
                                   "B25091_021", # 40 to 49.9% - not mortgaged
                                   "B25091_022", # 50% or more - not mortgaged
                                   "B25002_002", # total occupied units
                                   #Percent linguistic isolation
                                   "C16002_001",
                                   "C16002_004",
                                   "C16002_007",
                                   "C16002_010",
                                   "C16002_013"),

                     state = "08",
                     year = 2019)%>%
      select(-moe) %>%
      spread(key = variable, value = estimate)%>%
      mutate(TotalPop = B01001_001,
             WhitePop = B03002_003, # White, non-Hispanic population
             MinPop = TotalPop-WhitePop, # all people that are not white, non-Hispanic
             Pov_PCT =  (C17002_001 - C17002_008)/C17002_001, # % of pop < 200% FPL (whose income:poverty could be determined)
             Min_PCT = MinPop/TotalPop, # percent of population that are non-white
             Min_FLAG = ifelse(Min_PCT > .4, 1, 0), # DI community flags
             FLP_FLAG = ifelse(Pov_PCT > .4, 1, 0),
             HHUnits = B25070_001+B25091_001, # renter total + owner total
             HH_Burdened = B25070_007+B25070_008+B25070_009+B25070_010+
               B25091_008+B25091_009+B25091_010+B25091_011+
               B25091_019+B25091_020+B25091_021+B25091_022, # >30% renters, mortgaged, nonmortgaged
             HH_Burdened_Pct = HH_Burdened/HHUnits,
             Burdened_FLAG = ifelse(HH_Burdened_Pct > .5, 1, 0),
             LingIso_PCT = (C16002_004+C16002_007+C16002_010+C16002_013) / C16002_001,
             LingIso_FLAG = ifelse(LingIso_PCT > .2, 1, 0),
             Score_FLAG = ifelse(GEOID %in% bgscores80$GEOID, 1, 0))%>%
      dplyr::select(GEOID,Min_PCT,Min_FLAG,Pov_PCT,FLP_FLAG,HH_Burdened_Pct,
                    Burdened_FLAG, LingIso_PCT, LingIso_FLAG, Score_FLAG)%>%
      dplyr::rowwise()%>%
      dplyr::mutate(
        DI_communityCount = sum(c(Min_FLAG,
                                  FLP_FLAG,
                                  Burdened_FLAG,
                                  LingIso_FLAG,
                                  Score_FLAG), na.rm = TRUE),
        DI_community = case_when(
          DI_communityCount != 0 ~ 1,
          TRUE ~ 0
        )

      )

    bg_co <- merge(bg_co, bgscores, by = "GEOID", all.x = TRUE) %>%
      select(GEOID,Min_PCT,Min_FLAG,Pov_PCT,FLP_FLAG,HH_Burdened_Pct,Burdened_FLAG,
             LingIso_PCT, LingIso_FLAG, EnviroScreen_Pctl, Score_FLAG, DI_communityCount, DI_community)


    if(removeNativeLand == TRUE){
      censusTractsNative <- c("08083941100","08067940400", "08067940300", "08007940400")
      features <- c()
      for(i in seq_along(censusTractsNative)){
        features <- c(features, grep(pattern =  censusTractsNative[i], x = bg_co$GEOID))
      }
      bg_co <- bg_co[-features, ]
    }



    # read in geometry for census block groups
    geom <- sf::st_read("data/output/spatialLayers/censusBlockGroups/coloradoCensusBlockGroups.geojson")%>%
      dplyr::select(GEOID)%>%
      dplyr::left_join(bg_co, by = "GEOID")%>%
      dplyr::filter(Min_FLAG != 0 | FLP_FLAG != 0 | Burdened_FLAG !=0 |
                      LingIso_FLAG != 0 |Score_FLAG != 0)%>%
      st_transform(crs = st_crs(4326))%>%
      rmapshaper::ms_simplify(keep_shapes = TRUE)

    #write feature
    saveRDS(object = geom, file = pathToData)
    return(paste0("The DI community spatial data was writen ", pathToData))
  }
}
