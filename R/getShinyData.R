###
# compile multiple geography levels into single file for upload to the shiny app
# 20211129
# carverd@colostate.edu
###

getShinyData <- function(removeNativeLand, removeZeroPop, version, spanish){
  ###
  # takes outputs from the enviroscreen scoring process and combines and renames
  # for use in the shiny app
  ###

  # grab total population from ACS data
  acsData <- list.files(path = paste0("data/output/acs/",version), pattern = ".csv", full.names = TRUE)%>%
    purrr::map(read_csv)%>%
    purrr::reduce(rbind)%>%
    dplyr::select("GEOID","Total Population"= "total_Population")

  # read in spatial data
  county <- sf::st_read("data/output/spatialLayers/county/coloradoCounties.geojson")%>%
    dplyr::mutate(area = "County",
                  areaSpanish = "Condado",
                  name = paste0(NAME, " County"))%>%
    dplyr::select(GEOID, "cNAME" = NAME,name, area,areaSpanish)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()


    # grap county name to attached to each lower geometry
  countyName <- county %>% st_drop_geometry() %>% dplyr::select(GEOID, cNAME)
  county <- dplyr::select(county, -"cNAME")
  # census tract
  censusTract <- sf::st_read("data/output/spatialLayers/censusTracts/coloradoCensusTracts.geojson")%>%
    dplyr::mutate(area = "Census Tract",
                  areaSpanish = "área census",
                  geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0(cNAME, " County"))%>%
    dplyr::select(GEOID, name, area,areaSpanish)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()


  # census block group
  censusBlockGroup <- sf::st_read("data/output/spatialLayers/censusBlockGroups/coloradoCensusBlockGroups.geojson")%>%
    dplyr::mutate(area = "Census Block Group",
                  areaSpanish = "Grupo de manzanas censales",
                  geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0( cNAME, " County"))%>%
    dplyr::select(GEOID, name, area,areaSpanish)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()

  ### compile names based on the county relationship

  # read in enviroscreen score files
  c_data <- read_csv(paste0("data/output/enviroscreenScore/county_",version,".csv"))%>%
    dplyr::left_join(county, by = "GEOID")
  ct_data <- read_csv(paste0("data/output/enviroscreenScore/censusTract_",version,".csv"))%>%
    dplyr::left_join(censusTract, by = "GEOID")
  cbg_data <- read_csv(paste0("data/output/enviroscreenScore/censusBlockGroup_",version,".csv"))%>%
    dplyr::left_join(censusBlockGroup, by = "GEOID")


  # join all features
  df <- dplyr::bind_rows(c_data, ct_data, cbg_data)

  # remove native lands
  if(removeNativeLand == TRUE){
    censusTractsNative <- c("08083941100","08067940400", "08067940300", "08007940400")
    # using grep and for loop to capture all census block groups while providing
    # only the track indexes.
    features <- c()
    for(i in seq_along(censusTractsNative)){
      features <- c(features, grep(pattern =  censusTractsNative[i], x = df$GEOID))
    }
    df <- df[-features, ]
  }

  # remove zero population areas
  if(removeZeroPop == TRUE){
    features <- acsData %>%
      dplyr::filter(`Total Population` == 0)%>%
      pull(var = GEOID)
    features <- c()
    df <- df[!df$GEOID %in% features, ]
  }

  if(isFALSE(spanish)){
    # select to define order and rename features
    df <- df %>%
      dplyr::select(
        "GEOID"
        ,"County Name" = "name"
        ,"EnviroScreen Score"=  "finalScore"
        ,"EnviroScreen Score Percentile"="finalScore_Pctl"
        ,"Pollution and Climate Burden Score"="pollClimBurden"
        ,"Pollution and Climate Burden Score Percentile"= "pollClimBurden_Pctl"
        ,"Pollution and Climate Burden Scale Value"= "scaledpollClimate"
        ,"Health and Social Factors Score"="popCharacteristic"
        ,"Health and Social Factors Score Percentile"="popCharacteristic_Pctl"
        ,"Health and Social Factors Scale Value" =  "scaledPopChar"
        ,"Environmental Exposures Score"="envExp"
        ,"Environmental Effects Score"="envEff"
        ,"Climate Vulnerability Score"="climate"
        ,"Sensitive Populations Score"="senPop"
        ,"Demographics Score"="socEco"
        ,"Environmental Exposures Score Percentile"="envExp_Pctl"
        ,"Environmental Effects Score Percentile"="envEff_Pctl"
        ,"Climate Vulnerability Score Percentile"="climate_Pctl"
        ,"Sensitive Populations Score Percentile"="senPop_Pctl"
        ,"Demographics Score Percentile"="socEco_Pctl"
        # exposures
        ,"Ozone"="ozone"
        ,"Ozone Percentile"="ozone_pcntl"
        ,"Fine particle pollution"="pm25"
        ,"Fine particle pollution Percentile"="pm25_pcntl"
        ,"Lead exposure risk"="leadPaint"
        ,"Lead exposure risk Percentile"="leadPaint_pcntl"
        ,"Diesel particulate matter (PM)"="deiselPM"
        ,"Diesel particulate matter (PM) Percentile"="deiselPM_pcntl"
        ,"Traffic proximity & volume"="trafficeProx"
        ,"Traffic proximity & volume Percentile"="trafficeProx_pcntl"
        ,"Air toxics emissions"="HAPS"
        ,"Air toxics emissions Percentile"="HAPS_pcntl"
        ,"Other air pollutants" = "otherHAPS"
        ,"Other air pollutants Percentile" = "otherHAPS_pcntl"
        ,"Drinking water regulations" = "drinkingWater"
        ,"Drinking water regulations Percentile"= "drinkingWater_pcntl"
        ,"Noise" = "noiseLevel"
        ,"Noise Percentile" = "noiseLevel_pcntl"
        # effects
        ,"Wastewater discharge indicator"="waterDischarge"
        ,"Wastewater discharge indicator Percentile"="waterDischarge_pcntl"
        ,"Proximity to National Priorities List sites"="nplProx"
        ,"Proximity to National Priorities List sites Percentile"="nplProx_pcntl"
        ,"Proximity to Risk Management Plan sites"="rmpProx"
        ,"Proximity to Risk Management Plan sites Percentile"="rmpProx_pcntl"
        ,"Proximity to hazardous waste facilities" ="tsdfProx"
        ,"Proximity to hazardous waste facilities Percentile"="tsdfProx_pcntl"
        ,"Proximity to oil and gas" = "proxyOilGas"
        ,"Proximity to oil and gas Percentile" = "proxyOilGas_pcntl"
        ,"Proximity to mining locations" = "mining"
        ,"Proximity to mining locations Percentile" = "mining_pcntl"
        ,"Impaired streams and rivers" = "surfaceWater"
        ,"Impaired streams and rivers Percentile" = "surfaceWater_pcntl"
        # climate
        ,"Wildfire risk"="wildfire"
        ,"Wildfire risk Percentile"="wildfire_pcntl"
        ,"Floodplains"="floodplainPercent"
        ,"Floodplains Percentile"="floodplainPercent_pcntl"
        ,"Drought"="drought"
        ,"Drought Percentile"="drought_pcntl"
        ,"Extreme heat days"="aveHeatDays"
        ,"Extreme heat days Percentile"="aveHeatDays_pcntl"
        # populations
        ,"Population under 5"="under5"
        ,"Population under 5 Percentile"="under5_pcntl"
        ,"Population over 64"=  "over64"
        ,"Population over 64 Percentile"= "over64_pcntl"
        ,"Heart disease in adults"="heartDisease"
        ,"Heart disease in adults Percentile"="heartDisease_pcntl"
        ,"Asthma hospitalization rate"="asthma"
        ,"Asthma hospitalization rate Percentile"="asthma_pcntl"
        ,"Life expectancy"="lifeExpectancy"
        ,"Life expectancy Percentile"="lifeExpectancy_pcntl"
        ,"Low birth weight"="lowBirthWeight"
        ,"Low birth weight Percentile"="lowBirthWeight_pcntl"
        ,"Cancer prevalence"="cancer"
        ,"Cancer prevalence Percentile"="cancer_pcntl"
        ,"Diabetes prevalence"="diabetes"
        ,"Diabetes prevalence Percentile"="diabetes_pcntl"
        ,"Mental health indicator"="mentalHealth"
        ,"Mental health indicator Percentile"="mentalHealth_pcntl"
        # SocEco
        ,"Percent people of color"="peopleOfColor"
        ,"Percent people of color Percentile" = "peopleOfColor_pcntl"
        ,"Percent less than high school education"="highSchool"
        ,"Percent less than high school education Percentile"= "highSchool_pcntl"
        ,"Percent low income"="percent_lowincome"
        ,"Percent low income Percentile"="percent_lowincome_pcntl"
        ,"Percent linguistic isolation"= "percent_lingiso"
        ,"Percent linguistic isolation Percentile"="percent_lingiso_pcntl"
        ,"Percent disability"="percent_disability"
        ,"Percent disability Percentile"="percent_disability_pcntl"
        ,"Housing cost burdened" = "HH_Burdened_Pct"
        ,"Housing cost burdened Percentile" = "HH_Burdened_Pct_pcntl"
        ,"area"
        ,"geometry"

      )

    # convert to an sf object
    df <- df %>%
      mutate(across(where(is.numeric), round, digits=2))%>%
      sf::st_as_sf()

    # add label for Coal, oil/gas, rural, justice 40, and di community -------------------------------------
    coal <- readRDS("data/shinyContent/coalCommunity.rds")%>%
      dplyr::select("GEOID","Coal Community" = "coal")%>%
      st_drop_geometry()
    og <- readRDS("data/shinyContent/oilgasCommunity.rds")%>%
      dplyr::select("GEOID","Oil and Gas Community" = "oilGas")%>%
      st_drop_geometry()
    rural <- readRDS("data/shinyContent/ruralCommunity.rds")%>%
      dplyr::select("GEOID","Rural" = "rural")%>%
      st_drop_geometry()
    justice40 <- readRDS("data/shinyContent/justice40.rds") %>%
      dplyr::select("GEOID","Justice40" = "Identified.as.disadvantaged")%>%
      st_drop_geometry()
    diCommunity <- readRDS("data/shinyContent/diCommunities.rds")%>%
      dplyr::select("GEOID","Disproportionately Impacted Community" = "DI_community")%>%
      dplyr::mutate("Disproportionately Impacted Community" = case_when(
        `Disproportionately Impacted Community` == 1 ~ TRUE
      )
      )%>%
      st_drop_geometry()

    # county level joins
    df$GEOID2 <- str_sub(df$GEOID, 1,5)
    df <- dplyr::left_join(x = df ,y = coal, by = c("GEOID2" = "GEOID"))%>%
      dplyr::left_join(y = og, by = c("GEOID2" = "GEOID"))%>%
      dplyr::left_join(y = rural, by = c("GEOID2" = "GEOID"))%>%
      dplyr::select(-"GEOID2")

    # census tract level joins
    df$GEOID3 <- str_sub(df$GEOID, 1,11)
    df <- dplyr::left_join(x = df ,y = justice40, by = c("GEOID3" = "GEOID"))

    # census block group join
    df <- dplyr::left_join(x = df ,y = diCommunity, by = c("GEOID" = "GEOID"))

    #Join Population Data
    df <- dplyr::left_join(x = df, y = acsData, by = "GEOID")

    # rdata delete_dsn
    saveRDS(df, file = paste0("data/shinyContent/allScores_",version,".rds"))
  }else{
    # select to define order and rename features
    df <- df %>%
      dplyr::select(
        "GEOID"
        ,"Nombre del condado" = "name"
        ,"Puntaje de Colorado EnviroScreen"=  "finalScore"
        ,"Percentil del puntaje de Colorado EnviroScreen"="finalScore_Pctl"
        ,"Contaminación y carga climática"="pollClimBurden"
        ,"Percentil de contaminación y carga climática"= "pollClimBurden_Pctl"
        ,"Pollution and Climate Burden Scale Value"= "scaledpollClimate"
        ,"Factores de salud y sociales"="popCharacteristic"
        ,"Percentil de factores de salud y sociales"="popCharacteristic_Pctl"
        ,"Health and Social Factors Scale Value" =  "scaledPopChar"
        ,"Exposiciones ambientales"="envExp"
        ,"Efectos ambientales"="envEff"
        ,"Vulnerabilidad climática"="climate"
        ,"Poblaciones sensibles"="senPop"
        ,"Características demográficas"="socEco"
        ,"Percentil de exposiciones ambientales"="envExp_Pctl"
        ,"Percentil de efectos ambientales"="envEff_Pctl"
        ,"Percentil de vulnerabilidad climática"="climate_Pctl"
        ,"Percentil de poblaciones sensibles"="senPop_Pctl"
        ,"Percentil de características demográficas"="socEco_Pctl"
        # exposures
        ,"Ozono"="ozone"
        ,"Percentil de ozono"="ozone_pcntl"
        ,"Contaminación por partículas finas"="pm25"
        ,"Percentil de contaminación por partículas finas"="pm25_pcntl"
        ,"Riesgo de exposición al plomok"="leadPaint"
        ,"Percentil de riesgo de exposición al plomo"="leadPaint_pcntl"
        ,"Material particulado (PM) de diésel"="deiselPM"
        ,"Percentil de material particulado (PM) de diésel"="deiselPM_pcntl"
        ,"Proximidad y volumen de tráfico"="trafficeProx"
        ,"Percentil de proximidad y volumen de tráfico"="trafficeProx_pcntl"
        ,"Emisiones de contaminantes tóxicos del aire"="HAPS"
        ,"Percentil de emisiones de contaminantes tóxicos del aire"="HAPS_pcntl"
        ,"Otros contaminantes del aire" = "otherHAPS"
        ,"Percentil de otros contaminantes del aire" = "otherHAPS_pcntl"
        ,"Reglamentos sobre agua potable" = "drinkingWater"
        ,"Percentil de reglamentos sobre agua potable"= "drinkingWater_pcntl"
        ,"Ruido" = "noiseLevel"
        ,"Percentil de ruido" = "noiseLevel_pcntl"
        # effects
        ,"Indicador de descargas de aguas residuales"="waterDischarge"
        ,"Percentil del indicador de descargas de aguas residuales"="waterDischarge_pcntl"
        ,"Proximidad a los sitios de la Lista Nacional de Prioridades"="nplProx"
        ,"Percentil de proximidad a los sitios de la Lista Nacional de Prioridades"="nplProx_pcntl"
        ,"Proximidad a los sitios del Plan de Gestión de Riesgos"="rmpProx"
        ,"Percentil de proximidad a los sitios del Plan de Gestión de Riesgos"="rmpProx_pcntl"
        ,"Proximidad a instalaciones de residuos peligrosos" ="tsdfProx"
        ,"Percentil de proximidad a instalaciones de residuos peligrosos"="tsdfProx_pcntl"
        ,"Proximidad a petróleo y gas" = "proxyOilGas"
        ,"Percentil de proximidad a petróleo y gas" = "proxyOilGas_pcntl"
        ,"Proximidad a minas" = "mining"
        ,"Percentil de proximidad a minas" = "mining_pcntl"
        ,"Arroyos y ríos deteriorados" = "surfaceWater"
        ,"Percentil de arroyos y ríos deteriorados" = "surfaceWater_pcntl"
        # climate
        ,"Riesgo de incendios forestales"="wildfire"
        ,"Percentil de riesgo de incendios forestales"="wildfire_pcntl"
        ,"Inundación (planicies aluviales)"="floodplainPercent"
        ,"Percentil de inundación (planicies aluviales)"="floodplainPercent_pcntl"
        ,"Sequía"="drought"
        ,"Percentil de sequía"="drought_pcntl"
        ,"Días de calor extremo"="aveHeatDays"
        ,"Percentil de días de calor extremo"="aveHeatDays_pcntl"
        # populations
        ,"Población por debajo de 5 años"="under5"
        ,"Percentil de población por debajo de 5 años"="under5_pcntl"
        ,"Población por encima de 64 años"=  "over64"
        ,"Percentil de población por encima de 64 años"= "over64_pcntl"
        ,"Enfermedades cardiacas en adultos"="heartDisease"
        ,"Percentil de enfermedades cardiacas en adultos"="heartDisease_pcntl"
        ,"Tasa de hospitalización por asma"="asthma"
        ,"Percentil de tasa de hospitalización por asma"="asthma_pcntl"
        ,"Expectativa de vida"="lifeExpectancy"
        ,"Percentil de expectativa de vida"="lifeExpectancy_pcntl"
        ,"Bajo peso al nacer"="lowBirthWeight"
        ,"Percentil de bajo peso al nacer"="lowBirthWeight_pcntl"
        ,"Prevalencia de cáncer"="cancer"
        ,"Percentil de prevalencia de cáncer"="cancer_pcntl"
        ,"Prevalencia de diabetes"="diabetes"
        ,"Percentil de prevalencia de diabetes"="diabetes_pcntl"
        ,"Indicador de salud mental"="mentalHealth"
        ,"Percentil del indicador de salud mental"="mentalHealth_pcntl"
        # SocEco
        ,"Porcentaje de personas de color"="peopleOfColor"
        ,"Percentil del porcentaje de personas de color" = "peopleOfColor_pcntl"
        ,"Porcentaje que no completaron los estudios de secundaria"="highSchool"
        ,"Percentil del porcentaje que no completaron los estudios de secundaria"= "highSchool_pcntl"
        ,"Porcentaje de bajos ingresos"="percent_lowincome"
        ,"Percentil del porcentaje de bajos ingresos"="percent_lowincome_pcntl"
        ,"Porcentaje de aislamiento lingüístico"= "percent_lingiso"
        ,"Percentil del porcentaje de aislamiento lingüístico"="percent_lingiso_pcntl"
        ,"Porcentaje de discapacidades"="percent_disability"
        ,"Percentil del porcentaje de discapacidades"="percent_disability_pcntl"
        ,"Sobrecarga por gastos de vivienda" = "HH_Burdened_Pct"
        ,"Percentil de sobrecarga por gastos de vivienda" = "HH_Burdened_Pct_pcntl"
        ,"area" = "areaSpanish"
        ,"geometry"
      )

    # convert to an sf object
    df <- df %>%
      mutate(across(where(is.numeric), round, digits=2))%>%
      sf::st_as_sf()

    # add label for Coal, oil/gas, rural, justice 40, and di community -------------------------------------
    coal <- readRDS("data/shinyContent/coalCommunity.rds")%>%
      dplyr::select("GEOID","Comunidad con carbón" = "coal")%>%
      st_drop_geometry()
    og <- readRDS("data/shinyContent/oilgasCommunity.rds")%>%
      dplyr::select("GEOID","Comunidad con petróleo y gas" = "oilGas")%>%
      st_drop_geometry()
    rural <- readRDS("data/shinyContent/ruralCommunity.rds")%>%
      dplyr::select("GEOID","Comunidad rural" = "rural")%>%
      st_drop_geometry()
    justice40 <- readRDS("data/shinyContent/justice40.rds") %>%
      dplyr::select("GEOID","Comunidad de Justice40" = "Identified.as.disadvantaged")%>%
      st_drop_geometry()
    diCommunity <- readRDS("data/shinyContent/diCommunities.rds")%>%
      dplyr::select("GEOID","Comunidad afectada de manera desproporcionada" = "DI_community")%>%
      dplyr::mutate("Comunidad afectada de manera desproporcionada" = case_when(
        `Comunidad afectada de manera desproporcionada` == 1 ~ TRUE
      )
      )%>%
      st_drop_geometry()

    # county level joins
    df$GEOID2 <- str_sub(df$GEOID, 1,5)
    df <- dplyr::left_join(x = df ,y = coal, by = c("GEOID2" = "GEOID"))%>%
      dplyr::left_join(y = og, by = c("GEOID2" = "GEOID"))%>%
      dplyr::left_join(y = rural, by = c("GEOID2" = "GEOID"))%>%
      dplyr::select(-"GEOID2")

    # census tract level joins
    df$GEOID3 <- str_sub(df$GEOID, 1,11)
    df <- dplyr::left_join(x = df ,y = justice40, by = c("GEOID3" = "GEOID"))

    # census block group join
    df <- dplyr::left_join(x = df ,y = diCommunity, by = c("GEOID" = "GEOID"))

    #Join Population Data
    names(acsData) <- c("GEOID", "Total de la población")
    df <- dplyr::left_join(x = df, y = acsData, by = "GEOID")

    # rdata delete_dsn
    saveRDS(df, file = paste0("data/shinyContent/allScores_",version,"_spanish.rds"))
  }
}


