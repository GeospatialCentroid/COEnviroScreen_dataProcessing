#' Generate drinking water values
#'
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data

  getDrinkingWater <- function(geometry, processingLevel,version, overwrite){
  # create version dir
  dir <- paste0("data/output/drinkingWater/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_drinkingWater.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- read_csv(file)
  }else{
  #### County boundaries ----
  county <- setSpatialData(processingLevel = "county")

  #### All PWS systems ----

  # this is used to calculate the total population served by systems meeting the
  # system specifications above

  allPWS <- read.csv("data/input/drinkingWater/AllPWS_CommunityNTNC.csv", stringsAsFactors = F) %>%
    rename(PWS.ID = PWS.ID..Links.to.Records.)

  # removing water haulers, keeping only Community systems, and calculating pop served per county

  county_popserved <- allPWS %>%
    filter(Water.Hauler == "No",
           Federal.Type == "Community")%>%
    group_by(County) %>%
    summarise(PopServed = sum(Population))

  #### Violation data ----

  # Import violation and lead 90th percentile data and calculate duration for
  # each violation.

  ##### MCLs and other Health-based violations ----

  # MGH downloaded a new file 1/31/2022 meeting the criteria above, 2010-2020

  v <- read.csv("data/input/drinkingWater/Violations2010_2020.csv", stringsAsFactors = F)

  v.2010.2020.duration <- v %>%
    mutate(Year = as.numeric(substr(Begin.Date, nchar(Begin.Date)-3, nchar(Begin.Date))),
           End.Date = as.Date(End.Date, format = "%b %d, %Y"),
           Begin.Date = as.Date(Begin.Date, format = "%b %d, %Y"),
           Unresolved = ifelse(Resolved == "No", 1, 0),
           Duration = as.numeric(difftime(End.Date, Begin.Date, unit = "weeks"))/52.25) %>%
    filter(Year < 2021,
           Year > 2009) %>%
    select(PWS.ID, Name, Violation.Name, Analyte.Name, Unresolved, County, Year, Duration, Begin.Date, End.Date)

  # explore duration lengths
  avg.duration <- v.2010.2020.duration %>% ### note change the feature name here /
    group_by(Unresolved)%>%
    summarise(n(),
              AvgDuration = mean(Duration),
              MinDuration = min(Duration),
              MaxDuration = max(Duration))

  ##### Lead data ----

  # Duration of lead 90th percentile violations is tricky to calculate. Here,
  # we calculate the length of time from one sample date to the next sample date
  # in the case of a result above 15.
  # If the subsequent result is also above the action level, this code calculates
  # the duration of that violation as well.

  Pb <- read.csv("data/input/drinkingWater/Lead90th.csv", stringsAsFactors = F) # all Pb results

  Pb90.duration <- Pb %>%
    rename(Collection.Date = Collection.Date..Date...Time.) %>%
    mutate(DateNew = mdy(Collection.Date),
           Year = year(DateNew),
           AboveActionLevel = ifelse(Measure > 15, 1, 0),
           Violation.Name = "Lead 90th Percentile",
           Analyte.Name = "Lead")%>%
    filter(Year < 2021,
           Year > 2009)%>%
    arrange(DateNew) %>%
    group_by(PWS.ID) %>%
    mutate(Unresolved = case_when(
      DateNew == max(DateNew) & AboveActionLevel == 1 ~ 1,
      AboveActionLevel == 0 ~ NA_real_,
      TRUE ~ 0
    ),
    End.Date = lead(DateNew)) %>%
    ungroup()%>%
    group_by(PWS.ID) %>%
    mutate(Duration = case_when(
      Unresolved == 1 ~ as.numeric(difftime(as.Date("12/31/2020", format = "%m/%d/%Y"), DateNew, units = "weeks"))/52.25,
      Unresolved == 0 ~ as.numeric(difftime(lead(DateNew), DateNew,  units = "weeks"))/52.25,
      TRUE ~ NA_real_
    ))%>%
    ungroup()%>%
    filter(AboveActionLevel == 1) %>%
    select(PWS.ID, Name, Violation.Name, Analyte.Name, Unresolved, County, Year, Duration, Begin.Date = DateNew, End.Date)


  #### Combine violation datasets ----

  AllHBVs.d <- rbind(v.2010.2020.duration, Pb90.duration)


  AllHBVs_pop.d <- merge(AllHBVs.d, allPWS, by = c("PWS.ID")) %>%
    select(PWS.ID, Name=Name.x, Violation.Name, Analyte.Name, Unresolved, Duration, County=County.x, Year, Population)


  topfourexplore <- AllHBVs_pop.d %>%
    filter(County %in% c("PROWERS", "KIT CARSON", "OTERO", "LOGAN"))%>%
    group_by(County, PWS.ID, Name, Population) %>%
    summarise(ViolationCount = n(),
              Duration = sum(Duration))

  # we lost ~200 rows because the water system was not in our complete water
  # system file (active community water systems) and therefore did not meet the
  # specifications.

  # Violation summary

  ViolationSummary.r <- AllHBVs_pop.d %>%
    group_by(PWS.ID, Name, Violation.Name, Analyte.Name) %>%
    summarise(Count = n(),
              Duration = sum(Duration))

  ## Metric calculation ----

  # ok so now for each water system, we want to find the total duration of its
  # violations and weight by population for the county measure.

  System.Duration <- AllHBVs_pop.d %>%
    group_by(PWS.ID, Name, County) %>%
    summarise(ViolationCount = n(),
              Duration = sum(Duration),
              Population = mean(Population)) %>%
    ungroup()%>%
    mutate(numerator = Duration*Population)

  county.duration <- merge(System.Duration, county_popserved, by = "County", all = T)

  # First set NAs to 0 (counties that did not have any violations or affected pop)

  county.duration[is.na(county.duration)] <- 0

  CountyViolations <- county.duration %>%
    group_by(County, PopServed) %>%
    summarise(ViolationCount = sum(ViolationCount),
              WeightedAverage = ifelse(ViolationCount > 0, sum(numerator)/PopServed, NA),
              TotalViolationYears = sum(Duration),
              AffectedPopulation = sum(Population))%>%
    group_by(County, ViolationCount, AffectedPopulation, TotalViolationYears, WeightedAverage) %>%
    summarise(PopServed = mean(PopServed)) %>%
    ungroup() %>%
    mutate(Percentile = percent_rank(WeightedAverage)*100,
           PercentPopulationAffected = 100*AffectedPopulation/PopServed) %>%
    select(County, PopServed, AffectedPopulation, PercentPopulationAffected,
           ViolationCount, TotalViolationYears, WeightedAverage, Percentile)
  ### we dont want the percentile value at this point -- consider adjusting this

  # Final spatial dataset

  CountyViolations.duration <- st_drop_geometry(county) %>%
    dplyr::select("GEOID","NAME")%>%
    dplyr::mutate(NAME = toupper(NAME))%>%
    dplyr::left_join(CountyViolations, by = c("NAME" = "County"))

  # adapt values to geometry object
  geom <- st_drop_geometry(geometry) %>%
    mutate(geoid2 = str_sub(GEOID, 1,5))%>%
    dplyr::left_join(CountyViolations.duration, by = c("geoid2" = "GEOID"))%>%
    dplyr::select("GEOID", drinkingWater = "WeightedAverage")%>%
    dplyr::mutate(drinkingWater = case_when(
      is.na(drinkingWater) ~ 0,
      TRUE ~ drinkingWater
    ))
  write_csv(x = geom, file = file)
  }
  # return feature
  return(geom)
}
