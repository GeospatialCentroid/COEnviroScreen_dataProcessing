#' Get American Community Survey Data
#'
#' @description Processing function that pulls data from the 2019 Census American Community
#' Survey. Includes parameters : percent people of color, percent under 5,
#' percent over 64, Percent linguistic isolation,Percent less than high school education,
#' Percent low income,Percent disability. The ACS data is called via tidycensus
#'
#' @param processingLevel One of the following character strings ("county","censusTract", "censusBlockGroup")
#' @param version : character description of the current version
#' @param overwrite binary value to determine if data should be rewritten
#'
#'
#' @return data frame
#'

acs <- function(processingLevel, version, overwrite){
  # create version dir
  dir <- paste0("data/output/acs/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  file <- paste0(dir,"/",processingLevel,"_acs.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- vroom::vroom(file)
  }else{

    # change the geometry character to match requirements in tidy census
    if(processingLevel == "censusBlockGroup"){
      scale <- "block group"
    }
    if(processingLevel == "county"){
      scale <- "county"
    }
    if(processingLevel == "censusTract"){
      scale <- "tract"
    }

    # pull ACS data
    acs <- tidycensus::get_acs(
      geography = scale,
      variables = c(
        # under 5
        "B01001_003",
        "B01001_027",
        # over 64
        paste0("B01001_0", 20:25),
        paste0("B01001_0", 44:49),
        #percent people of color
        "B03002_001",
        "B03002_003",
        #Percent low income
        "C17002_001",
        "C17002_008",
        #Percent linguistic isolation
        "C16002_001",
        "C16002_004",
        "C16002_007",
        "C16002_010",
        "C16002_013",
        #Percent less than high school education
        "B15002_001",
        paste0("B15002_00", 3:9),
        "B15002_010",
        paste0("B15002_0", 20:27),
        #Percent disability
        paste0("B18101_", c("001","004","007","010","013","016","019","023",
                            "026","029","032","035","038")),
        #total Population
        "B01003_001"
      ),
      state = "08",
      year = 2019
    )
    # NOTE, for those where total pop/known pop is '0' I change to NA. EJ Screen
    # would set these to '0' but in some cases '0' was actually a meaningful
    # number (i.e., 0 people of color)
    acs <- acs %>%
      tidyr::spread(key = variable, value = estimate) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::group_by(GEOID) %>% # not sure why.. but had to group_by a second time to get correct calculations
      dplyr::mutate(
        age_under5 = sum(B01001_003, B01001_027),
        age_over64 = sum(B01001_020, B01001_021),
        percent_minority = ifelse(B03002_001 == 0, NA, (B03002_001 - B03002_003) /
                                    B03002_001),
        percent_lowincome = ifelse(C17002_001 == 0, NA, (C17002_001 - C17002_008) / C17002_001),
        percent_lingiso = ifelse(
          C16002_001 == 0,
          NA,
          sum(C16002_004, C16002_007, C16002_010, C16002_013) / C16002_001
        ),
        percent_lths = ifelse(
          B15002_001 == 0,
          NA,
          sum(
            B15002_003,
            B15002_004,
            B15002_005,
            B15002_006,
            B15002_007,
            B15002_008,
            B15002_009,
            B15002_010,
            B15002_020,
            B15002_021,
            B15002_022,
            B15002_023,
            B15002_024,
            B15002_025,
            B15002_026,
            B15002_027
          ) / B15002_001
        ),
        percent_disability = sum(B18101_004, B18101_007,
                                 B18101_010, B18101_013,
                                 B18101_016, B18101_019,
                                 B18101_023, B18101_026,
                                 B18101_029, B18101_032,
                                 B18101_035, B18101_038) / B18101_001,
        total_Population = B01003_001

      ) %>%
      dplyr::select(GEOID, age_under5, age_over64, percent_minority, percent_lowincome,
                    percent_lingiso, percent_lths, percent_disability,total_Population)
    write_csv(x = acs, file = file)
    return(acs)
  }
}
