#' Get EJ Screen data
#'
#' Gather input parameters from the 2021 EPA EJ Screen dataset
#' includes Percent People of Color, Percent Low Income, Percent without high school diploma,
#' Percent linguistic isolation, Percent under 5, percent over 64, Homes with Lead Paint,
#' Diesel Particulate matter, traffic proximity, Waste water discharge, npl site proximity
#' rmp site proximity, tsdf site proximity
#'
#' @param geometry An sf polygon object with GEOID column
#' @param processingLevel One of the following character strings ("county","censusTract", "censusBlockGroup")
#' @param version : character description of the current version
#' @param overwrite binary value to determine if data should be rewritten
#'
#' @return a dataframe

ej_screen <- function(filePath, geometry, processingLevel, version,  overwrite){
  # create version dir
  dir <- paste0("data/output/EJScreen/",version)
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  # relates to the output file
  file <- paste0(dir,"/",processingLevel,"_EJScreen.csv")
  if(file.exists(file) & isFALSE(overwrite)){
    geom <- vroom::vroom(file)
  }else{
    d1 <- vroom::vroom(filePath)%>%
      dplyr::filter(STATE_NAME == "Colorado") %>%
      dplyr::select(
        GEOID = ID,
        peopleOfColor = MINORPCT,
        lowIncome = LOWINCPCT,
        highSchool = LESSHSPCT,
        linguisticIsolation = LINGISOPCT,
        under5 = UNDER5PCT,
        over64 = OVER64PCT,
        leadPaint = PRE1960PCT,
        deiselPM = DSLPM,
        trafficeProx = PTRAF,
        waterDischarge = PWDIS,
        nplProx = PNPL,
        rmpProx = PRMP,
        tsdfProx = PTSDF
      )

    # process based on geometry level
    if(nchar(geometry$GEOID[1])==5){
      i <- 5
    }
    if(nchar(geometry$GEOID[1])==11){
      i <- 11
    }
    if(nchar(geometry$GEOID[1])==12){
      i <- 12
    }
    # processing based on the length of GEOID object
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = i)) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(
        peopleOfColor = mean(peopleOfColor,na.rm=TRUE),
        lowIncome = mean(lowIncome,na.rm=TRUE),
        highSchool = mean(highSchool,na.rm=TRUE),
        linguisticIsolation = mean(linguisticIsolation,na.rm=TRUE),
        under5 = mean(under5,na.rm=TRUE),
        over64 = mean(over64,na.rm=TRUE),
        leadPaint = mean(leadPaint,na.rm=TRUE),
        deiselPM = mean(deiselPM,na.rm=TRUE),
        trafficeProx = mean(trafficeProx,na.rm=TRUE),
        waterDischarge = mean(waterDischarge,na.rm=TRUE),
        nplProx = mean(nplProx,na.rm=TRUE),
        rmpProx = mean(rmpProx,na.rm=TRUE),
        tsdfProx = mean(tsdfProx,na.rm=TRUE)
      )
    write_csv(x = geom, file = file)
  }
  return(geom)
}
