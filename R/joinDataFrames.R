

joinDataFrames <- function(dataframes){

  ###
  # directions from cal enviroscreen
  # When a geographic area has no indicator value (for example, the census tract
  # has no hazardous waste generators or facilities), it is excluded from the percentile
  # calculation and assigned a score of zero for that indicator. When data are
  # unreliable or missing for a geographic area, such as census data with large
  # uncertainties, it is excluded from the percentile calculation and is not assigned
  # any score for that indicator. Thus the percentile score can be thought of as a
  # comparison of one geographic area to other localities in the state where the
  # hazard effect or population characteristic is present
  ###

  # combine all features
  df <- dataframes %>%
    purrr::reduce(dplyr::left_join, by = "GEOID")

  # a indicator wise process for filtering and assigning values
  for(i in 2:length(names(df))){
    # grab geoid and specific indicator
    df2 <- df[ ,c(1,i)]
    # NA values from the percentile calculation
    df2a <- df2 %>%
      filter(across(where(is.numeric), ~!is.na(.)))%>%
      # across(where(is.numeric), ~. != 0))
      dplyr::mutate(
        across(where(is.numeric),
               .fns = list(pcntl = ~cume_dist(.)*100),
               .names = "{col}_{fn}"))
    # assign values to pcntl for all zero and NA measures
    df2b <- df2 %>%
      filter(!GEOID %in% df2a$GEOID)%>%
      dplyr::mutate(
        across(where(is.numeric),
               .fns = list(pcntl = ~.),
               .names = "{col}_{fn}"))
    #combine the datasets
    df3 <- bind_rows(df2a,df2b)
    # combine after loop
    if(i ==2){
      df3a <- df3
    }else{
      df3a <- dplyr::left_join(df3a, df3, by = "GEOID")
    }
  }
  return(df3a)
}
