#geometric mean
# take from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

gm_mean = function(x){
  # Drop the NA values from the list of features.
  # the reduced the length of the denominator
  x <- x[!is.na(x)]

  ### moving away from this due to potential diff between lenght in input in numerator/denomator when zeros are present
  # exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))

  exp(mean(log(x[x>0])))

}

