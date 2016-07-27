###################################################
# Standardize data for variables using the square root
# + a range from 0 to 1.
###################################################

standardizeData <- function(df) {
  #       df$density <- sqrt(df$density)
  #       df$PCcommutingNotCar <- sqrt(df$PCcommutingNotCar)
  #       df$PClessHighSchool <- sqrt(df$PClessHighSchool)
  #       df$PCdoctorate <- sqrt(df$PCdoctorate)
  #       df$PCunmarriedSSCouple <- sqrt(df$PCunmarriedSSCouple) # or LOG
  #       df$PCraceBlackAlone <- sqrt(df$PCraceBlackAlone)
  #       df$PCraceAsianAlone <- sqrt(df$PCraceAsianAlone)
  #       df$PCraceHispanic <- sqrt(df$PCraceHispanic)
  #       df$PCunemployed <- sqrt(df$PCunemployed)
  #       df$PCpoorStruggling <- sqrt(df$PCpoorStruggling)
  #       df$PCveryWealthyHHolds <- sqrt(df$PCveryWealthyHHolds)

  # store and remove non-value columns
  names <- names(df)
  df.geo <- as.data.frame(df$Geo_FIPS)
  df$year <- NULL
  df$Geo_FIPS <- NULL
  
  # Standardize data by the square root
  df <- as.data.frame(apply(df, 2, sqrt))
  #df <- scale(df, center = TRUE, scale = TRUE)
  
  # Standardize data to a 0-1 range
  range01 <- function(x){ (x - min(x)) / (max(x) - min(x)) }
  df <- apply(df, MARGIN=2, FUN=range01) # ?: only on non-% values?
  
  df <- cbind(df.geo, df)
  names(df) <- names
  df <- as.data.frame(df)
} # StandardizeData()