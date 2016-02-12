
##############################################
### KMEANS ###################################
##############################################

for (i in 1:length(years)) {
  name <- paste("sfbact.cc.", years[i], sep="")
  assign(name, readAndParse(years[i], files[i]))
}

sfbact.cc.years <- rbind(sfbact.cc.2014, 
                         sfbact.cc.2013, 
                         sfbact.cc.2012,
                         sfbact.cc.2011,
                         sfbact.cc.2010,
                         sfbact.cc.2009)

sfbact.geo <- sfbact.cc.years %>% select(Geo_FIPS,year)
sfbact.cc.years$Geo_FIPS <- NULL
sfbact.cc.years$year <- NULL

cl <- sfbact.cc.years %>% scale %>% kmeans(10)
sfbact.km.clusters <- as.data.frame(cbind(sfbact.geo$Geo_FIPS,
                                          sfbact.geo$year,
                                          cl$cluster)) %>% 
  select(Geo_FIPS=V1,
         year=V2,
         cluster=V3)

# Export to CSV
for (i in 1:length(years)) {
  df <- filter(sfbact.km.clusters, year==years[i])
  write.csv(df, 
            file=paste("data/kmeans/sfbact-clusters-",
                       years[i], 
                       ".csv", 
                       sep=""))
}