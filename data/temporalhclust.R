
################################################
## Hclust for all years all together ###########
################################################
# for (i in 1:length(years)) {
#   name <- paste("sfbact.cc.", years[i], sep="")
#   assign(name, readAndParse(years[i], files[i]))
# }

temporalClustering <- function(st.2009, st.2010, st.2011, st.2012, st.2013, st.2014) {
  
#   st.2009 <- sfbact.cc.st.2009
#   st.2010 <- sfbact.cc.st.2010
#   st.2011 <- sfbact.cc.st.2011
#   st.2012 <- sfbact.cc.st.2012
#   st.2013 <- sfbact.cc.st.2013
#   st.2014 <- sfbact.cc.st.2014
  
  st.2009 <- st.2009 %>% mutate(year = 2009)
  st.2010 <- st.2010 %>% mutate(year = 2010)
  st.2011 <- st.2011 %>% mutate(year = 2011)
  st.2012 <- st.2012 %>% mutate(year = 2012)
  st.2013 <- st.2013 %>% mutate(year = 2013)
  st.2014 <- st.2014 %>% mutate(year = 2014)
  
  st.years <- rbind(st.2009, st.2010, st.2011, st.2012, st.2013, st.2014)
  
  years.geo <- st.years %>% select(Geo_FIPS,year)
  
  st.years$Geo_FIPS <- NULL
  st.years$year <- NULL
  
  # Perform (temporal) hierarchical clustering
  years.thc <- st.years %>% dist(method = "euclidean") %>% hclust(method = "complete")
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf("data/dendrogram/thclust-dendrogram.pdf")
  sfbact.dend <- years.thc %>% 
    as.dendrogram %>% 
    set("branches_k_color", k = nclass, value=colors) %>% 
    plot
  dev.off()
  
  # Find the clusters and assign them to the corresponding Census Tract
  # k = number of classes (where to cut the tree)
  years.thc.cuts <- data.frame(cutree(years.thc,k=nclass))
  
  years.thc.clusters <- cbind(years.geo,years.thc.cuts) %>% 
    select(Geo_FIPS, year = year, cluster = cutree.years.thc..k...nclass.)
  
  # Export to CSV
  for (i in 1:length(years)) {
    df <- filter(years.thc.clusters, year==years[i])
    write.csv(df, 
              file=paste("data/temphclust/complete/sfbact-clusters-",
                         years[i], 
                         ".csv", 
                         sep=""))
  }
  years.thc.clusters
}

exportTempVarData <- function(var, clust, year) {
  sfba.var.cl <- inner_join(var, clust, by = "Geo_FIPS")
  write.csv(sfba.var.cl, 
            file=paste("data/var/thclust/sfbact-var-",
                       year, 
                       ".csv", 
                       sep = ""))
} # exportTempVarData

################################################################################
## Export data for the radar plot in D3 ########################################
################################################################################
radarPlotTempData <- function(st, cl, year) {
  
  # join standardized data set with cluster dataset by geo ID
  stcl <- inner_join(st,cl, by="Geo_FIPS")
  stcl$year <- NULL
  
  # Calculate means of each variables for the whole bay area
  stcl.means <- as.vector(apply(stcl[2:23], 2, mean))
  
  # calculate distance to the Bay Area mean for each cluster
  stcl.means.cl <- stcl[2:24] %>% 
    group_by(cluster) %>% 
    summarise_each(funs(mean))
  
  stcl.means.dev <- stcl.means.cl %>% 
    mutate(density=density-stcl.means[1],
           PConeUnit=PConeUnit-stcl.means[2],
           PCcommutingNotCar=PCcommutingNotCar-stcl.means[3],
           PClessHighSchool=PClessHighSchool-stcl.means[4],
           PCsomeCollegeOrMore=PCsomeCollegeOrMore-stcl.means[5],
           PCdoctorate=PCdoctorate-stcl.means[6],
           PCmarriedCouple=PCmarriedCouple-stcl.means[7],
           PCwithKids=PCwithKids-stcl.means[8],
           PCunmarriedSSCouple=PCunmarriedSSCouple-stcl.means[9],
           PCsexMale=PCsexMale-stcl.means[10],
           medianAge=medianAge-stcl.means[11],
           PCraceWhiteAlone=PCraceWhiteAlone-stcl.means[12],
           PCraceBlackAlone=PCraceBlackAlone-stcl.means[13],
           PCraceAsianAlone=PCraceAsianAlone-stcl.means[14],
           PCraceHispanic=PCraceHispanic-stcl.means[15],
           PCforeignBorn=PCforeignBorn-stcl.means[16],
           PCownerOccUnits=PCownerOccUnits-stcl.means[17],
           PCwithInterests=PCwithInterests-stcl.means[18],
           perCapitaIncome=perCapitaIncome-stcl.means[19],
           PCunemployed=PCunemployed-stcl.means[20],
           PCpoorStruggling=PCpoorStruggling-stcl.means[21],
           PCveryWealthyHHolds=PCveryWealthyHHolds-stcl.means[22]
    )
  
  # Remove 'cluster' column
  stcl.means.dev <- stcl.means.dev[,-1]
  # Rename variables
  Namen <- c("Density", "One Unit", 
             "Public Transit", "Less than High School", 
             "Some college", "PhD holder", 
             "Married couple", "With kids", 
             "Same-sex couples", "Males", 
             "Median age", "Whites", 
             "Blacks", "Asian", 
             "Hispanic", "Foreign-born", 
             "Owner-occupied", "With interests", 
             "Per capita income", "Unemployment", 
             "Poor and Struggling", "Very wealthy")
  names(stcl.means.dev) <- Namen
  
  # Output json file used in D3 visualization
  json.list <- list()
  for (i in 1:nclass) { # 10 classes
    # Artificially add .5 to facilitate the creation of the radar plot in D3
    stcl.means.dev.i <- stcl.means.dev[i,]+0.5
    stcl.means.dev.i.melted <- melt(stcl.means.dev.i)
    names(stcl.means.dev.i.melted) <- c("axis", "value")
    json.list[[i]] <- stcl.means.dev.i.melted
  }
  # Export all clusters in one json file
  write(toJSON(json.list, pretty = T), 
        paste("data/radar/thclust/radar-", year, ".json", sep = ""))
  # Export each cluster in a separate json file
  for (i in 1:nclass) {
    write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/radar/thclust/radar-", year, "-cl", i, ".json", sep = ""))
  }
  stcl.means.dev
} #RadarPlotTempData