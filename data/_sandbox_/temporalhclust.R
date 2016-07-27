
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
  years.thc <- st.years %>% dist(method = "euclidean") %>% hclust(method = linkage)
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf(paste("data/dendrogram/thclust-", linkage, "-", nclass, "-dendrogram.pdf", sep = ""))
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
              file=paste("data/thclust/", linkage, "/", nclass, "/sfbact-clusters-",
                         years[i], 
                         ".csv", 
                         sep=""))
  }
  years.thc.clusters
}

################################################################################
## Export variable data ########################################################
################################################################################
exportTempVarData <- function(var, clust, cyear) {
  #print(clust)
#   var <- sfbact.cc.2014
#   clust <- years.thclust
#   cyear <- 2014
  cl.year <- clust %>% filter(year==cyear)
  
  sfba.var.cl <- inner_join(var, cl.year, by = "Geo_FIPS")
  
  # Standardize Density, Median age, Income and Median Year Built to have 0 to 1 value
  # (The others variables are percentage)
  # To facilitate their display on the map (with the same color scale for all var)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  sfba.var.cl.ranging <- as.data.frame(apply(sfba.var.cl[c(2, 12, 20, 25)], MARGIN=2, FUN=range01))
  sfba.var.cl$density <- NULL
  sfba.var.cl$medianAge <- NULL
  sfba.var.cl$perCapitaIncome <- NULL
  sfba.var.cl$medianYearBuilt <- NULL
  sfba.var.cl <- cbind(sfba.var.cl, sfba.var.cl.ranging)
  sfba.var.cl$year.y <- NULL
  colnames(sfba.var.cl)[which(names(sfba.var.cl) == "year.x")] <- "year"
  # Reorder columns
  sfba.var.cl <- sfba.var.cl[, c(1, 24, 27, 2, 16, 3, 21, 4:6, 7, 9, 8, 10, 25, 12:14, 11, 15, 26, 18, 19, 17, 20, 23)]

  # Export CSV files
  write.csv(sfba.var.cl, 
            file=paste("data/var/thclust/", linkage, "/", nclass, "/sfbact-var-",
                       cyear, 
                       ".csv", 
                       sep = ""))
  sfba.var.cl
} # exportTempVarData

################################################################################
## Export data for the radar plot in D3 ########################################
################################################################################
radarPlotTempData <- function(st, cl, cyear) {
  
  # get current year data
  cl.year <- cl %>% filter(year==cyear)
  
  # join standardized data set with cluster dataset by geo ID
  stcl <- inner_join(st, cl.year, by="Geo_FIPS")
  stcl$year <- NULL
  
  # Calculate means of each variables for the whole bay area
  stcl.means <- as.vector(apply(stcl[2:25], 2, mean))
  
  # calculate distance to the Bay Area mean for each cluster
  stcl.means.cl <- stcl[2:26] %>% 
    group_by(cluster) %>% 
    summarise_each(funs(mean))
  
  stcl.means.dev <- stcl.means.cl %>% 
    mutate(density=density-stcl.means[1],
           medianYearBuilt=medianYearBuilt-stcl.means[24],
           PConeUnit=PConeUnit-stcl.means[2],
           PCownerOccUnits=PCownerOccUnits-stcl.means[17],
           PCcommutingNotCar=PCcommutingNotCar-stcl.means[3],
           PCmovedWithinCounty=PCmovedWithinCounty-stcl.means[23],
           PClessHighSchool=PClessHighSchool-stcl.means[4],
           PCsomeCollegeOrMore=PCsomeCollegeOrMore-stcl.means[5],
           PCdoctorate=PCdoctorate-stcl.means[6],
           PCmarriedCouple=PCmarriedCouple-stcl.means[7],
           PCunmarriedSSCouple=PCunmarriedSSCouple-stcl.means[9],
           PCwithKids=PCwithKids-stcl.means[8],
           PCsexMale=PCsexMale-stcl.means[10],
           medianAge=medianAge-stcl.means[11],
           PCraceBlackAlone=PCraceBlackAlone-stcl.means[13],
           PCraceAsianAlone=PCraceAsianAlone-stcl.means[14],
           PCraceHispanic=PCraceHispanic-stcl.means[15],
           PCraceWhiteAlone=PCraceWhiteAlone-stcl.means[12],
           PCforeignBorn=PCforeignBorn-stcl.means[16],
           perCapitaIncome=perCapitaIncome-stcl.means[19],
           PCunemployed=PCunemployed-stcl.means[20],
           PCpoorStruggling=PCpoorStruggling-stcl.means[21],
           PCwithInterests=PCwithInterests-stcl.means[18],
           PCveryWealthyHHolds=PCveryWealthyHHolds-stcl.means[22]
           )
  
  # Remove 'cluster' column
  stcl.means.dev <- stcl.means.dev[,-1]
  # Reorder Columns
  stcl.means.dev <- stcl.means.dev[, c(1, 24, 2, 17, 3, 23, 4:6, 7, 9, 8, 10, 11, 13:15, 12, 16, 19, 20, 21, 18, 22)]
  # Rename variables
  Namen <- c("Density",
             "median year of construction",
             "One Unit", 
             "Owner-occupied", 
             "Public Transit",
             "Moved within county in the year",
              
             "Less than High School", 
             "Some college", 
             "PhD holder", 
             
             "Married couple", 
             "Same-sex couples",
             "With kids", 
              
             "Males", 
             "Median age", 
             "Blacks", 
             "Asian", 
             "Hispanic", 
             "Whites",
             "Foreign-born", 
             
             "Per capita income", 
             "Unemployment",
             "Poor and Struggling",
             "With interests",
             "Very wealthy"
             )
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
        paste("data/radar/thclust/", linkage, "/", nclass, "/radar-", cyear, ".json", sep = ""))
  # Export each cluster in a separate json file
  for (i in 1:nclass) {
    write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/radar/thclust/", linkage, "/", nclass, "/radar-", cyear, "-cl", i, ".json", sep = ""))
  }
  stcl.means.dev
} #RadarPlotTempData