# Function to export raw data for every tracts
# + assigned cluster.
# Used in the "raw variables" tab of the viz.
exportVarData <- function(var, clust, year) {
  #var <- sfbact.cc.2014
  #clust <- sfbact.2014.hc.cl
  sfba.var.cl <- inner_join(var, clust, by = "Geo_FIPS")
  
  # Standardize Density, Median age, Income and Median Year Built to have 0 to 1 value
  # (The others variables are percentage)
  # To facilitate their display on the map (with the same color scale for all var)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  sfba.var.cl.ranging <- as.data.frame(apply(sfba.var.cl[c(2, 3, 15, 21)], MARGIN=2, FUN=range01))
  sfba.var.cl$density <- NULL
  sfba.var.cl$medianAge <- NULL
  sfba.var.cl$perCapitaIncome <- NULL
  sfba.var.cl$medianYearBuilt <- NULL
  sfba.var.cl <- cbind(sfba.var.cl, sfba.var.cl.ranging)
  sfba.var.cl$year.y <- NULL
  colnames(sfba.var.cl)[which(names(sfba.var.cl) == "year.x")] <- "year"
  
  # Reorder columns
  #sfba.var.cl <- sfba.var.cl[, c(1, 24, 27, 2, 16, 3, 21, 4:6, 7, 9, 8, 10, 25, 12:14, 11, 15, 26, 18, 19, 17, 20, 23)]
  sfba.var.cl <- sfba.var.cl[, c(1, 23, 24, 2:12, 25, 13:17, 26, 18:22)]
  
  # Export CSV files
  write.csv(sfba.var.cl, 
            file=paste("data/var/sfbact-var-", 
                       linkage, "-", 
                       nclass, "-", 
                       year, 
                       ".csv", 
                       sep = ""), row.names = FALSE)
  sfba.var.cl
} # exportVarData


################################################################################
## Export data for the radar plot ##############################################
################################################################################
radarPlotData <- function(st, cl, year) {
  #   st <- sfbact.cc.2000.st
  #   cl <- sfbact.2000.hc.cl
  
  # join standardized data set with cluster dataset by geo ID
  stcl <- inner_join(st, cl, by="Geo_FIPS")
  #stcl$year <- NULL
  #stcl$totalPop <- NULL
  
  # Calculate the Bay Area average value for every variable
  stcl.means <- as.vector(apply(stcl[2:25], 2, mean))
  
  # calculate distance to the Bay Area avg for each cluster
  ## Avg value by var and cluster
  stcl.means.cl <- stcl[2:26] %>% 
    group_by(cluster) %>% 
    summarise_each(funs(mean))
  
  ## Distance to Bay Area avg by var and cluster
  stcl.means.dev <- stcl.means.cl %>% 
    mutate(density=density-stcl.means[1],
           medianYearBuilt=medianYearBuilt-stcl.means[2],
           PConeUnit=PConeUnit-stcl.means[3],
           PCownerOccUnits=PCownerOccUnits-stcl.means[4],
           PCcommutingNotCar=PCcommutingNotCar-stcl.means[5],
           PCmovedWithinCounty=PCmovedWithinCounty-stcl.means[6],
           PClessHighSchool=PClessHighSchool-stcl.means[7],
           PCsomeCollegeOrMore=PCsomeCollegeOrMore-stcl.means[8],
           PCdoctorate=PCdoctorate-stcl.means[9],
           PCmarriedCouple=PCmarriedCouple-stcl.means[10],
           PCunmarriedSSCouple=PCunmarriedSSCouple-stcl.means[11],
           PCwithKids=PCwithKids-stcl.means[12],
           PCsexMale=PCsexMale-stcl.means[13],
           medianAge=medianAge-stcl.means[14],
           PCraceBlackAlone=PCraceBlackAlone-stcl.means[15],
           PCraceAsianAlone=PCraceAsianAlone-stcl.means[16],
           PCraceHispanic=PCraceHispanic-stcl.means[17],
           PCraceWhiteAlone=PCraceWhiteAlone-stcl.means[18],
           PCforeignBorn=PCforeignBorn-stcl.means[19],
           perCapitaIncome=perCapitaIncome-stcl.means[20],
           PCunemployed=PCunemployed-stcl.means[21],
           PCpoorStruggling=PCpoorStruggling-stcl.means[22],
           PCwithInterests=PCwithInterests-stcl.means[23],
           PCveryWealthyHHolds=PCveryWealthyHHolds-stcl.means[24]
    )
  
  # Remove 'cluster' column
  stcl.means.dev <- stcl.means.dev[,-1]

  # Rename variables
  Namen <- c("Density",
             "Median year of construction",
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
        paste("data/radar/radar-", linkage, "-", nclass, "-", year, ".json", sep = ""))
  # Export each cluster in a separate json file
  for (i in 1:nclass) {
    write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/radar/radar-", linkage, "-", nclass, "-", year, "-cl", i, ".json", sep = ""))
  }
  stcl.means.dev
} #RadarPlotData


##############################################
### EXPORT DATA FOR SANKEY DIAGRAM ###########
##############################################
# To do: convert to a function...

cl1.2 <- cl %>% group_by(cl2000, cl2010) %>%
  tally() %>%
  mutate(cluster.source = cl2000-1, 
         cluster.target = cl2010 + (nclass-1)) %>%
  select(cluster.source, cluster.target, n)
cl1.2$cl2000 <- NULL

cl2.3 <- cl %>% group_by(cl2010, cl2014) %>%
  tally() %>%
  mutate(cluster.source = cl2010 + (nclass - 1),
         cluster.target = cl2014 + ((nclass*2) - 1)) %>%
  select(cluster.source, cluster.target, n)
cl2.3$cl2010 <- NULL

cl.1.2.3 <- as.data.frame(rbind(cl1.2, cl2.3)) # as df to dplyr ungroup

links <- cl.1.2.3 %>% select(source = cluster.source, target = cluster.target, value = n) %>% 
  toJSON(pretty = TRUE)

#names <- c(as.character(1:15))
names <- c("cl1", "cl2", "cl3", "cl4", "cl5", "cl6", "cl7",  "cl8", 
           "cl9", "cl10", "cl11", "cl12", "cl13", "cl14",  "cl15")

names <- rep(names,3)
length(names)

nodes <- as.data.frame(matrix(ncol=2,nrow=length(names)))
for (i in 0:length(names)) {
  nodes[i, 1] <- i-1
  nodes[i, 2] <- names[i]
}
names(nodes) <- c("node", "name")
nodes <- toJSON(nodes, pretty = T)

write(paste("{\"nodes\":
", nodes, ",
\"links\":
",links, "}", sep=""), "data/sankey/sankey.json")