source("data/lib_var.R")
source("data/util.R")
source("data/temporalhclust.R")
nclass <- 15
linkage = "complete"

# # # # # # # # # # # # # # # # # # 
# # # All years together # # # # #
# # # # # # # # # # # # # # # # # # 
for (i in 1:length(years)) {
  
  # Create a data frame with variables for each year  
  sfbact.cc <- paste("sfbact.cc.", years[i], sep = "")
  assign(sfbact.cc, 
         readAndParse(years[i], files[i]),
         env = .GlobalEnv)
  
  # Standardize data (square root + range standardization)
  sfbact.cc.st <- paste("sfbact.cc.st.", years[i], sep = "")
  cc <- get(paste("sfbact.cc.", years[i], sep = ""))
  assign(sfbact.cc.st,
         standardizeData(cc), 
         env = .GlobalEnv)
}

# Perform (temporal) hierarchical clustering
years.thclust <- temporalClustering(sfbact.cc.st.2009, 
                     sfbact.cc.st.2010, 
                     sfbact.cc.st.2011, 
                     sfbact.cc.st.2012, 
                     sfbact.cc.st.2013, 
                     sfbact.cc.st.2014)


for (i in 1:length(years)) {
  # Export csv data file with raw data + cluster number
  # used in the d3 plot of raw variables
  sfba.var.cl <- paste("sfba.var.cl.", years[i], sep = "")

  var <- get(paste("sfbact.cc.", years[i], sep = ""))
  assign(sfba.var.cl,
         exportTempVarData(var, years.thclust, years[i]),
         env = .GlobalEnv)
  
  # Calculate means, distances to the mean
  # and export data for the radar plot
  st <- get(paste("sfbact.cc.st.", years[i], sep = ""))
  radarPlotTempData(st, years.thclust, years[i])
}

unique(sfba.var.cl.2014$cluster)

# Measure change
#cl.2009 <- years.thclust %>% filter(year==2009) %>% select(Geo_FIPS, cluster)
cl.2010 <- years.thclust %>% filter(year==2010) %>% select(Geo_FIPS, cluster)
cl.2011 <- years.thclust %>% filter(year==2011) %>% select(Geo_FIPS, cluster)
cl.2012 <- years.thclust %>% filter(year==2012) %>% select(Geo_FIPS, cluster)
cl.2013 <- years.thclust %>% filter(year==2013) %>% select(Geo_FIPS, cluster)
cl.2014 <- years.thclust %>% filter(year==2014) %>% select(Geo_FIPS, cluster)


cl.1011 <- merge(cl.2010, cl.2011, by = "Geo_FIPS")
cl.101112 <- merge(cl.1011, cl.2012, by = "Geo_FIPS")
cl.10111213 <- merge(cl.101112, cl.2013, by = "Geo_FIPS")
cl.merged <- merge(cl.10111213, cl.2014, by = "Geo_FIPS")

names(cl.merged) <- c("Geo_FIPS","cl.2010", "cl.2011", "cl.2012", "cl.2013", "cl.2014")
cl.merged$change <- 0

# if (cl.merged[5,2] != cl.merged[5,3]) {cl.merged[5,8] <- 1}
# if (cl.merged[5,3] != cl.merged[5,4]) {cl.merged[5,8] <- cl.merged[5,8]+1}
# if (cl.merged[5,4] != cl.merged[5,5]) {cl.merged[5,8] <- cl.merged[5,8]+1}
# if (cl.merged[5,5] != cl.merged[5,6]) {cl.merged[5,8] <- cl.merged[5,8]+1}
# if (cl.merged[5,6] != cl.merged[5,7]) {cl.merged[5,8] <- cl.merged[5,8]+1}

for (i in 1:length(cl.merged$Geo_FIPS)) {
  if (cl.merged[i,2] != cl.merged[i,3]) {cl.merged[i,7] <- 1}
  if (cl.merged[i,3] != cl.merged[i,4]) {cl.merged[i,7] <- cl.merged[i,7]+1}
  if (cl.merged[i,4] != cl.merged[i,5]) {cl.merged[i,7] <- cl.merged[i,7]+1}
  if (cl.merged[i,5] != cl.merged[i,6]) {cl.merged[i,7] <- cl.merged[i,7]+1}
}
unique(cl.merged$change)

write.csv(cl.merged, 
paste("data/change/thclust/", linkage, "/", nclass, "/change.csv", sep = ""))
###################################################






# # # # # # # # # # # # # # # # # # 
# # # Each year separately # # # # #
# # # # # # # # # # # # # # # # # # 
for (i in 1:length(years)) {

  # Create a data frame with variables for each year  
  sfbact.cc <- paste("sfbact.cc.", years[i], sep = "")
  assign(sfbact.cc, 
         readAndParse(years[i], files[i]),
         env = .GlobalEnv)

  # Standardize data (square root + range standardization)
  sfbact.cc.st <- paste("sfbact.cc.st.", years[i], sep = "")
  cc <- get(paste("sfbact.cc.", years[i], sep = ""))
  assign(sfbact.cc.st,
         standardizeData(cc), 
         env = .GlobalEnv)

  # Perform hierarchical clustering
  sfbact.hc.cl <- paste("sfbact.hc.cl.", years[i], sep = "")
  st <- get(paste("sfbact.cc.st.", years[i], sep = ""))
  assign(sfbact.hc.cl, 
         clusteringByYear(st, years[i]), 
         env = .GlobalEnv)
  
  # Export csv data file with raw data + cluster number
  # used in the d3 plot of raw variables
  sfba.var.cl <- paste("sfba.var.cl.", years[i], sep = "")
  var <- get(paste("sfbact.cc.", years[i], sep = ""))
  clust <- get(paste("sfbact.hc.cl.", years[i], sep = ""))
  assign(sfba.var.cl,
         exportVarData(var, clust, years[i]),
         env = .GlobalEnv)
  
  
  # Calculate means, distances to the mean
  # and export data for the radar plot
  cl <- get(paste("sfbact.hc.cl.", years[i], sep = ""))
  cl.means <- paste("cl.means.", years[i], sep = "") ## Unused
  assign(cl.means, 
         radarPlotData(st, cl, years[i]), 
         env = .GlobalEnv)
}

################################################
## Spatial overlap of clusters over time #######
################################################






##############################################
### RADIAL PLOTS #############################
##############################################

# var2014 <- sfbact.cc.2014
# var2014$Geo_FIPS <- NULL
# # Standardize data
# var2014.scaled <- as.data.frame(scale(var2014))
# # Check standard deviation = 1 and mean = 0
# apply(var2014.scaled, 2, sd)
# apply(var2014.scaled, 2, mean) ## it is not...


##############################################
### DESCRIPTION OF CLUSTERS ##################
##############################################

cl2014 <- sfbact.hc.clusters %>% filter(year==2014)
sfbact.cc.2014.geo <- sfbact.cc.2014$Geo_FIPS
sfbact.cc.2014$year <- NULL
sfbact.cc.2014$Geo_FIPS <- NULL

x <-sfbact.cc.2013$perCapitaIncome

hist(x)
hist(log(x+sqrt(x^2+1)))
hist(log(2*x))

pairs(sfbact.cc.[,c(9,10,11,12,13)])
# Standardize data
sfbact.cc.2014.scaled <- as.data.frame(scale(sfbact.cc.2014))
# Check standard deviation = 1 and mean = 0
apply(sfbact.cc.2014.scaled, 2, sd)
apply(sfbact.cc.2014.scaled, 2, mean) ## it is not...

var2014 <- cbind(sfbact.cc.2014.geo,sfbact.cc.2014.scaled, cl2014)
var2014[,1] <- NULL
colnames(var2014) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "Geo_FIPS", "year", "cluster")

# Group by Clusters and calculate the mean distance to the mean
# for each cluster
var2014.cl.means <- var2014 %>% group_by(cluster) %>% 
  summarize(MV1=mean(V1),
            MV2=mean(V2),
            MV3=mean(V3),
            MV4=mean(V4),
            MV5=mean(V5),
            MV6=mean(V6),
            MV7=mean(V7),
            MV8=mean(V8),
              MV9=mean(V9),
              MV10=mean(V10),
              MV11=mean(V11),
              MV12=mean(V12))

plot(var2014$medianAge)

# Reshape the data for plotting
melted <- melt(cl2014.means.dev)
melted$cluster <- 1:10
melted <- melted[-c(1:10),]

# Plot the distances to the mean for each variable, for each cluster
png("data/clusters_desc.png", 
  width = 500, 
  height = 1000) ## Open the PNG device
g <- ggplot(melted, aes(variable,value, fill=factor(cluster)))
g+facet_grid(cluster~.)+geom_bar(stat="identity")
dev.off()