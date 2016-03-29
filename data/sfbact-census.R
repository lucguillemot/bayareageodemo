source("data/lib_var.R")
source("data/util.R")
source("data/temporalhclust.R")
nclass <- 15
linkage = "complete"

  # Create a data frame with variables for each year
  sfbact.cc.2000 <- readAndParseCensusData(2000, "R11097593_SL140-2000-Census.csv")
  sfbact.cc.2010 <- readAndParse(2010, "R11097565_SL140-2010")
  sfbact.cc.2014 <- readAndParse(2014, "R11088321_SL140-2014")
# sfbact.cc.2000[sfbact.cc.2000$Geo_FIPS=="6001402700",]
# sfbact.cc.2000[sfbact.cc.2000$Geo_FIPS=="6075012602",]
# df[df$Geo_FIPS=="6075012602",]
  # Standardize data (square root + range standardization)
  sfbact.cc.2000.st <- standardizeData(sfbact.cc.2000)
  sfbact.cc.2010.st <- standardizeData(sfbact.cc.2010)
  sfbact.cc.2014.st <- standardizeData(sfbact.cc.2014)

  # Perform (temporal) hierarchical clustering
  sfbact.2000.hc.cl <- clusteringByYear(sfbact.cc.2000.st, 2000)
  sfbact.2010.hc.cl <- clusteringByYear(sfbact.cc.2010.st, 2010)
  sfbact.2014.hc.cl <- clusteringByYear(sfbact.cc.2014.st, 2014)

  # Export csv data file with raw data + cluster number
  # used in the d3 plot of raw variables
  sfbact.2000.var.cl <- exportVarData(sfbact.cc.2000, sfbact.2000.hc.cl, 2000)
  sfbact.2010.var.cl <- exportVarData(sfbact.cc.2010, sfbact.2010.hc.cl, 2010)
  sfbact.2014.var.cl <- exportVarData(sfbact.cc.2014, sfbact.2014.hc.cl, 2014)
  
  # Calculate means, distances to the mean
  # and export data for the radar plot
  sfbact.2000.mdev <- radarPlotData(sfbact.cc.2000.st, sfbact.2000.hc.cl, 2000)
  sfbact.2010.mdev <- radarPlotData(sfbact.cc.2010.st, sfbact.2010.hc.cl, 2010)
  sfbact.2014.mdev <- radarPlotData(sfbact.cc.2014.st, sfbact.2014.hc.cl, 2014)

  unique(sfbact.2000.hc.cl$cluster)
  unique(sfbact.2010.hc.cl$cluster)
  unique(sfbact.2014.hc.cl$cluster)


################################################
## Spatial overlap of clusters over time #######
################################################
# GET relationship file between Geo_FIPS 2000 and Geo_FIPS 2010
geo.rel <- read.csv("data/census/relationship.csv") %>% select(d,m)
names <- c("geo2010", "Geo_FIPS")
names(geo.rel) <- names
sfbact.2000.hc.cl <- merge(sfbact.2000.hc.cl, geo.rel, by = "Geo_FIPS")
sfbact.2000.hc.cl$Geo_FIPS <- NULL # removes the 2000 Geo_FIPS
names <- c("cluster", "Geo_FIPS")
names(sfbact.2000.hc.cl) <- names

# merge all clusters files
cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
names(cl) <- names

cl.change <- cl %>% select(Geo_FIPS, cl2014, cl2010) %>% 
  group_by(cl2014, cl2010) %>%
  tally() # just 'tally()' count the number of occurrences without summing!


################################################
## Cluster Reassignment ########################
################################################

# 2014 is the base year. New cluster numbers are assigned to
# previous years according to the maximal number of overlapping
# tracts with 2014.

cl.2014 <- c(1:15) # 2014 and  2010 OLD
cl.2010.new <- c(1, 6, 3, 8, 5, 
                 3, 4, 7, 8, 13, 
                 10, 13, 12, 2, 5) # 2010 NEW - we lose clusters 9, 11, 14 & 15

sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 1] <- cl.2010.new[1]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 2] <- cl.2010.new[2]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 3] <- cl.2010.new[3]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 4] <- cl.2010.new[4]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 5] <- cl.2010.new[5]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 6] <- cl.2010.new[6]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 7] <- cl.2010.new[7]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 8] <- cl.2010.new[8]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 9] <- cl.2010.new[9]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 10] <- cl.2010.new[10]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 11] <- cl.2010.new[11]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 12] <- cl.2010.new[12]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 13] <- cl.2010.new[13]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 14] <- cl.2010.new[14]
sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 15] <- cl.2010.new[15]

# Re evaluate change
# merge all clusters files
cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
names(cl) <- names

cl.change <- cl %>% select(Geo_FIPS, cl2010, cl2000) %>% 
  group_by(cl2010, cl2000) %>%
  tally() # just 'tally()' count the number of occurrences without summing!

# New cluster numbers are assigned to
# 2000 according to the maximal number of overlapping
# tracts with 2010.

cl.2010 <- c(1:15) # 2010 and  2000 OLD
cl.2000.new <- c(1, 14, 2, 3, 11, 
                 0, 2, 6, 0, 2, 
                 0, 2, 0, 0, 0) # 2000 NEW

sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 1] <- cl.2000.new[1]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 2] <- cl.2000.new[2]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 3] <- cl.2000.new[3]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 4] <- cl.2000.new[4]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 5] <- cl.2000.new[5]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 6] <- cl.2000.new[6]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 7] <- cl.2000.new[7]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 8] <- cl.2000.new[8]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 9] <- cl.2000.new[9]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 10] <- cl.2000.new[10]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 11] <- cl.2000.new[11]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 12] <- cl.2000.new[12]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 13] <- cl.2000.new[13]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 14] <- cl.2000.new[14]
sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 15] <- cl.2000.new[15]





# Export to CSVs
write.csv(sfbact.2000.hc.cl, 
          file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
                     "2000", 
                     ".csv", 
                     sep = ""))
write.csv(sfbact.2010.hc.cl, 
          file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
                     "2010", 
                     ".csv", 
                     sep = ""))

##############################################
### EXPORT DATA FOR SANKEY DIAGRAM ###########
##############################################


# Re evaluate change
# merge all clusters files
cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
names(cl) <- names

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
names <- c("Cluster One",
           "Cluster Two",
           "Cluster Three",
           "Cluster Four",
           "Cluster Five",
           "Cluster Six",
           "Cluster Seven",
           "Cluster Eight",
           "Cluster Nine",
           "Cluster Ten",
           "Cluster Eleven",
           "Cluster Twelve",
           "Cluster Thirteen",
           "Cluster Fourteen",
           "Cluster Fifteen")

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
",links, "}", sep=""), "data/censushclust/sankey/sankey.json")





##############################################
### MEASURE CHANGE ###########################
##############################################
# count the number of class change for each tract.

cl$change <- 0 # Initiate an empty field
for (i in 1:length(cl$Geo_FIPS)) {
  if (cl[i,2] != cl[i,3]) {cl[i,5] <- 1}
  if (cl[i,3] != cl[i,4]) {cl[i,5] <- cl[i,5]+1}
}
unique(cl[,5])

names(cl) <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014", "change")

write.csv(cl, 
paste("data/censushclust/change/", linkage, "/", nclass, "/change.csv", sep = ""))
###################################################





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