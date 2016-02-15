source("data/util.R")
source("data/radialPlot.R")
###################################################
### HIERARCHICAL CLUSTERING #######################
###################################################
clusteringByYear <- function(df) {
  
  df.geo <- as.data.frame(df$Geo_FIPS)
  names(df.geo) <- "Geo_FIPS"
  #print(df.geo)
  df$Geo_FIPS <- NULL
  
  # calculate the distance matrix,
  # and perform the clustering
  sfbact.hc.complete <- df %>%
    dist(method="euclidean") %>% hclust(method="average") 
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf(paste("data/dendrogram-",year,".pdf", sep=""))
  sfbact.dend <- sfbact.hc.complete %>% 
    as.dendrogram %>% 
    set("branches_k_color", k = nclass, value=colors) %>% 
    plot
  dev.off()
      
  # Find the clusters and assign them to the corresponding Census Tract
  # k = number of classes (where to cut the tree)
  sfbact.hc.complete.cuts <- data.frame(cutree(sfbact.hc.complete,k=nclass))
  
  sfbact.hc.clusters <- cbind(df.geo,sfbact.hc.complete.cuts) %>% 
    select(Geo_FIPS, cluster = cutree.sfbact.hc.complete..k...nclass.)
  
  # Export to CSV
  write.csv(sfbact.hc.clusters, 
            file=paste("data/hclust/sfbact-clusters-",
                       year, 
                       ".csv", 
                       sep=""))
  sfbact.hc.clusters
}

for (i in 1:length(years)) {

  # Create a data frame with variables for each year  
  sfbact.cc <- paste("sfbact.cc.", years[i], sep = "")
  assign(sfbact.cc, 
         readAndParse(years[i], files[i]),
         env = .GlobalEnv)

  # Standardize Data
  sfbact.cc.st <- paste("sfbact.cc.st.", years[i], sep = "")
  cc <- get(paste("sfbact.cc.", years[i], sep = ""))
  assign(sfbact.cc.st,
         standardizeData(cc), 
         env = .GlobalEnv)

  # Perform clustering
  sfbact.hc.cl <- paste("sfbact.hc.cl.", years[i], sep = "")
  st <- get(paste("sfbact.cc.st.", years[i], sep = ""))
  assign(sfbact.hc.cl, 
         clusteringByYear(st), 
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

cl2014 <- inner_join(sfbact.cc.st.2014,sfbact.hc.cl.2014, by="Geo_FIPS")
cl2014$year <- NULL
#cl2014[2:23] <- scale(cl2014[2:23])

means <- as.vector(apply(cl2014[2:23], 2, mean))

cl2014.means.dev <- cl2014[2:24] %>% 
  group_by(cluster) %>% 
    summarise_each(funs(mean)) %>%
      mutate(density=density-means[1],
              PConeUnit=PConeUnit-means[2],
              PCcommutingNotCar=PCcommutingNotCar-means[3],
              PClessHighSchool=PClessHighSchool-means[4],
              PCsomeCollegeOrMore=PCsomeCollegeOrMore-means[5],
              PCdoctorate=PCdoctorate-means[6],
              PCmarriedCouple=PCmarriedCouple-means[7],
              PCwithKids=PCwithKids-means[8],
              PCunmarriedSSCouple=PCunmarriedSSCouple-means[9],
              PCsexMale=PCsexMale-means[10],
              medianAge=medianAge-means[11],
              PCraceWhiteAlone=PCraceWhiteAlone-means[12],
              PCraceBlackAlone=PCraceBlackAlone-means[13],
              PCraceAsianAlone=PCraceAsianAlone-means[14],
              PCraceHispanic=PCraceHispanic-means[15],
              PCforeignBorn=PCforeignBorn-means[16],
              PCownerOccUnits=PCownerOccUnits-means[17],
              PCwithInterests=PCwithInterests-means[18],
              perCapitaIncome=perCapitaIncome-means[19],
              PCunemployed=PCunemployed-means[20],
              PCpoorStruggling=PCpoorStruggling-means[21],
              PCveryWealthyHHolds=PCveryWealthyHHolds-means[22]
             )

source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r")

plot.data <- cl2014.means.dev[-c(3:10),-c(10:23)]
names(plot.data)[1] <- "group"

source("data/radialPlot.R")
# CreateRadialPlot(plot.data)  #Default plot.extent amended to include all of axis label text
################################################################################
# Export data for the radar plot in D3
cl2014.means.var <- cl2014.means[,-1]
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
names(cl2014.means.var) <- Namen
json.list <- list()
for (i in 1:nclass) { # 10 classes
  cl2014.means.i <- cl2014.means.var[i,]
  cl2014.means.i.melted <- melt(cl2014.means.i)
  names(cl2014.means.i.melted) <- c("axis", "value")
  json.list[[i]] <- cl2014.means.i.melted
}
# Export all clusters in one json file
write(toJSON(json.list, pretty = T), "data/radar/radar.json")
# Export each cluster in a separate json file
for (i in 1:nclass) {
  write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/radar/radar-cl", i, ".json", sep = ""))
}

#######
# Same with distance to mean values data
cl2014.means.dev.var <- cl2014.means.dev[,-1]
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
names(cl2014.means.dev.var) <- Namen
json.list <- list()
for (i in 1:nclass) { # 10 classes
  # Artificially add .5 to facilitate the creation of the radar plot in D3
  cl2014.means.dev.i <- cl2014.means.dev.var[i,]+0.5
  cl2014.means.dev.i.melted <- melt(cl2014.means.dev.i)
  names(cl2014.means.dev.i.melted) <- c("axis", "value")
  json.list[[i]] <- cl2014.means.dev.i.melted
}
# Export all clusters in one json file
write(toJSON(json.list, pretty = T), "data/radar/radar-dev.json")
# Export each cluster in a separate json file
for (i in 1:nclass) {
  write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/radar/radar-dev-cl", i, ".json", sep = ""))
}


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


##############################################
### EXPORT DATA FOR SANKEY DIAGRAM ###########
##############################################

# grReshape <- sfbact.hc.clusters %>% dcast(year, cluster, mean, margins=c("year", "cluster"))
# gr <- sfbact.hc.clusters %>% group_by(year, cluster) %>% summarize(tot=sum(cluster))


y2010 <- sfbact.hc.clusters %>% filter(year==2010)
y2011 <- sfbact.hc.clusters %>% filter(year==2011)
y2012 <- sfbact.hc.clusters %>% filter(year==2012)
y2013 <- sfbact.hc.clusters %>% filter(year==2013)
y2014 <- sfbact.hc.clusters %>% filter(year==2014)

y1011 <- inner_join(y2010, y2011, by="Geo_FIPS")
y1112 <- inner_join(y2011, y2012, by="Geo_FIPS")
y1213 <- inner_join(y2012, y2013, by="Geo_FIPS")
y1314 <- inner_join(y2013, y2014, by="Geo_FIPS")
#y <- join_all(list(y2010, y2011, y2012, y2013, y2014), by="Geo_FIPS", type="left")

y1011 <- y1011 %>% group_by(cluster.x, cluster.y) %>%
            summarize(tr=sum(cluster.y)) %>%
              mutate(cluster.source=cluster.x-1, cluster.target=cluster.y+(nclass-1))
y1112 <- y1112 %>% group_by(cluster.x, cluster.y) %>% 
            summarize(tr=sum(cluster.y)) %>%
                  mutate(cluster.source=cluster.x+(nclass-1), cluster.target=cluster.y+((nclass*2)-1))
y1213 <- y1213 %>% group_by(cluster.x, cluster.y) %>% 
            summarize(tr=sum(cluster.y)) %>%
                  mutate(cluster.source=cluster.x+((nclass*2)-1), cluster.target=cluster.y+((nclass*3)-1))
y1314 <- y1314 %>% group_by(cluster.x, cluster.y) %>% 
            summarize(tr=sum(cluster.y)) %>%
                  mutate(cluster.source=cluster.x+((nclass*3)-1), cluster.target=cluster.y+((nclass*4)-1))

y <- rbind(y1011, y1112, y1213, y1314)
#y$cluster.x <- NULL
#y$cluster.y <- NULL

links <- y %>% select(source=cluster.source,target=cluster.target, value=tr) %>% 
                        toJSON(pretty = TRUE)

names <- rep(names,5)

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