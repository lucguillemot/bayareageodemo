###################################################
### HIERARCHICAL CLUSTERING #######################
###################################################

clusteringAllYears <- function(st.2000, st.2010, st.2014, years) {
  
  #   st.2000 <- sfbact.cc.2000.st
  #   st.2010 <- sfbact.cc.2010.st
  #   st.2014 <- sfbact.cc.2014.st
  
  st.2000 <- st.2000 %>% mutate(year = 2000)
  st.2010 <- st.2010 %>% mutate(year = 2010)
  st.2014 <- st.2014 %>% mutate(year = 2014)
  
  st.years <- rbind(st.2000, st.2010, st.2014)
  
  ## !! REMOVING OF OUTLIERS !! #######################
  #   df <- df[!(df$Geo_FIPS == "6095252802"),] # Air force base
  #   df <- df[!(df$Geo_FIPS == "6095252801"),] # Air force base
  #   df <- df[!(df$Geo_FIPS == "6095980000"),] # Air force base
  st.years <- st.years[!(st.years$Geo_FIPS == "6055200900"), ] # NAPA
  st.years <- st.years[!(st.years$Geo_FIPS == "6041122000"), ] # Saint-Quentin (prison)
  
  years.geo <- st.years %>% select(Geo_FIPS, year)
  
  st.years$Geo_FIPS <- NULL
  st.years$year <- NULL
  
  # Perform (temporal) hierarchical clustering
  years.thc <- st.years %>% dist(method = "euclidean") %>% hclust(method = linkage)
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf(paste("data/clusters.pdf", sep = ""), width = nclass)
  sfbact.dend <- years.thc %>% 
    as.dendrogram %>% 
    set("branches_k_color", k = nclass, value=colors) %>% 
    plot
  dev.off()
  
  # Find the clusters and assign them to the corresponding Census Tract
  # k = number of classes (where to cut the tree)
  years.thc.cuts <- data.frame(cutree(years.thc, k=nclass))
  
  years.thc.clusters <- cbind(years.geo, years.thc.cuts) %>% 
    select(Geo_FIPS, year = year, cluster = cutree.years.thc..k...nclass.)
  
  # Export to CSV
  for (i in 1:length(years)) {
    df <- filter(years.thc.clusters, year == years[i])
    write.csv(df, 
              file=paste("data/clusters/sfbact-clusters-", 
                         linkage, "-", 
                         nclass, "-",
                         years[i], 
                         ".csv", 
                         sep=""), row.names = FALSE)
  }
  years.thc.clusters
  } # clusteringAllYears()







# Not used
clusteringByYear <- function(df, year) {
  
  ## !! REMOVING OF OUTLIERS !! #######################
  #   if (year == "2014") {
  #     df <- df[!(df$Geo_FIPS == "6095252802"),]
  #     df <- df[!(df$Geo_FIPS == "6095252801"),]
  #   } else if (year == "2010") {
  #     df <- df[!(df$Geo_FIPS == "6095252801"),]
  #     df <- df[!(df$Geo_FIPS == "6055200900"),]
  #     df <- df[!(df$Geo_FIPS == "6001420400"),]
  #   } else {
  #     df <- df[!(df$Geo_FIPS == "6055200900"),]
  #     df <- df[!(df$Geo_FIPS == "6041122000"),]
  #   }
  
  ## XX REMOVING OF OUTLIERS XX #######################  
  
  df.geo <- as.data.frame(df$Geo_FIPS)
  names(df.geo) <- "Geo_FIPS"
  #print(df.geo)
  df$Geo_FIPS <- NULL
  
  # calculate the distance matrix,
  # and perform the clustering
  sfbact.hc.complete <- df %>%
    dist(method = "euclidean") %>% hclust(method = linkage) 
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf(paste("data/censushclust/dendrogram-", year, ".pdf", sep = ""), width = 15)
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
            file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
                       year, 
                       ".csv", 
                       sep = ""))
  sfbact.hc.clusters
} # ClusteringByYears()