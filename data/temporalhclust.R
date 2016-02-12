
################################################
## Hclust for all years all together ###########
################################################
for (i in 1:length(years)) {
  name <- paste("sfbact.cc.", years[i], sep="")
  assign(name, readAndParse(years[i], files[i]))
}

sfbact.cc.years <- rbind(sfbact.cc.2010, 
                         sfbact.cc.2011, 
                         sfbact.cc.2012,
                         sfbact.cc.2013,
                         sfbact.cc.2014)



sfbact.geo <- sfbact.cc.years %>% select(Geo_FIPS,year)
sfbact.cc.years$Geo_FIPS <- NULL
sfbact.cc.years$year <- NULL

sfbact.hc.scaled <- sfbact.cc.years %>% scale

sfbact.hc.complete <- sfbact.hc.scaled %>% dist(method="euclidean") %>% hclust(method="complete") 

# Display and export the dendrogram
## The colors don't correspond to those on the map...
pdf("data/dendrogram.pdf")
sfbact.dend <- sfbact.hc.complete %>% 
  as.dendrogram %>% set("branches_k_color", k = 8, value=colors) %>% 
  plot
dev.off()

sfbact.hc.complete.cuts <- data.frame(cutree(sfbact.hc.complete,k=nclass))

sfbact.hc.clusters <- cbind(sfbact.geo,sfbact.hc.complete.cuts) %>% 
  select(Geo_FIPS,
         year,
         cluster=cutree.sfbact.hc.complete..k...nclass.)

# Export to CSV
for (i in 1:length(years)) {
  df <- filter(sfbact.hc.clusters, year==years[i])
  write.csv(df, 
            file=paste("data/hclust/sfbact-clusters-",
                       years[i], 
                       ".csv", 
                       sep=""))
}