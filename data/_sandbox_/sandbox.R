


################################################
## Principal component analysis ################
################################################


library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)


x2014 <- sfbact.cc.2014.st[2:25]
x2010 <- sfbact.cc.2010.st[2:25]
x2000 <- sfbact.cc.2000.st[2:25]

pca2014 <- prcomp(x2014, center = TRUE, scale. = TRUE)
pca2010 <- prcomp(x2010, center = TRUE, scale. = TRUE)
pca2000 <- prcomp(x2000, center = TRUE, scale. = TRUE)

plot(pca2014, type = "l")
summary(pca2014)

pca2014
# 
# g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
#               groups = ir.species, ellipse = TRUE, 
#               circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

biplot(pca)

predictions2014 <- as.data.frame(predict(pca2014, x2014))
predictions2010 <- as.data.frame(predict(pca2010, x2010))
predictions2000 <- as.data.frame(predict(pca2000, x2000))

x2014 <- cbind(sfbact.2014.var.cl, predictions2014)
x2010 <- cbind(sfbact.2010.var.cl, predictions2010)
x2000 <- cbind(sfbact.2000.var.cl, predictions2000)

write.csv(x2014, "data/censushclust/pca/pca2014.csv", row.names = FALSE)
write.csv(x2010, "data/censushclust/pca/pca2010.csv", row.names = FALSE)
write.csv(x2000, "data/censushclust/pca/pca2000.csv", row.names = FALSE)



################################################
## Spatial overlap of clusters over time #######
################################################
# GET relationship file between Geo_FIPS 2000 and Geo_FIPS 2010
# geo.rel <- read.csv("data/census/relationship.csv") %>% select(d,m)
# names <- c("geo2010", "Geo_FIPS")
# names(geo.rel) <- names
# sfbact.2000.hc.cl <- merge(sfbact.2000.hc.cl, geo.rel, by = "Geo_FIPS")
# sfbact.2000.hc.cl$Geo_FIPS <- NULL # removes the 2000 Geo_FIPS
# names <- c("cluster", "Geo_FIPS")
# names(sfbact.2000.hc.cl) <- names
# 
# # merge all clusters files
# cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
# cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
# names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
# names(cl) <- names
# 
# cl.change <- cl %>% select(Geo_FIPS, cl2014, cl2010) %>% 
#   group_by(cl2014, cl2010) %>%
#   tally() # just 'tally()' count the number of occurrences without summing!


################################################
## Cluster Reassignment ########################
################################################

# 2014 is the base year. New cluster numbers are assigned to
# previous years according to the maximal number of overlapping
# tracts with 2014.

# cl.2014 <- c(1:15) # 2014 and  2010 OLD
# cl.2010.new <- c(1, 6, 3, 8, 5, 
#                  3, 4, 7, 8, 13, 
#                  10, 13, 12, 2, 5) # 2010 NEW - we lose clusters 9, 11, 14 & 15
# 
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 1] <- cl.2010.new[1]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 2] <- cl.2010.new[2]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 3] <- cl.2010.new[3]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 4] <- cl.2010.new[4]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 5] <- cl.2010.new[5]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 6] <- cl.2010.new[6]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 7] <- cl.2010.new[7]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 8] <- cl.2010.new[8]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 9] <- cl.2010.new[9]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 10] <- cl.2010.new[10]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 11] <- cl.2010.new[11]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 12] <- cl.2010.new[12]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 13] <- cl.2010.new[13]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 14] <- cl.2010.new[14]
# sfbact.2010.hc.cl$cluster[sfbact.2010.hc.cl$cluster == 15] <- cl.2010.new[15]
# 
# # Re evaluate change
# # merge all clusters files
# cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
# cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
# names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
# names(cl) <- names
# 
# cl.change <- cl %>% select(Geo_FIPS, cl2010, cl2000) %>% 
#   group_by(cl2010, cl2000) %>%
#   tally() # just 'tally()' count the number of occurrences without summing!
# 
# # New cluster numbers are assigned to
# # 2000 according to the maximal number of overlapping
# # tracts with 2010.
# 
# cl.2010 <- c(1:15) # 2010 and  2000 OLD
# cl.2000.new <- c(1, 14, 2, 3, 11, 
#                  0, 2, 6, 0, 2, 
#                  0, 2, 0, 0, 0) # 2000 NEW
# 
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 1] <- cl.2000.new[1]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 2] <- cl.2000.new[2]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 3] <- cl.2000.new[3]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 4] <- cl.2000.new[4]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 5] <- cl.2000.new[5]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 6] <- cl.2000.new[6]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 7] <- cl.2000.new[7]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 8] <- cl.2000.new[8]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 9] <- cl.2000.new[9]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 10] <- cl.2000.new[10]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 11] <- cl.2000.new[11]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 12] <- cl.2000.new[12]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 13] <- cl.2000.new[13]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 14] <- cl.2000.new[14]
# sfbact.2000.hc.cl$cluster[sfbact.2000.hc.cl$cluster == 15] <- cl.2000.new[15]
# 
# # Export to CSVs
# write.csv(sfbact.2000.hc.cl, 
#           file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
#                      "2000", 
#                      ".csv", 
#                      sep = ""))
# write.csv(sfbact.2010.hc.cl, 
#           file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
#                      "2010", 
#                      ".csv", 
#                      sep = ""))




##############################################
### MEASURE CHANGE ###########################
##############################################
# count the number of class change for each tract.

#distance between clusters 
cl.dist <- read.csv("data/censushclust/cluster-dist.csv")

# merge all clusters files
cl1 <- merge(sfbact.2000.hc.cl, sfbact.2010.hc.cl, by = "Geo_FIPS")
cl <- merge(cl1, sfbact.2014.hc.cl, by = "Geo_FIPS")
names <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014")
names(cl) <- names


cl$change <- 0 # Initiate an empty field
cl$traj <- 0 # Initiate an empty field

for (i in 1:length(cl$Geo_FIPS)) {
  a <- cl[i, 2]
  b <- cl[i, 3] + 1
  c <- cl[i, 3]
  d <- cl[i, 4] + 1
  cl[i, 5] <- cl.dist[a, b] + cl.dist[c, d] # CHANGE
}

for (i in 1:length(cl$Geo_FIPS)) {
  if (cl[i, 2] != 7 & cl[i, 4] == 7) {
    cl[i, 6] <- "traj2" # TRAJECTORY OF CHANGE
  }
  else if (cl[i, 2] == 3 & cl[i, 3] == 2 && cl[i, 4] == 2) {
    cl[i, 6] <- "traj1" # TRAJECTORY OF CHANGE
  }
  #   else if (cl[i, 2] == 3 & cl[i, 3] == 6 && cl[i, 4] == 6) {
  #     cl[i, 6] <- "traj2" # TRAJECTORY OF CHANGE
  #   }
  #   else if (cl[i, 2] == 8 & cl[i, 3] == 13 && cl[i, 4] == 13) {
  #     cl[i, 6] <- "traj3" # TRAJECTORY OF CHANGE
  #   }
  #   else if (cl[i, 2] == 13 & cl[i, 3] == 13 && cl[i, 4] == 8) {
  #     cl[i, 6] <- "traj4" # TRAJECTORY OF CHANGE
  #   }
  #   else if (cl[i, 2] == 7 & cl[i, 3] == 11 && cl[i, 4] == 11) {
  #     cl[i, 6] <- "traj5" # TRAJECTORY OF CHANGE
  #   }
  #   else if (cl[i, 2] == 7 & cl[i, 3] == 7 && cl[i, 4] == 11) {
  #     cl[i, 6] <- "traj5" # TRAJECTORY OF CHANGE
  #   }
  else {cl[i, 6] <- "traj0"}
}

str(unique(cl[,5]))

names(cl) <- c("Geo_FIPS", "cl2000", "cl2010", "cl2014", "change", "traj")

write.csv(cl, 
          paste("data/censushclust/change/", linkage, "/", nclass, "/change.csv", sep = ""))






##############################################
### CHARACTERIZE CHANGE ######################
##############################################















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



############################################################################
############################################################################
## DISTANCE 
############################################################################
############################################################################

# Specify a distance of minimum 60 units between cluster means?

st.years$memb <- cutree(years.thc, k = 15)
#st.years$memb <- NULL

# plot clusters in colors
#plot(df$x,df$y,col=df$memb)

# calculate cluster means
cent <- NULL
for(k in 1:length(unique(st.years$memb))){
  cent <- rbind(cent, colMeans(st.years[st.years$memb == k, , drop = FALSE]))
}
cent <- as.data.frame(cent)

# plot points on top of clusters
#points(cent$x,cent$y,pch=15)

cent$memb <- NULL

# Summary of distance between cluster means
d <- dist(cent[1:24])
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
toJSON(x)

# export data for scatterplot in d3
coord <- cbind(x, y) %>% melt() %>% 
  rename(id=Var1, axis=Var2, coord=value) %>% 
  toJSON(pretty=TRUE) %>% write("../COLORS/data/coord.json")
coord2 <- toJSON(cbind(x, y), pretty=TRUE) %>% write("../COLORS/data/coord2.json")

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS",  type="n")
text(x, y, labels = row.names(cent), cex=.7)


distance <- melt(as.matrix(d), varnames = c("row", "col"))
write.csv(distance, "data/censushclust/force/dist.csv", row.names = FALSE)
distance$row <- as.character(distance$row)
distance$col <- as.character(distance$col)
distance$value <- distance$value

links <- distance %>% select(source = row, target = col, value = value) %>% 
  toJSON(pretty = TRUE)
names <- c(1,2,3,
           4,
           5,
           6,
           7,
           8,
           9,
           10,
           11,
           12,
           13,
           14,
           15)
nodes <- as.data.frame(matrix(ncol=2,nrow=length(names)))
for (i in 0:length(names)) {
  nodes[i, 1] <- as.character(i)
  nodes[i, 2] <- as.character(names[i])
}
names(nodes) <- c("id", "group")
nodes <- toJSON(nodes, pretty = T)

write(paste("{\"nodes\":
              ", nodes, ",
              \"links\":
              ",links, "}", sep=""), "data/censushclust/force/force.json")
############################################################################
############################################################################