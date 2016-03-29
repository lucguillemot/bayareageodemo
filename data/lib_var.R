library(dendextend)
# library(plyr)
library(dplyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(jsonlite)
library(reshape2)

# Colors for the dendrogram built later with 'dendextend'
# colors <- c("#18154C","#EC501C","#7F1B4F","#25318C",
#             "#1A86C3","#E71584","#4F3089","#FEF30C","#E6211C","#17ABA1")
#colors <- c("#18154C","#EC501C","#7F1B4F","#005C37","#1A86C3","#E71584","#4F3089","#FEF30C","#E6211C","#17ABA1", "#b15928", "#f69c45", "#bb5fa1", "#000000", "#b0c1c9")
colors <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#dedede', '#ffed6f', '#e0f3f8', '#ffa696')
years <- c(
  "2009",
  "2010","2011","2012","2013","2014")
files <- c(
  "R11097581_SL140-2009",
           "R11097565_SL140-2010",
           "R11097561_SL140-2011",
           "R11097559_SL140-2012", 
           "R11097554_SL140-2013",
           "R11088321_SL140-2014")
names <- c("Cluster One",
           "Cluster Two",
           "Cluster Three",
           "Cluster Four",
           "Cluster Five",
           "Cluster Six",
           "Cluster Seven",
           "Cluster Eight")