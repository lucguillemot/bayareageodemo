library(dendextend)
# library(plyr)
library(dplyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(jsonlite)
library(reshape2)

# Colors for the dendrogram built later with 'dendextend'
colors <- c("#18154C","#EC501C","#7F1B4F","#25318C",
            "#1A86C3","#E71584","#4F3089","#FEF30C","#E6211C","#17ABA1")
years <- c(
  #"2009",
  "2010","2011","2012","2013","2014")
files <- c(
  #"R11097581_SL140-2009",
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