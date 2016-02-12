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
years <- c("2009","2010","2011","2012","2013","2014")
files <- c("R11097581_SL140-2009",
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

year <- "2014"
file <- "R11088321_SL140-2014"
nclass <- 10


# This function reads a selected number of variables 
# from the raw downloaded file from Social Explorer, 
# calculate percentages and return the variables used for clustering
# + export variables data to CSVs for each year > maps.
readAndParse <- function(year, file) {
  # Read the raw data downloaded from the Social Explorer website
  ## This is the 'Comprehensive report' for all census tracts in San Francisco
  ## The data dictionary is in the file named 'R11088304.txt'
  df <- read.csv("data/census/R11088321_SL140-2014.csv")
  df <- read.csv(paste("data/census/", file, ".csv", sep = ""))
  
  # Select the relevant variables and calculate relative values (%)
  sfbact <- df %>%
    select(Geo_FIPS,
           #POPULATION
           totalPop=SE_T001_001,
           ## age
           ageL18=SE_T009_002, # of total pop
           age1824=SE_T009_003, # of total pop
           age2575=SE_T009_004, # of total pop
           ageM65=SE_T009_005, # of total pop
           ageM75=SE_T007B011, # of total pop
           medianAge=SE_T012_001, ##
           ## sex
           sexMale=SE_T004_002,  # of total pop (totalPop)
           ageMedianBySex=SE_T012_002, ##
           ## race
           raceWhiteAlone=SE_T013_002, # of total pop (totalPop)
           raceBlackAlone=SE_T013_003, # of total pop
           raceNativeAlone=SE_T013_004, # of total pop
           raceAsianAlone=SE_T013_005, # of total pop
           raceHispanic=SE_T014_010, # of total pop
           ## education
           popM25=SE_T150_001, ## # highest attainment
           lessHighSchool=SE_T150_002, # of pop more than 25
           someCollegeOrMore=SE_T150_004, # of pop more than 25
           bachelorOrMore=SE_T150_005, # of pop more than 25
           #bachelor=SE_T150_005, # of pop more than 25
           master=SE_T150_006, # of pop more than 25
           doctorate=SE_T150_008, # of pop more than 25
           civilPop1619=SE_T030_001, ##
           dropOutRate=SE_T030_002, # of civilian pop 16 to 19 years old
           
           popOver16=SE_T033_001, ##
           inArmedForces=SE_T033_003, # of population 16 and over
           ## family structure
           households=SE_T017_001, ##
           marriedCouple=SE_T017_003, # of households
           withKids=SE_T018_002, # of households
           unmarriedSSCouple=SE_T023_003, # of households
           
           divorced=SE_T022_004, # of pop more than 25 years old (popM25)
           separated=SE_T022_006, # of pop more than 25 years old (popM25)
           popGroupQuarters=SE_T024_002, # of total population (totalPop)
           ## origin
           foreignBorn=SE_T133_003, # of total population (totalPop)
           ArrivedB1990=SE_T134_005, # of foreign-born pop (foreignBorn)
           # ENVIRONMENT
           ## housing
           housingUnits=SE_T093_001, ##
           occupiedHousingUnits=SE_T094_001, ##
           ownerOccUnits=SE_T094_002, # of occupied units (occupiedHousingUnits)
           vacantHousingUnits=SE_T096_001, ##
           forRentVacantUnits=SE_T096_002, # of vacant housing units (vacantHousingUnits)
           medianYearBuilt=SE_T098_001, ##
           #housesMedianValue=SE_T101_001, ##
           ## urban environement
           oneUnit=SE_T097_002, # of housing units (housingUnits)
           density=SE_T002_002, ##
           workersM16=SE_T128_001, ##
           commutingPT=SE_T128_003, # workers 16 years and over (workersM16)
           commutingBicycle=SE_T128_005, # workers 16 years and over (workersM16)
           commutingWalked=SE_T128_006, # workers 16 years and over (workersM16)
           ## These two only in 2010-2014:
           #workersM16NotHome=SE_T158A001, ##
           #commutingL45min=SE_T158A005, # of workers 16+ not working at home (workersM16NotHome)
           
           averageCommute=SE_T147_001, ##
           #commuteM45=SE_T158B005, # of workers 16+ not working at home (workersM16NotHome)
           #commuteM60=SE_T158B006, # of workers 16+ not working at home (workersM16NotHome)
           #commuteM90=SE_T158B007, # of workers 16+ not working at home (workersM16NotHome)
           ## stability
           popOneYearInUS=SE_T130_001, ##
           SameHouseOneYearAgo=SE_T130_002, # of pop since at least a year in the US (popOneYearInUS)
           # ECONOMY
           ## occupation
           laborForceM16=SE_T037_001, ##
           unemployed=SE_T037_003, # of population in labor force 16+ (laborForceM16)
           employedPopM16=SE_T053_001, ##      
           privateSector=SE_T053_002, # of employed pop 16+ (employedPopM16)
           ## wealth
           medianIncome=SE_T057_001, ##
           veryWealthyHHolds=SE_T056_017, # of households (households)
           medianOccupiedHouseValue=SE_T101_001, ##
           withEarnings=SE_T074_002, # of households (households)
           withInterests=SE_T077_002, # of households (households)
           #withSSIncome=T078_002, # of households (households)
           ## inequality
           #ONLY available 2011:2014 gini=SE_T157_001, ##
           perCapitaIncome=SE_T083_001, ##
           publicAssIncome=SE_T080_002, # of households (households)
           PopWithPovStatus=SE_T118_001, ##
           poorStruggling=SE_T118_004 # of pop with poverty status (PopWithPovStatus)
           #ONLY AVAILABLE 2011:2014 civilNonInstitPop=SE_T145_001 ##
           #ONLY AVAILABLE 2011:2014 noHealthInsurrance=SE_T145_002 # of civil non institutionalized pop (civilNonInstitPop)
    ) %>% 
    mutate(#POPULATION
          ## age
          PCageL18=ageL18/totalPop,
          PCage1824=age1824/totalPop,
          PCage2575=age2575/totalPop,
          PCageM75=ageM75/totalPop,
          PCageM65=ageM65/totalPop,
          ## sex
          PCsexMale=sexMale/totalPop,
          ## race
          PCraceWhiteAlone=raceWhiteAlone/totalPop,
          PCraceBlackAlone=raceBlackAlone/totalPop,
          PCraceNativeAlone=raceNativeAlone/totalPop,
          PCraceAsianAlone=raceAsianAlone/totalPop,
          PCraceHispanic=raceHispanic/totalPop,
          ## education
          PClessHighSchool=lessHighSchool/popM25,
          PCsomeCollegeOrMore=someCollegeOrMore/popM25,
          PCbachelorOrMore=bachelorOrMore/popM25,
          #PCbachelor=bachelor/popM25,
          PCmaster=master/popM25,
          PCdoctorate=doctorate/popM25,
          PCdropOutRate=dropOutRate/civilPop1619,
          ## family structure
          PCmarriedCouple=marriedCouple/households,
          PCwithKids=withKids/households,
          PCunmarriedSSCouple=unmarriedSSCouple/households,
          
          PCdivorced=divorced/popM25,
          PCseparated=separated/popM25,
          PCpopGroupQuarters=popGroupQuarters/totalPop,
          ## origin
          PCforeignBorn=foreignBorn/totalPop,
          PCArrivedB1990=ArrivedB1990/foreignBorn,
          # ENVIRONMENT
          ## housing
          PCownerOccUnits=ownerOccUnits/occupiedHousingUnits,
          PCforRentVacantUnits=forRentVacantUnits/vacantHousingUnits,
          ## urban environement
          PConeUnit=oneUnit/housingUnits,
          PCcommutingNotCar=(commutingPT+commutingBicycle+commutingWalked)/workersM16,
          #PCcommutingL45min=commutingL45min/workersM16NotHome,
          #PCcommuteM45=commuteM45/workersM16NotHome,
          #PCcommuteM60=commuteM60/workersM16NotHome,
          #PCcommuteM90=commuteM90/workersM16NotHome,
          
          ## stability
          PCsameHouseOneYearAgo=SameHouseOneYearAgo/popOneYearInUS,
          # ECONOMY
          ## occupation
          PCunemployed=unemployed/laborForceM16,
          PCprivateSector=privateSector/employedPopM16,
          ## wealth
          PCveryWealthyHHolds=veryWealthyHHolds/households,
          PCwithEarnings=withEarnings/households,
          PCwithInterests=withInterests/households,
          #PCwithSSIncome=withSSIncome/households,
          ## inequality
          PCpublicAssIncome=publicAssIncome/households,
          PCpoorStruggling=poorStruggling/PopWithPovStatus
          #PCnoHealthInsurrance=noHealthInsurrance/civilNonInstitPop
    ) %>%
#             select(Geo_FIPS,
#                    #POPULATION
#                    ## age
#                    PCageL18,
#                    PCage1824,
#                    PCage2575,
#                    PCageM65,
#                    PCageM75,
#                    medianAge,
#                    ## sex
#                    PCsexMale,
#                    ageMedianBySex, ##
#                    ## race
#                    PCraceWhiteAlone,
#                    PCraceBlackAlone,
#                    PCraceNativeAlone,
#                    PCraceAsianAlone,
#                    PCraceHispanic,
#                    ## education
#                    PClessHighSchool,
#                    PCsomeCollegeOrMore,
#                    PCbachelorOrMore,
#                    #PCbachelor,
#                    PCmaster,
#                    PCdoctorate,
#                    PCdropOutRate,
#                    ## family structure
#                    PCmarriedCouple,
#                    PCwithKids,
#                    PCunmarriedSSCouple,
#                    PCdivorced,
#                    PCseparated,
#                    PCpopGroupQuarters,
#                    ## origin
#                    PCforeignBorn,
#                    PCArrivedB1990,
#                    # ENVIRONMENT
#                    ## housing
#                    housingUnits, ##
#                    PCownerOccUnits,
#                    PCforRentVacantUnits,
#                    medianYearBuilt, ##
#                    #housesMedianValue,
#                    ## urban environement
#                    PConeUnit,
#                    density, ##
#                    PCcommutingNotCar,
#                    #PCcommutingL45min,
#                    #PCcommuteM45,
#                    #PCcommuteM60,
#                    averageCommute, ##
#                    ## stability
#                    PCsameHouseOneYearAgo,
#                    # ECONOMY
#                    ## occupation
#                    PCunemployed,    
#                    PCprivateSector,
#                    ## wealth
#                    medianIncome, ##
#                    PCveryWealthyHHolds,
#                    PCwithEarnings,
#                    PCwithInterests,
#                    #PCwithSSIncome,
#                    medianOccupiedHouseValue, ##
#                    ## inequality
#                    #gini, ##
#                    perCapitaIncome, ##
#                    PCpublicAssIncome,
#                    PCpoorStruggling
#                    #PCnoHealthInsurrance
#             )


#### SELECTION OF VARIABLES ######
      select(Geo_FIPS,
             totalPop,
             density,
             #medianYearBuilt, ## Berkeley mising...
             PConeUnit,
             PCcommutingNotCar,
             #averageCommute,
             PClessHighSchool,
             PCsomeCollegeOrMore,
             PCdoctorate,
             PCmarriedCouple,
             PCwithKids,
             PCunmarriedSSCouple,
             PCsexMale,
             medianAge,
             PCraceWhiteAlone,
             PCraceBlackAlone,
             PCraceAsianAlone,
             PCraceHispanic,
             PCforeignBorn,
             #medianOccupiedHouseValue,
             PCownerOccUnits,
             PCwithInterests,
             perCapitaIncome,
             #medianIncome, ## Berkeley missing...
             PCunemployed,
             PCpoorStruggling,
             PCveryWealthyHHolds)
      
  # Scatterplots to test collinearity
  #plot(sfbact$PCraceWhite, sfbact$PCraceBlack)
  #pairs(sfbact[,c(9,10,11,12,13)])
  
  #remove empty columns (there shouldn't be any)
  sfbact.ne <- Filter(function(x)!all(is.na(x)), sfbact)
  # extract complete cases
  sfbact.cc.pop <- sfbact.ne[complete.cases(sfbact.ne),]
  # Remove census tracts with less than 100 inhabitants
  # and remove the column with total population
  sfbact.cc <- sfbact.cc.pop[!(sfbact.cc.pop$totalPop<100),]
  sfbact.cc$totalPop <- NULL

  sfbact.cc <- mutate(sfbact.cc,year=year)

  # Export data to a csv file
  write.csv(sfbact, 
            file=paste("data/var/sfbact-var-",
                       year, 
                       ".csv", 
                       sep=""))
  sfbact.cc
}


# Standardize data for variables that don't have
# a gaussian distribution using the square root.
standardizeData <- function(df) {
#       df$density <- sqrt(df$density)
#       df$PCcommutingNotCar <- sqrt(df$PCcommutingNotCar)
#       df$PClessHighSchool <- sqrt(df$PClessHighSchool)
#       df$PCdoctorate <- sqrt(df$PCdoctorate)
#       df$PCunmarriedSSCouple <- sqrt(df$PCunmarriedSSCouple) # or LOG
#       df$PCraceBlackAlone <- sqrt(df$PCraceBlackAlone)
#       df$PCraceAsianAlone <- sqrt(df$PCraceAsianAlone)
#       df$PCraceHispanic <- sqrt(df$PCraceHispanic)
#       df$PCunemployed <- sqrt(df$PCunemployed)
#       df$PCpoorStruggling <- sqrt(df$PCpoorStruggling)
#       df$PCveryWealthyHHolds <- sqrt(df$PCveryWealthyHHolds)
  df$year <- NULL
  names <- names(df)
  df.geo <- as.data.frame(df$Geo_FIPS)
  df$Geo_FIPS <- NULL
  
  # Standardize data by the sqaure root
  df <- as.data.frame(apply(df,2,sqrt))
  #df <- scale(df, center = TRUE, scale = TRUE)
  
  ##STANDARDIZE DATA TO A 0-1 RANGE
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  df <- apply(df, MARGIN=2, FUN=range01)
  
  df <- cbind(df.geo,df)
  names(df) <- names
  df <- as.data.frame(df)
}
