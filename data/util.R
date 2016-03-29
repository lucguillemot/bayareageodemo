# This function reads a selected number of variables 
# from the raw downloaded file from Social Explorer, 
# calculate percentages and return the variables used for clustering
# + export variables data to CSVs for each year > maps.
readAndParse <- function(year, file) {
  # Read the raw data downloaded from the Social Explorer website
  ## This is the 'Comprehensive report' for all census tracts in San Francisco
  ## The data dictionary is in the file named 'R11088304.txt'
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
           threeFourUnits=SE_T097_006,  # of housing units (housingUnits)
           fiveNineUnits=SE_T097_007,  # of housing units (housingUnits)
           tenNineteenUnits=SE_T097_008,  # of housing units (housingUnits)
           twentyFortynineUnits=SE_T097_009,  # of housing units (housingUnits)
           moreFiftyUnits=SE_T097_010,  # of housing units (housingUnits)
           
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
           movedWithinCounty=SE_T130_003, # of pop since at least a year in the US (popOneYearInUS)
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
          PCmoreThreeUnits=(threeFourUnits+fiveNineUnits+tenNineteenUnits+twentyFortynineUnits+moreFiftyUnits)/housingUnits,
          
          PCcommutingNotCar=(commutingPT+commutingBicycle+commutingWalked)/workersM16,
          #PCcommutingL45min=commutingL45min/workersM16NotHome,
          #PCcommuteM45=commuteM45/workersM16NotHome,
          #PCcommuteM60=commuteM60/workersM16NotHome,
          #PCcommuteM90=commuteM90/workersM16NotHome,
          
          ## stability
          PCsameHouseOneYearAgo=SameHouseOneYearAgo/popOneYearInUS,
          PCmovedWithinCounty=movedWithinCounty/popOneYearInUS,
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
#                    PCmovedWithinCounty,
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
             PCveryWealthyHHolds,
             PCmovedWithinCounty,
             #PCmoreThreeUnits,
             medianYearBuilt 
             )

  # Scatterplots to test collinearity
  #plot(sfbact$PCraceWhite, sfbact$PCraceBlack)
  #pairs(sfbact[,c(9,10,11,12,13)])
  
  #remove empty columns (there shouldn't be any)
  sfbact.ne <- Filter(function(x)!all(is.na(x)), sfbact)
####sfbact.ne <- sfbact

# Add two missing values for Median Year Built in Berkeley for 2013 and 2014
#   if (year==2014) {
#     #sfbact.ne$medianYearBuilt[sfbact.ne$Geo_FIPS="6001422600"] <- 1939
#     sfbact.ne <- within(sfbact.ne, medianYearBuilt[Geo_FIPS == "6001422600"] <- 1939)
#     print("year 2014")
#     #within(df, Name[Name == 'John Smith' & State == 'WI'] <- 'John Smith1')
#   }

  # extract complete cases
  sfbact.cc.pop <- sfbact.ne[complete.cases(sfbact.ne),]

  # Remove census tracts with less than 1000 inhabitants
  # and remove the column with total population
  sfbact.cc <- sfbact.cc.pop[!(sfbact.cc.pop$totalPop < 1000),]

  sfbact.cc$totalPop <- NULL

  # Remove one tract in NAPA that is an outlier
  # (very poor and old population), always
  # clusters in its one cluster...
#   sfbact.cc <- sfbact.cc[!(sfbact.cc$Geo_FIPS == "6055200900"),]
  
  
  

#   sfbact.cc <- mutate(sfbact.cc,year=year)

#   # Export data to a csv file
#   write.csv(sfbact, 
#             file=paste("data/var/sfbact-var-",
#                        year, 
#                        ".csv", 
#                        sep=""))
  sfbact.cc
}

###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################

readAndParseCensusData <- function(year, file) {
  # Read the raw data downloaded from the Social Explorer website
  ## This is the 'Comprehensive report' for all census tracts in San Francisco
  ## The data dictionary is in the file named 'R11088304.txt'
  
#   file <- "R11097587_SL140-2010-Census.csv"
#   year <- 2010
  
  
  df <- read.csv(paste("data/census/", file, sep = ""))
  
  # Select the relevant variables and calculate relative values (%)
  sfbact <- df %>%
    select(Geo_FIPS,
           #POPULATION
           totalPop=SE_T001_001,
           ## age
           medianAge=SE_T013_001, ##
           ## sex
           sexMale=SE_T005_002,  # of total pop (totalPop)
           ## race
           raceWhiteAlone=SE_T014_002, # of total pop (totalPop)
           raceBlackAlone=SE_T014_003, # of total pop
           raceAsianAlone=SE_T014_005, # of total pop
           raceHispanic=SE_T015_010, # of total pop
           ## education
           popM25=SE_T040_001, ## # highest attainment
           lessHighSchool=SE_T040_002, # of pop more than 25
           someCollegeOrMore=SE_T040_004, # of pop more than 25
           doctorate=SE_T040_008, # of pop more than 25
           ## family structure
           households=SE_T020_001, ##
           marriedCouple=SE_T020_003, # of households
           withKids=SE_T021_002, # of households
           unmarriedSSCouple=SE_T028_003, # of households
           ## origin
           foreignBorn=SE_T201_003, # of total population (totalPop)
           # ENVIRONMENT
           ## housing
           housingUnits=SE_T159_001, ##
           occupiedHousingUnits=SE_T156_001, ##
           ownerOccUnits=SE_T156_002, # of occupied units (occupiedHousingUnits)
           medianYearBuilt=SE_T160_001, ##
           #housesMedianValue=SE_T101_001, ##
           ## urban environement
           oneUnit=SE_T159_002, # of housing units (housingUnits)
           density=SE_T003_001, ##
           workersM16=SE_T195_001, ##
           commutingPT=SE_T195_003, # workers 16 years and over (workersM16)
           commutingBicycle=SE_T195_005, # workers 16 years and over (workersM16)
           commutingWalked=SE_T195_006, # workers 16 years and over (workersM16)
           averageCommute=SE_T218_001, ## 
           ## stability
           popOneYearInUS=SE_T197_004, ##
           movedWithinCounty=SE_T197_005, # of pop since at least a year in the US (popOneYearInUS)
           # ECONOMY
           ## occupation
           laborForceM16=SE_T069_004, ##
           unemployed=SE_T069_006, # of population in labor force 16+ (laborForceM16)
           ## wealth
           veryWealthyHHolds=SE_T092_017, # of households (households)
           withInterests=SE_T139_002, # of households (households)
           #withSSIncome=T078_002, # of households (households)
           ## inequality
           #ONLY available 2011:2014 gini=SE_T157_001, ##
           perCapitaIncome=SE_T145_001, ##
           PopWithPovStatus=SE_T185_001, ##
           poorStruggling=SE_T185_004 # of pop with poverty status (PopWithPovStatus)
           #ONLY AVAILABLE 2011:2014 civilNonInstitPop=SE_T145_001 ##
           #ONLY AVAILABLE 2011:2014 noHealthInsurrance=SE_T145_002 # of civil non institutionalized pop (civilNonInstitPop)
    ) %>% 
    mutate(#POPULATION
      ## sex
      PCsexMale=sexMale/totalPop,
      ## race
      PCraceWhiteAlone=raceWhiteAlone/totalPop,
      PCraceBlackAlone=raceBlackAlone/totalPop,
      PCraceAsianAlone=raceAsianAlone/totalPop,
      PCraceHispanic=raceHispanic/totalPop,
      ## education
      PClessHighSchool=lessHighSchool/popM25,
      PCsomeCollegeOrMore=someCollegeOrMore/popM25,
      PCdoctorate=doctorate/popM25,
      ## family structure
      PCmarriedCouple=marriedCouple/households,
      PCwithKids=withKids/households,
      PCunmarriedSSCouple=unmarriedSSCouple/households,
      ## origin
      PCforeignBorn=foreignBorn/totalPop,
      # ENVIRONMENT
      ## housing
      PCownerOccUnits=ownerOccUnits/occupiedHousingUnits,
      ## urban environement
      PConeUnit=oneUnit/housingUnits,
      PCcommutingNotCar=(commutingPT+commutingBicycle+commutingWalked)/workersM16,
      ## stability
      PCmovedWithinCounty=movedWithinCounty/popOneYearInUS,
      # ECONOMY
      ## occupation
      PCunemployed=unemployed/laborForceM16,
      ## wealth
      PCveryWealthyHHolds=veryWealthyHHolds/households,
      PCwithInterests=withInterests/households,
      #PCwithSSIncome=withSSIncome/households,
      ## inequality
      PCpoorStruggling=poorStruggling/PopWithPovStatus
      #PCnoHealthInsurrance=noHealthInsurrance/civilNonInstitPop
    ) %>%
 #### SELECTION OF VARIABLES ######
  select(Geo_FIPS,
         totalPop,
         density,
         
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
         #medianIncome, 
         PCunemployed,
         PCpoorStruggling,
         PCveryWealthyHHolds,
         PCmovedWithinCounty,
         #PCmoreThreeUnits,
         medianYearBuilt 
  )
  print(sfbact[sfbact$Geo_FIPS == "06075011500"])
  # Scatterplots to test collinearity
  #plot(sfbact$PCraceWhite, sfbact$PCraceBlack)
  #pairs(sfbact[,c(9,10,11,12,13)])
  
  #remove empty columns (there shouldn't be any)
  sfbact.ne <- Filter(function(x)!all(is.na(x)), sfbact)
    
  # extract complete cases
  sfbact.cc.pop <- sfbact.ne[complete.cases(sfbact.ne),]
  
  # Remove census tracts with less than 100 inhabitants
  # and remove the column with total population
  sfbact.cc <- sfbact.cc.pop[!(sfbact.cc.pop$totalPop < 1000),]
  
  sfbact.cc$totalPop <- NULL
  
  # Remove one tract in NAPA that is an outlier
  # (very poor and old population), always
  # clusters in its one cluster...
  #sfbact.cc <- sfbact.cc[!(sfbact.cc$Geo_FIPS == "6055200900"),]
  
 # Remove NA tracts for 2000
#  sfbact.cc <- sfbact.cc[!(sfbact.cc$Geo_FIPS == "6081604300"),]
#  sfbact.cc <- sfbact.cc[!(sfbact.cc$Geo_FIPS == "6095253000"),]
  
  
  
  ##sfbact.cc <- mutate(sfbact.cc,year=year)
  
  #   # Export data to a csv file
  #   write.csv(sfbact, 
  #             file=paste("data/var/sfbact-var-",
  #                        year, 
  #                        ".csv", 
  #                        sep=""))
  ##sfbact.cc
 sfbact.cc
} # ReadAndParseCensusData()

###################################################

###################################################
# Standardize data for variables that don't have
###################################################
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
  
  #df <- sfbact.cc.2000
  df$year <- NULL
  names <- names(df)
  df.geo <- as.data.frame(df$Geo_FIPS)
  df$Geo_FIPS <- NULL
  
  # Standardize data by the square root
  df <- as.data.frame(apply(df,2,sqrt))
  #df <- scale(df, center = TRUE, scale = TRUE)
  
  ##STANDARDIZE DATA TO A 0-1 RANGE
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  df <- apply(df, MARGIN=2, FUN=range01)
  
  df <- cbind(df.geo,df)
  names(df) <- names
  df <- as.data.frame(df)
} # StandardizeData()


###################################################
### HIERARCHICAL CLUSTERING #######################
###################################################
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




clusteringAllYears <- function(st.2000, st.2010, st.2014) {
  
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
  st.years <- st.years[!(st.years$Geo_FIPS == "6055200900"),] # NAPA
  st.years <- st.years[!(st.years$Geo_FIPS == "6041122000"),] # Saint-Quentin prison
  ## XX REMOVING OF OUTLIERS XX #######################  
  
  years.geo <- st.years %>% select(Geo_FIPS,year)
  
  st.years$Geo_FIPS <- NULL
  st.years$year <- NULL
  
  # Perform (temporal) hierarchical clustering
  years.thc <- st.years %>% dist(method = "euclidean") %>% hclust(method = linkage)
  
  # Display and export the dendrogram
  ## The colors don't correspond to those on the map...
  pdf(paste("data/censushclust/dendrogram.pdf", sep = ""), width = 15)
  sfbact.dend <- years.thc %>% 
    as.dendrogram %>% 
    set("branches_k_color", k = nclass, value=colors) %>% 
    plot
  dev.off()
############################################################################  
# TO GET DISTANCE BETWEEN CLUSTER                                          #
#   years.thc$height                                                       #
# dend <- as.dendrogram(years.thc)                                         #
# get_nodes_attr(dend, "height")                                           #
#                                                                          #
# dend2 <- cut(dend, h = 1.8)
# #str(dend2)
# plot(dend2$upper, edgePar=list())
# dend2$lower # list of heights at cut height 1.8
# plot(dend2$upper, nodePar = list(pch = c(1,7), col = 2:1))
# str(dend2$upper) # !!!
############################################################################

  # Find the clusters and assign them to the corresponding Census Tract
  # k = number of classes (where to cut the tree)
  years.thc.cuts <- data.frame(cutree(years.thc,k=nclass))
  
  years.thc.clusters <- cbind(years.geo,years.thc.cuts) %>% 
    select(Geo_FIPS, year = year, cluster = cutree.years.thc..k...nclass.)
  
  years <- c("2000", "2010", "2014")
  
  # Export to CSV
  for (i in 1:length(years)) {
    df <- filter(years.thc.clusters, year==years[i])
    write.csv(df, 
              file=paste("data/censushclust/clusters/", linkage, "/", nclass, "/sfbact-clusters-",
                         years[i], 
                         ".csv", 
                         sep=""))
  }
  years.thc.clusters
} # clusteringAllYears()

exportVarData <- function(var, clust, year) {
#   var <- sfbact.cc.2000
#   clust <- sfbact.2000.hc.cl
  sfba.var.cl <- inner_join(var, clust, by = "Geo_FIPS")
  
  # Standardize Density, Median age, Income and Median Year Built to have 0 to 1 value
  # (The others variables are percentage)
  # To facilitate their display on the map (with the same color scale for all var)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  sfba.var.cl.ranging <- as.data.frame(apply(sfba.var.cl[c(2, 12, 20, 25)], MARGIN=2, FUN=range01))
  sfba.var.cl$density <- NULL
  sfba.var.cl$medianAge <- NULL
  sfba.var.cl$perCapitaIncome <- NULL
  sfba.var.cl$medianYearBuilt <- NULL
  sfba.var.cl <- cbind(sfba.var.cl, sfba.var.cl.ranging)
  sfba.var.cl$year.y <- NULL
  colnames(sfba.var.cl)[which(names(sfba.var.cl) == "year.x")] <- "year"
  # Reorder columns
  #sfba.var.cl <- sfba.var.cl[, c(1, 24, 27, 2, 16, 3, 21, 4:6, 7, 9, 8, 10, 25, 12:14, 11, 15, 26, 18, 19, 17, 20, 23)]
  sfba.var.cl <- sfba.var.cl[, c(1, 23, 26, 2, 16, 3, 21, 4:6, 7, 9, 8, 10, 24, 12:14, 11, 15, 25, 18, 19, 17, 20, 22)]
  
  # Export CSV files
  write.csv(sfba.var.cl, 
            file=paste("data/censushclust/var/", linkage, "/", nclass, "/sfbact-var-",
                       year, 
                       ".csv", 
                       sep = ""))
  sfba.var.cl
} # exportVarData

################################################################################
## Export data for the radar plot in D3 ########################################
################################################################################
radarPlotData <- function(st, cl, year) {
#   st <- sfbact.cc.2000.st
#   cl <- sfbact.2000.hc.cl
  # join standardized data set with cluster dataset by geo ID
  stcl <- inner_join(st, cl, by="Geo_FIPS")
  stcl$year <- NULL
  stcl$totalPop <- NULL
  
  # Calculate means of each variables for the whole bay area
  stcl.means <- as.vector(apply(stcl[2:25], 2, mean))
  # stcl.means.df <- as.data.frame(apply(stcl[2:25], 2, mean))
  
  # calculate distance to the Bay Area mean for each cluster
  stcl.means.cl <- stcl[2:26] %>% 
    group_by(cluster) %>% 
    summarise_each(funs(mean))
  
  stcl.means.dev <- stcl.means.cl %>% 
    mutate(density=density-stcl.means[1],
           medianYearBuilt=medianYearBuilt-stcl.means[24],
           PConeUnit=PConeUnit-stcl.means[2],
           PCownerOccUnits=PCownerOccUnits-stcl.means[17],
           PCcommutingNotCar=PCcommutingNotCar-stcl.means[3],
           PCmovedWithinCounty=PCmovedWithinCounty-stcl.means[23],
           PClessHighSchool=PClessHighSchool-stcl.means[4],
           PCsomeCollegeOrMore=PCsomeCollegeOrMore-stcl.means[5],
           PCdoctorate=PCdoctorate-stcl.means[6],
           PCmarriedCouple=PCmarriedCouple-stcl.means[7],
           PCunmarriedSSCouple=PCunmarriedSSCouple-stcl.means[9],
           PCwithKids=PCwithKids-stcl.means[8],
           PCsexMale=PCsexMale-stcl.means[10],
           medianAge=medianAge-stcl.means[11],
           PCraceBlackAlone=PCraceBlackAlone-stcl.means[13],
           PCraceAsianAlone=PCraceAsianAlone-stcl.means[14],
           PCraceHispanic=PCraceHispanic-stcl.means[15],
           PCraceWhiteAlone=PCraceWhiteAlone-stcl.means[12],
           PCforeignBorn=PCforeignBorn-stcl.means[16],
           perCapitaIncome=perCapitaIncome-stcl.means[19],
           PCunemployed=PCunemployed-stcl.means[20],
           PCpoorStruggling=PCpoorStruggling-stcl.means[21],
           PCwithInterests=PCwithInterests-stcl.means[18],
           PCveryWealthyHHolds=PCveryWealthyHHolds-stcl.means[22]
    )
  
  # Remove 'cluster' column
  stcl.means.dev <- stcl.means.dev[,-1]
  # Reorder Columns
  stcl.means.dev <- stcl.means.dev[, c(1, 24, 2, 17, 3, 23, 4:6, 7, 9, 8, 10, 11, 13:15, 12, 16, 19, 20, 21, 18, 22)]
  # Rename variables
  Namen <- c("Density",
             "median year of construction",
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
        paste("data/censushclust/radar/", linkage, "/", nclass, "/radar-", year, ".json", sep = ""))
  # Export each cluster in a separate json file
  for (i in 1:nclass) {
    write(paste("[", toJSON(json.list[[i]], pretty = T), "]", sep = ""), paste("data/censushclust/radar/", linkage, "/", nclass, "/radar-", year, "-cl", i, ".json", sep = ""))
  }
  stcl.means.dev
} #RadarPlotData








##############################################
### EXPORT DATA FOR SANKEY DIAGRAM ###########
##############################################

# grReshape <- sfbact.hc.clusters %>% dcast(year, cluster, mean, margins=c("year", "cluster"))
# gr <- sfbact.hc.clusters %>% group_by(year, cluster) %>% summarize(tot=sum(cluster))
# 
# 
# y2010 <- sfbact.hc.clusters %>% filter(year==2010)
# y2011 <- sfbact.hc.clusters %>% filter(year==2011)
# y2012 <- sfbact.hc.clusters %>% filter(year==2012)
# y2013 <- sfbact.hc.clusters %>% filter(year==2013)
# y2014 <- sfbact.hc.clusters %>% filter(year==2014)
# 
# y1011 <- inner_join(y2010, y2011, by="Geo_FIPS")
# y1112 <- inner_join(y2011, y2012, by="Geo_FIPS")
# y1213 <- inner_join(y2012, y2013, by="Geo_FIPS")
# y1314 <- inner_join(y2013, y2014, by="Geo_FIPS")
# #y <- join_all(list(y2010, y2011, y2012, y2013, y2014), by="Geo_FIPS", type="left")
# 
# y1011 <- y1011 %>% group_by(cluster.x, cluster.y) %>%
#   summarize(tr=sum(cluster.y)) %>%
#   mutate(cluster.source=cluster.x-1, cluster.target=cluster.y+(nclass-1))
# y1112 <- y1112 %>% group_by(cluster.x, cluster.y) %>% 
#   summarize(tr=sum(cluster.y)) %>%
#   mutate(cluster.source=cluster.x+(nclass-1), cluster.target=cluster.y+((nclass*2)-1))
# y1213 <- y1213 %>% group_by(cluster.x, cluster.y) %>% 
#   summarize(tr=sum(cluster.y)) %>%
#   mutate(cluster.source=cluster.x+((nclass*2)-1), cluster.target=cluster.y+((nclass*3)-1))
# y1314 <- y1314 %>% group_by(cluster.x, cluster.y) %>% 
#   summarize(tr=sum(cluster.y)) %>%
#   mutate(cluster.source=cluster.x+((nclass*3)-1), cluster.target=cluster.y+((nclass*4)-1))
# 
# y <- rbind(y1011, y1112, y1213, y1314)
# #y$cluster.x <- NULL
# #y$cluster.y <- NULL
# 
# links <- y %>% select(source=cluster.source,target=cluster.target, value=tr) %>% 
#   toJSON(pretty = TRUE)
# 
# names <- rep(names,5)
# 
# nodes <- as.data.frame(matrix(ncol=2,nrow=length(names)))
# for (i in 0:length(names)) {
#   nodes[i, 1] <- i-1
#   nodes[i, 2] <- names[i]
# }
# names(nodes) <- c("node", "name")
# nodes <- toJSON(nodes, pretty = T)
# 
# write(paste("{\"nodes\":
# ", nodes, ",
# \"links\":
# ",links, "}", sep=""), "data/sankey/sankey.json")