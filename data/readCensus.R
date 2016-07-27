###################################################
# These two functions read variables (ACS + decennial data)
# from the raw Census files (downloaded through Social Explorer), 
# calculate percentage values when applicable,
# select and return the variables used for clustering.
###################################################

# ACS DATA #####
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
    mutate( # calculate relative values
      #POPULATION
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
      #### SELECTION OF VARIABLES ######
      select(Geo_FIPS,
         totalPop,
         density,
         medianYearBuilt,
         PConeUnit,
         PCownerOccUnits,
         PCcommutingNotCar,
         PCmovedWithinCounty,
         #averageCommute,
         PClessHighSchool,
         PCsomeCollegeOrMore,
         PCdoctorate,
         PCmarriedCouple,
         PCunmarriedSSCouple,
         PCwithKids,
         PCsexMale,
         medianAge,
         PCraceBlackAlone,
         PCraceAsianAlone,
         PCraceHispanic,
         PCraceWhiteAlone,
         PCforeignBorn,
         #medianOccupiedHouseValue,
         perCapitaIncome,
         PCunemployed,
         PCpoorStruggling,
         PCwithInterests,
         #medianIncome, ## Berkeley missing...
         PCveryWealthyHHolds
  )
 
  #remove empty columns (there shouldn't be any)
  sfbact.ne <- Filter(function(x)!all(is.na(x)), sfbact)
  
  # extract complete cases
  sfbact.cc.pop <- sfbact.ne[complete.cases(sfbact.ne),]
  
  # Remove census tracts with less than 1000 inhabitants
  # and remove the column with total population
  sfbact.cc <- sfbact.cc.pop[!(sfbact.cc.pop$totalPop < 1000),]
  sfbact.cc$totalPop <- NULL

  sfbact.cc
}

###################################################
# Decennial Census data (codes are slightly different 
# than in the ACS files)
###################################################

readAndParseCensusData <- function(year, file) { # for decennial census data
  # Read the raw data downloaded from the Social Explorer website
  ## This is the 'Comprehensive report' for all census tracts in San Francisco
  ## The data dictionary is in the file named 'R11088304.txt'
 
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
    mutate( # calculate relative values
      #POPULATION
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
         medianYearBuilt,
         PConeUnit,
         PCownerOccUnits,
         PCcommutingNotCar,
         #averageCommute,
         PCmovedWithinCounty,
         PClessHighSchool,
         PCsomeCollegeOrMore,
         PCdoctorate,
         PCmarriedCouple,
         PCunmarriedSSCouple,
         PCwithKids,
         PCsexMale,
         medianAge,
         PCraceBlackAlone,
         PCraceAsianAlone,
         PCraceHispanic,
         PCraceWhiteAlone,
         PCforeignBorn,
         #medianOccupiedHouseValue,
         perCapitaIncome,
         PCunemployed,
         PCpoorStruggling,
         PCwithInterests,
         #medianIncome,
         PCveryWealthyHHolds
  )  
  #remove empty columns (there shouldn't be any)
  sfbact.ne <- Filter(function(x)!all(is.na(x)), sfbact)
  
  # extract complete cases
  sfbact.cc.pop <- sfbact.ne[complete.cases(sfbact.ne),]
  
  # Remove census tracts with less than 100 inhabitants
  # and remove the column with total population
  sfbact.cc <- sfbact.cc.pop[!(sfbact.cc.pop$totalPop < 1000),]
  
  sfbact.cc$totalPop <- NULL
 
  sfbact.cc
} # ReadAndParseCensusData()