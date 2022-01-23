#   ********************************************************************
#                               FILE OVERVIEW
#   ********************************************************************
#
#   File Name:      BRFSS Cleaned Data
#   File Author:    Esteban Valencia
#   File Location:  U:\EPI 514\BRFSS Data
#   File History:   Created on 05/14/19
#   File Purpose:   This file concerns the downloading, preliminary cleaning,
#                   and preparation of the BRFSS 2015, 2016, & 2017 data sets
#                   for the purpose of merging into a final analytic dataset
#                   to be used in the EPI 514 Emerging Adult Caregiver Mental
#                   & Behavioral Health Project. 
#   *********************************************************************

# [0.0] File Setup --------------------------------------------------------

#Installing relevant packages
install.packages("foreign")
install.packages("tidyverse")
install.packages("haven")
install.packages("plyr")
install.packages("uwIntroStats")
install.packages("FSA")
install.packages("knitr")
install.packages("scales")
install.packages("estimatr")
install.packages("ggplot2")
install.packages("car")
install.packages("dplyr")
install.packages("epiR")
install.packages("survey")

#Reading in packages necessary for analyses
library(plyr)
library(dplyr)
library(foreign)
library(tidyverse)
library(haven)
library(uwIntroStats)
library(FSA)
library(knitr)
library(scales)
library(estimatr)
library(ggplot2)
library(car)
library(epiR)
library(survey)

# [1.0] Rendering .XPT files into managble .CSV files ---------------------

#Setting file directory for reading .XPT files
dataDir = paste("U:/EPI 514/BRFSS Data/Original BRFSS Files/", sep = "")

#Reading 2015 .XPT Files
brfss2015v0 = read_xpt(paste0(dataDir, "LLCP2015.XPT"))
brfss2015v1 = read_xpt(paste0(dataDir, "LLCP15V1.XPT"))
brfss2015v2 = read_xpt(paste0(dataDir, "LLCP15V2.XPT"))

#Reading 2016 .XPT Files
brfss2016v0 = read_xpt(paste0(dataDir, "LLCP2016.XPT"))
brfss2016v1 = read_xpt(paste0(dataDir, "LLCP16V1.XPT"))
brfss2016v2 = read_xpt(paste0(dataDir, "LLCP16V2.XPT"))
brfss2016v3 = read_xpt(paste0(dataDir, "LLCP16V3.XPT"))

#Reading 2017 .XPT Files
brfss2017v0 = read_xpt(paste0(dataDir, "LLCP2017.XPT"))
brfss2017v1 = read_xpt(paste0(dataDir, "LLCP17V1.XPT"))
brfss2017v2 = read_xpt(paste0(dataDir, "LLCP17V2.XPT"))
brfss2017v3 = read_xpt(paste0(dataDir, "LLCP17V3.XPT"))

#Setting file directory for writing .csv files
dataDir = paste("U:/EPI 514/BRFSS Data/BRFSS Working Files",
                "/Disaggregated Files/", 
                sep = "")

#Writing 2015 .csv files
write.csv(brfss2015v0, paste0(dataDir, "brfss2015v0.csv"))
write.csv(brfss2015v1, paste0(dataDir, "brfss2015v1.csv"))
write.csv(brfss2015v2, paste0(dataDir, "brfss2015v2.csv"))

#Writing 2016 .csv files
write.csv(brfss2016v0, paste0(dataDir, "brfss2016v0.csv"))
write.csv(brfss2016v1, paste0(dataDir, "brfss2016v1.csv"))
write.csv(brfss2016v2, paste0(dataDir, "brfss2016v2.csv"))
write.csv(brfss2016v3, paste0(dataDir, "brfss2016v3.csv"))

#Writing 2017 .csv files
write.csv(brfss2017v0, paste0(dataDir, "brfss2017v0.csv"))
write.csv(brfss2017v1, paste0(dataDir, "brfss2017v1.csv"))
write.csv(brfss2017v2, paste0(dataDir, "brfss2017v2.csv"))
write.csv(brfss2017v3, paste0(dataDir, "brfss2017v3.csv"))

# [2.0] Reading BRFSS .csv files ------------------------------------------

#Setting directory
dataDir = paste("U:/EPI 514/BRFSS Data/BRFSS Working Files",
                "/Disaggregated Files/", 
                sep = "")

#Reading 2015 files
bd15v0 = read.csv(paste0(dataDir, "brfss2015v0.csv"))
bd15v1 = read.csv(paste0(dataDir, "brfss2015v1.csv"))
bd15v2 = read.csv(paste0(dataDir, "brfss2015v2.csv"))

#Reading 2016 files
bd16v0 = read.csv(paste0(dataDir, "brfss2016v0.csv"))
bd16v1 = read.csv(paste0(dataDir, "brfss2016v1.csv"))
bd16v2 = read.csv(paste0(dataDir, "brfss2016v2.csv"))
bd16v3 = read.csv(paste0(dataDir, "brfss2016v3.csv"))

#Reading 2017 files
bd17v0 = read.csv(paste0(dataDir, "brfss2017v0.csv"))
bd17v1 = read.csv(paste0(dataDir, "brfss2017v1.csv"))
bd17v2 = read.csv(paste0(dataDir, "brfss2017v2.csv"))
bd17v3 = read.csv(paste0(dataDir, "brfss2017v3.csv"))

# [3.1] Truncating "brfss.2015" for merging -------------------------------

#Codes for caregiver module participating states by survey version
v0_2015 = c(1, 15, 16, 17, 18, 19, 21, 28, 34, 41, 42, 45, 47, 51, 54, 55, 56)
v1_2015 = c(23, 24, 31)
v2_2015 = c(12, 36, 49)

#Extracting relevant cases into module-specific dataframes
bd2015v0 = bd15v0[bd15v0$X_STATE%in%v0_2015,]
bd2015v1 = bd15v1[bd15v1$X_STATE%in%v1_2015,]
bd2015v2 = bd15v2[bd15v2$X_STATE%in%v2_2015,]

#Identifying differences in column lables across versions
##bd2015v0 contains a column of "MSCODE" that v1 & v2 do not
##Names of _CLCWTV* & _LCPWTV* must be renamed
diff.a = setdiff(names(bd2015v0), names(bd2015v1))
diff.b = setdiff(names(bd2015v1), names(bd2015v0))
diff.c = setdiff(names(bd2015v2), names(bd2015v0))
diff.d = setdiff(names(bd2015v0), names(bd2015v2))

#Adding "MSCODE" to v1 and v2
bd2015v1$MSCODE = NA
bd2015v2$MSCODE = NA

#Correcting _CLCWTV* names for v1 and v2
colnames(bd2015v1)[colnames(bd2015v1) == "X_CLCWTV1"] = "X_CLLCPWT"
colnames(bd2015v2)[colnames(bd2015v2) == "X_CLCWTV2"] = "X_CLLCPWT"

#Correcting _LCPWTV* names for v1 and v2
colnames(bd2015v1)[colnames(bd2015v1) == "X_LCPWTV1"] = "X_LLCPWT"
colnames(bd2015v2)[colnames(bd2015v2) == "X_LCPWTV2"] = "X_LLCPWT"

#Generating "version" indicator variable
bd2015v0$VERSION = 0
bd2015v1$VERSION = 1
bd2015v2$VERSION = 2

#Generating "brfss.2015" prepped dataset
brfss.2015 = rbind(bd2015v0, bd2015v1, bd2015v2)

#Generating "cohort" indicating to which survey cohort cases belong
brfss.2015$cohort = 2015

#Calculating "ment14d" variable
brfss.2015$ment14d = NA
brfss.2015$ment14d[brfss.2015$MENTHLTH == 88] = 1
brfss.2015$ment14d[brfss.2015$MENTHLTH <= 13] = 2
brfss.2015$ment14d[brfss.2015$MENTHLTH > 13 & brfss.2015$MENTHLTH <31] = 3

# [3.2] Truncating "brfss.2016" for merging -------------------------------

#Codes for caregiver module participating states by survey version
v0_2016 = c(5, 11, 13, 27, 29, 30, 32, 34, 38, 41, 46, 47, 72)
v1_2016 = c(39, 49)
v2_2016 = c(4, 6, 9, 48)
v3_2016 = c(8, 36)

#Extracting relevant cases into module-specific dataframes
bd2016v0 = bd16v0[bd16v0$X_STATE%in%v0_2016,]
bd2016v1 = bd16v1[bd16v1$X_STATE%in%v1_2016,]
bd2016v2 = bd16v2[bd16v2$X_STATE%in%v2_2016,]
bd2016v3 = bd16v3[bd16v3$X_STATE%in%v3_2016,]

#Identifying differences in column lables across versions
##Names of _CLCWTV* & _LCPWTV* must be renamed
diff.a = setdiff(names(bd2016v0), names(bd2016v1))
diff.b = setdiff(names(bd2016v1), names(bd2016v0))
diff.c = setdiff(names(bd2016v2), names(bd2016v1))
diff.d = setdiff(names(bd2016v3), names(bd2016v2))

#Correcting _CLCWTV* names of v1, v2, & v3
colnames(bd2016v1)[colnames(bd2016v1) == "X_CLCWTV1"] = "X_CLLCPWT"
colnames(bd2016v2)[colnames(bd2016v2) == "X_CLCWTV2"] = "X_CLLCPWT"
colnames(bd2016v3)[colnames(bd2016v3) == "X_CLCWTV3"] = "X_CLLCPWT"

#Correcting _LCPWTV* names of v1, v2, & v3
colnames(bd2016v1)[colnames(bd2016v1) == "X_LCPWTV1"] = "X_LLCPWT"
colnames(bd2016v2)[colnames(bd2016v2) == "X_LCPWTV2"] = "X_LLCPWT"
colnames(bd2016v3)[colnames(bd2016v3) == "X_LCPWTV3"] = "X_LLCPWT"

#Generating "version" indicator variable
bd2016v0$VERSION = 0
bd2016v1$VERSION = 1
bd2016v2$VERSION = 2
bd2016v3$VERSION = 3

#Generating "brfss.2015" prepped dataset
brfss.2016 = rbind(bd2016v0, bd2016v1, bd2016v2, bd2016v3)

#Generating "cohort" indicating to which survey cohort cases belong
brfss.2016$cohort = 2016

# [3.3] Truncating "brfss.2017" for merging -------------------------------

#Codes for caregiver module participating states by survey version
v0_2017 = c(2, 15, 35, 41, 44)
v1_2017 = c(20, 26, 34)
v2_2017 = c(36, 40, 49)
v3_2017 = c(24)

#Extracting relevant cases into module-specific dataframes
bd2017v0 = bd17v0[bd17v0$X_STATE%in%v0_2017,]
bd2017v1 = bd17v1[bd17v1$X_STATE%in%v1_2017,]
bd2017v2 = bd17v2[bd17v2$X_STATE%in%v2_2017,]
bd2017v3 = bd17v3[bd17v3$X_STATE%in%v3_2017,]

#Identifying differences in column lables across versions
##Names of _CLCWTV* & _LCPWTV* must be renamed
diff.a = setdiff(names(bd2017v0), names(bd2017v1))
diff.b = setdiff(names(bd2017v1), names(bd2017v0))
diff.c = setdiff(names(bd2017v2), names(bd2017v1))
diff.d = setdiff(names(bd2017v3), names(bd2017v2))

#Correcting _CLCWTV* names of v1, v2, & v3
colnames(bd2017v1)[colnames(bd2017v1) == "X_CLCWTV1"] = "X_CLLCPWT"
colnames(bd2017v2)[colnames(bd2017v2) == "X_CLCWTV2"] = "X_CLLCPWT"
colnames(bd2017v3)[colnames(bd2017v3) == "X_CLCWTV3"] = "X_CLLCPWT"

#Correcting _LCPWTV* names of v1, v2, & v3
colnames(bd2017v1)[colnames(bd2017v1) == "X_LCPWTV1"] = "X_LLCPWT"
colnames(bd2017v2)[colnames(bd2017v2) == "X_LCPWTV2"] = "X_LLCPWT"
colnames(bd2017v3)[colnames(bd2017v3) == "X_LCPWTV3"] = "X_LLCPWT"

#Generating "version" indicator variable
bd2017v0$VERSION = 0
bd2017v1$VERSION = 1
bd2017v2$VERSION = 2
bd2017v3$VERSION = 3

#Generating "brfss.2015" prepped dataset
brfss.2017 = rbind(bd2017v0, bd2017v1, bd2017v2, bd2017v3)

#Generating "cohort" indicating to which survey cohort cases belong
brfss.2017$cohort = 2017

# [3.4] Reweighting survey ------------------------------------------------

#Determining total number of observations [Total = 357,544 cases]
total.obs = nrow(brfss.2015) + nrow(brfss.2016) + nrow(brfss.2017)

#Proportion of observations from 2015 [42.80121%]
prop.2015 = nrow(brfss.2015)/total.obs

#Proportion of observations from 2016 [38.27451%]
prop.2016 = nrow(brfss.2016)/total.obs

#Proportion of observations from 2017 [18.92429%]
prop.2017 = nrow(brfss.2017)/total.obs

#Reproportioning survey weighting
brfss.2015$svyweight = brfss.2015$X_LLCPWT*prop.2015
brfss.2016$svyweight = brfss.2016$X_LLCPWT*prop.2016
brfss.2017$svyweight = brfss.2017$X_LLCPWT*prop.2017
# [4.0] Writing & Reading cleaned .csv files ------------------------------

#Setting directory
dataDir = paste("U:/EPI 514/BRFSS Data/BRFSS Working Files/", sep = "")

#Writing cleaned .csv files
write.csv(brfss.2015, paste0(dataDir, "brfss_2015.csv"))
write.csv(brfss.2016, paste0(dataDir, "brfss_2016.csv"))
write.csv(brfss.2017, paste0(dataDir, "brfss_2017.csv"))

#Reading cleaned .csv files
brfss.2015 = read.csv(paste0(dataDir, "brfss_2015.csv"))
brfss.2016 = read.csv(paste0(dataDir, "brfss_2016.csv"))
brfss.2017 = read.csv(paste0(dataDir, "brfss_2017.csv"))

# [5.1] Prepping data sets for merge --------------------------------------

#Truncating brfss.2015 to retain only relevant variables
bd2015 = 
  as.data.frame(cbind(id = brfss.2015$X,
                      year = brfss.2015$IYEAR,
                      psu = brfss.2015$X_PSU,
                      ststr = brfss.2015$X_STSTR,
                      svyweight = brfss.2015$svyweight,
                      version = brfss.2015$VERSION,
                      cohort = brfss.2015$cohort,
                      state = brfss.2015$X_STATE,
                      age = brfss.2015$X_AGE80,
                      age2 = brfss.2015$X_AGEG5YR,
                      sex = brfss.2015$SEX,
                      race = brfss.2015$X_RACE,
                      race2 = brfss.2015$X_RACE_G1,
                      education = brfss.2015$EDUCA,
                      income = brfss.2015$X_INCOMG,
                      employ = brfss.2015$EMPLOY1,
                      hlthpln = brfss.2015$HLTHPLN1,
                      hlthpln2 = NA,
                      geog = brfss.2015$MSCODE,
                      caregiv = brfss.2015$CAREGIV1,
                      crgvrel = brfss.2015$CRGVREL1,
                      crgvlng = brfss.2015$CRGVLNG1,
                      crgvhrs = brfss.2015$CRGVHRS1,
                      crgvpers = brfss.2015$CRGVPERS,
                      crgvhous = brfss.2015$CRGVHOUS,
                      crgvprb = brfss.2015$CRGVPRB1,
                      crgvmst = brfss.2015$CRGVMST2,
                      crgvexp = brfss.2015$CRGVEXPT,
                      mnthlth = brfss.2015$MENTHLTH,
                      ment14d = brfss.2015$ment14d,
                      hvydrnk = brfss.2015$X_RFDRHV5,
                      bngdrnk = brfss.2015$X_RFBING5,
                      cig = brfss.2015$X_RFSMOK3,
                      ecig = NA))

#Truncating brfss.2016 to retain only relevant variables
bd2016 = 
  as.data.frame(cbind(id = brfss.2016$X,
                      year = brfss.2016$IYEAR,
                      psu = brfss.2016$X_PSU,
                      ststr = brfss.2016$X_STSTR,
                      svyweight = brfss.2016$svyweight,
                      version = brfss.2016$VERSION,
                      cohort = brfss.2016$cohort,
                      state = brfss.2016$X_STATE,
                      age = brfss.2016$X_AGE80,
                      age2 = brfss.2016$X_AGEG5YR,
                      sex = brfss.2016$SEX,
                      race = brfss.2016$X_RACE,
                      race2 = brfss.2016$X_RACE_G1,
                      education = brfss.2016$EDUCA,
                      income = brfss.2016$X_INCOMG,
                      employ = brfss.2016$EMPLOY1,
                      hlthpln = brfss.2016$HLTHPLN1,
                      hlthpln2 = brfss.2016$HLTHCVR1,
                      geog = brfss.2016$MSCODE,
                      caregiv = brfss.2016$CAREGIV1,
                      crgvrel = brfss.2016$CRGVREL1,
                      crgvlng = brfss.2016$CRGVLNG1,
                      crgvhrs = brfss.2016$CRGVHRS1,
                      crgvpers = brfss.2016$CRGVPERS,
                      crgvhous = brfss.2016$CRGVHOUS,
                      crgvprb = brfss.2016$CRGVPRB2,
                      crgvmst = brfss.2016$CRGVMST2,
                      crgvexp = brfss.2016$CRGVEXPT,
                      mnthlth = brfss.2016$MENTHLTH,
                      ment14d = brfss.2016$X_MENT14D,
                      hvydrnk = brfss.2016$X_RFDRHV5,
                      bngdrnk = brfss.2016$X_RFBING5,
                      cig = brfss.2016$X_RFSMOK3,
                      ecig = brfss.2016$X_CURECIG))

#Truncating brfss.2017 to retain only relevant variables
bd2017 = 
  as.data.frame(cbind(id = brfss.2017$X,
                      year = brfss.2017$IYEAR,
                      psu = brfss.2017$X_PSU,
                      ststr = brfss.2017$X_STSTR,
                      svyweight = brfss.2017$svyweight,
                      version = brfss.2017$VERSION,
                      cohort = brfss.2017$cohort,
                      state = brfss.2017$X_STATE,
                      age = brfss.2017$X_AGE80,
                      age2 = brfss.2017$X_AGEG5YR,
                      sex = brfss.2017$SEX,
                      race = brfss.2017$X_RACE,
                      race2 = brfss.2017$X_RACE_G1,
                      education = brfss.2017$EDUCA,
                      income = brfss.2017$X_INCOMG,
                      employ = brfss.2017$EMPLOY1,
                      hlthpln = brfss.2017$HLTHPLN1,
                      hlthpln2 = brfss.2017$HLTHCVR1,
                      geog = brfss.2017$MSCODE,
                      caregiv = brfss.2017$CAREGIV1,
                      crgvrel = brfss.2017$CRGVREL2,
                      crgvlng = brfss.2017$CRGVLNG1,
                      crgvhrs = brfss.2017$CRGVHRS1,
                      crgvpers = brfss.2017$CRGVPERS,
                      crgvhous = brfss.2017$CRGVHOUS,
                      crgvprb = brfss.2017$CRGVPRB2,
                      crgvmst = brfss.2017$CRGVMST2,
                      crgvexp = brfss.2017$CRGVEXPT,
                      mnthlth = brfss.2017$MENTHLTH,
                      ment14d = brfss.2017$X_MENT14D,
                      hvydrnk = brfss.2017$X_RFDRHV5,
                      bngdrnk = brfss.2017$X_RFBING5,
                      cig = brfss.2017$X_RFSMOK3,
                      ecig = brfss.2017$X_CURECIG))


# [5.2] Generating final BRFSS dataset ------------------------------------

#Merging data
brfss.data = as.data.frame(rbind(bd2015, bd2016, bd2017))

# [6.1] Defining emerging adults ------------------------------------------

#Age var: "emerging" adult caregivers (ages 18 - 24, inclusive)
brfss.data$age24 = NA
brfss.data$age24[brfss.data$age >= 18 & brfss.data$age <= 24] = 1
brfss.data$age24[brfss.data$age >= 25 & brfss.data$age <= 80] = 0
brfss.data$age24 = as.logical(brfss.data$age24)

#Age var: "emerging" adult caregivers (ages 18 - 25, inclusive)
brfss.data$age25 = NA
brfss.data$age25[brfss.data$age >= 18 & brfss.data$age <= 25] = 1
brfss.data$age25[brfss.data$age >= 26 & brfss.data$age <= 80] = 0
brfss.data$age25 = as.logical(brfss.data$age25)

# [6.2] Cleaning outcome variables ----------------------------------------

#Outcome var: poor mental health indicator (14 - 30 poor MH days in past month) 
brfss.data$poorment = NA
brfss.data$poorment[brfss.data$ment14d == 1 | brfss.data$ment14d == 2] = 0
brfss.data$poorment[brfss.data$ment14d == 3] = 1
brfss.data$poorment = as.logical(brfss.data$poorment)

#Outcome var: recoding "current binge drinker" variable
brfss.data$bngdrnk[brfss.data$bngdrnk == 9] = NA
brfss.data$bngdrnk[brfss.data$bngdrnk == 1] = 0
brfss.data$bngdrnk[brfss.data$bngdrnk == 2] = 1
brfss.data$bngdrnk = as.logical(brfss.data$bngdrnk)

#Outcome var: recoding "current heavy drinker" variable
brfss.data$hvydrnk[brfss.data$hvydrnk == 9] = NA
brfss.data$hvydrnk[brfss.data$hvydrnk == 1] = 0
brfss.data$hvydrnk[brfss.data$hvydrnk == 2] = 1
brfss.data$hvydrnk = as.logical(brfss.data$hvydrnk)

#Outcome var: recoding "current cigarette smoker" variable
brfss.data$cig[brfss.data$cig == 9] = NA
brfss.data$cig[brfss.data$cig == 1] = 0
brfss.data$cig[brfss.data$cig == 2] = 1
brfss.data$cig = as.logical(brfss.data$cig)

#Outcome var: recoding "current e-cig smoker" variable
brfss.data$ecig[brfss.data$ecig == 9] = NA
brfss.data$ecig[brfss.data$ecig == 1] = 0
brfss.data$ecig[brfss.data$ecig == 2] = 1
brfss.data$ecig = as.logical(brfss.data$ecig)

# [6.3] Cleaning exposure variables ---------------------------------------

#Cleaning "caregiv" variable [1 = "Yes", 0 = "No"]
brfss.data$caregiv[brfss.data$caregiv == 7] = NA
brfss.data$caregiv[brfss.data$caregiv == 8] = NA
brfss.data$caregiv[brfss.data$caregiv == 9] = NA
brfss.data$caregiv[brfss.data$caregiv == 2] = 0
brfss.data$caregiv = as.logical(brfss.data$caregiv)

#Cleaning "crgvexp" variable [1 = "Yes", 0 = "No"]
brfss.data$crgvexp[brfss.data$crgvexp == 7] = NA
brfss.data$crgvexp[brfss.data$crgvexp == 9] = NA
brfss.data$crgvexp[brfss.data$crgvexp == 2] = 0
brfss.data$crgvexp = as.logical(brfss.data$crgvexp)

#Correcting "crgvexp" for those who violated skip pattern
brfss.data$crgvexp[brfss.data$caregiv == 1] = NA

#Exposure var: 2-level caregiver v/s expectant caregiver variable
brfss.data$exposure2X = NA
brfss.data$exposure2X[brfss.data$crgvexp == 1] = 0
brfss.data$exposure2X[brfss.data$caregiv == 1] = 1
brfss.data$exposure2X = as.logical(brfss.data$exposure2X)

#Exposure var: 2-level caregiver v/s noncaregiver variable
brfss.data$exposure2N = NA
brfss.data$exposure2N[brfss.data$crgvexp == 0] = 0
brfss.data$exposure2N[brfss.data$caregiv == 1] = 1
brfss.data$exposure2N = as.logical(brfss.data$exposure2N)

#Exposure var: 2-level removing caregivers who have been CGs for < 30 days
brfss.data$exposure2Xs = brfss.data$exposure2X
brfss.data$exposure2Ns = brfss.data$exposure2N
brfss.data$exposure2Xs[brfss.data$crgvlng == 1] = NA
brfss.data$exposure2Ns[brfss.data$crgvlng == 1] = NA

  #Exposure var: cleaning caregiver type and hours/wk variables
  brfss.data$crgvhous[brfss.data$crgvhous == 7 | brfss.data$crgvhous == 9] = NA
  brfss.data$crgvpers[brfss.data$crgvpers == 7 | brfss.data$crgvpers == 9] = NA
  brfss.data$crgvhrs[brfss.data$crgvhrs == 7 | brfss.data$crgvhrs == 9] = NA
  brfss.data$crgvhous[brfss.data$crgvhous == 2] = 0
  brfss.data$crgvpers[brfss.data$crgvpers == 2] = 0
  
  #Exposure var: recoding caregiving type variable
  brfss.data$crgvtype = NA
  brfss.data$crgvtype[brfss.data$crgvhous == 0 & brfss.data$crgvpers == 0] = 1
  brfss.data$crgvtype[brfss.data$crgvhous == 1 & brfss.data$crgvpers == 0] = 2
  brfss.data$crgvtype[brfss.data$crgvhous == 0 & brfss.data$crgvpers == 1] = 3
  brfss.data$crgvtype[brfss.data$crgvhous == 1 & brfss.data$crgvpers == 1] = 4

#Exposure var: 3-level stratification var of exposure types
brfss.data$exposure3 = NA
brfss.data$exposure3[brfss.data$crgvexp == 0] = 1
brfss.data$exposure3[brfss.data$crgvexp == 1] = 2
brfss.data$exposure3[brfss.data$caregiv == 1] = 3
  
#Exposure var: 5-level dose response variable + comparisons (CG Type)
brfss.data$exposure5NType = NA
brfss.data$exposure5NType = brfss.data$crgvtype + 1
brfss.data$exposure5NType[brfss.data$crgvexp == FALSE] = 1
brfss.data$exposure5NType = 
  factor(x = brfss.data$exposure5NType,
         levels = 1:5,
         labels = c("(1) Non-CG", "(2) CG = Neither Type", 
                    "(3) CG = Household Only", "(4) CG = Personal Only", 
                    "(5) CG = Both"))

brfss.data$exposure5XType = NA
brfss.data$exposure5XType = brfss.data$crgvtype + 1
brfss.data$exposure5XType[brfss.data$crgvexp == TRUE] = 1
brfss.data$exposure5XType = 
  factor(x = brfss.data$exposure5XType,
         levels = 1:5,
         labels = c("(1) Exp CG", "(2) CG = Neither Type", 
                    "(3) CG = Household Only", "(4) CG = Personal Only", 
                    "(5) CG = Both"))

#Exposure var: 5-level dose response variable + comparisons (CG Hours per week)
brfss.data$exposure5NHours = NA
brfss.data$exposure5NHours = brfss.data$crgvhrs + 1
brfss.data$exposure5NHours[brfss.data$crgvexp == FALSE] = 1
brfss.data$exposure5NHours = 
  factor(x = brfss.data$exposure5NHours,
         levels = 1:5,
         labels = c("(1) Non-CG", "(2) CG hrs = (0, 8]", "(3) CG hrs = [9, 19]", 
                    "(4) CG hrs = [20, 39]", "(5) CG hrs = 40+"))

brfss.data$exposure5XHours = NA
brfss.data$exposure5XHours = brfss.data$crgvhrs + 1
brfss.data$exposure5XHours[brfss.data$crgvexp == TRUE] = 1
brfss.data$exposure5XHours = 
  factor(x = brfss.data$exposure5XHours,
         levels = 1:5,
         labels = c("(1) Exp CG", "(2) CG hrs = (0, 8]", "(3) CG hrs = [9, 19]", 
                    "(4) CG hrs = [20, 39]", "(5) CG hrs = 40+"))

#Exposure var: removing caregivers who have been CGs for less < 30 days
brfss.data$exposure5NType2 = brfss.data$exposure5NType
brfss.data$exposure5XType2 = brfss.data$exposure5XType
brfss.data$exposure5NHours2 = brfss.data$exposure5NHours
brfss.data$exposure5XHours2 = brfss.data$exposure5XHours
brfss.data$exposure5NType2[brfss.data$crgvlng == 1] = NA
brfss.data$exposure5XType2[brfss.data$crgvlng == 1] = NA
brfss.data$exposure5NHours2[brfss.data$crgvlng == 1] = NA
brfss.data$exposure5XHours2[brfss.data$crgvlng == 1] = NA

# [6.4] Cleaning covariates -----------------------------------------------

#Covariate: Create age category 21+ [0 = No, 1 = Yes]
brfss.data$age21=NA
brfss.data$age21[brfss.data$age < 21] = 0
brfss.data$age21[brfss.data$age >= 21] = 1
brfss.data$age21 = as.logical(brfss.data$age21)

#Covariate: Collapse educ categories
brfss.data$educ4= NA
brfss.data$educ4[brfss.data$education == 1] = 1     # <HS
brfss.data$educ4[brfss.data$education == 2] = 1     # <HS
brfss.data$educ4[brfss.data$education == 3] = 1     # <HS
brfss.data$educ4[brfss.data$education == 4] = 2     #  HS Grad
brfss.data$educ4[brfss.data$education == 5] = 3     #  Some College
brfss.data$educ4[brfss.data$education == 6] = 4     #  College Grad
brfss.data$educ4 = 
  factor(x = brfss.data$educ4,
         levels = c(1, 2, 3, 4),
         labels = c("<HS", "HS Grad", "Some College", "College Grad"))

#Covariate: Collapse employment
brfss.data$employment=NA
brfss.data$employment[brfss.data$employ == 1 | brfss.data$employ == 2] = 1 #employed
brfss.data$employment[brfss.data$employ == 3 | brfss.data$employ == 4] = 2 #unemployed
brfss.data$employment[brfss.data$employ == 5] = 3 #homemaker
brfss.data$employment[brfss.data$employ == 6] = 4 #student
brfss.data$employment[brfss.data$employ == 7] = 5 #retired
brfss.data$employment[brfss.data$employ == 8] = 6 #unable to work
brfss.data$employment = 
  factor(x = brfss.data$employment,
         levels = c(1, 2, 3, 4, 5, 6),
         labels = c("employed", "unemployed", "homemaker", "student",
                    "retired", "can't work"))

#Covariate: generating "male" gender variable
brfss.data$male = NA
brfss.data$male[brfss.data$sex == 2] = 0   #Female
brfss.data$male[brfss.data$sex == 1] = 1   #Male
brfss.data$male = as.logical(brfss.data$male)

#Covariate: cleaning "race" variable
brfss.data$race[brfss.data$race == 9] = NA
brfss.data$race =
  factor(x = brfss.data$race,
         levels = c(1, 2, 3, 4, 5, 6, 7, 8),
         labels = c("White", "Black", "AIAN", "Asian", "NHPI", "Other",
                    "Multircaial", "Hispanic"))

#Covariate: cleaning "race2" variable
brfss.data$race2 =
  factor(x = brfss.data$race2,
         levels = c(1, 2, 3, 4, 5),
         labels = c("White", "Black", "Hispanic", "Other", "Multiracial"))

#Covariate: cleaning "hlthpln" variable
brfss.data$hlthpln[brfss.data$hlthpln == 9] = NA
brfss.data$hlthpln[brfss.data$hlthpln == 7] = NA
brfss.data$hlthpln[brfss.data$hlthpln == 2] = 0
brfss.data$hlthpln = as.logical(brfss.data$hlthpln)

#Covariate: cleaning "income" variable
brfss.data$income[brfss.data$income == 9] = NA
brfss.data$income = 
  factor(x = brfss.data$income,
         levels = 1:5,
         labels = c("($0, $15k)", "[$15k $25k)", "[$25k, $35k)", "[35k, 50k)",
                    "50k+"))


# [6.5] Removing California from data set ---------------------------------

#Removing California [state == 6] from dataset
brfss.data = subset(brfss.data[brfss.data$state != 6,])

# [7.0] Writing final data set --------------------------------------------

#Setting directory
dataDir = paste("U:/EPI 514/BRFSS Data/BRFSS Working Files/", sep = "")

#Writing .csv file
write.csv(brfss.data, paste0(dataDir, "BRFSS_Data.csv"))

#Reading .csv file
brfss.data = read.csv(paste0(dataDir, "BRFSS_Data.csv"))
