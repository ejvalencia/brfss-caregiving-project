#   ********************************************************************
#                               FILE OVERVIEW
#   ********************************************************************
#
#   File Name:      BRFSS_Analyses_EJV
#   File Author:    Esteban Valencia
#   File Location:  U:\EPI 514\BRFSS Data
#   File History:   Created on 05/14/19
#   File Purpose:   This file concerns all analyses of BRFSS data for the
#                   young adult caregiver project. 
#   *********************************************************************

# [0.1] Loading required packages -----------------------------------------

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
install.packages("tableone")
install.packages("sandwich")
install.packages("lmtest")

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
library(tableone)
library(sandwich)
library(lmtest)

# [0.2] Loading & restricting data ----------------------------------------

#Setting directory
dataDir = paste("U:/EPI 514/BRFSS Data/BRFSS Working Files/", sep = "")

#Reading BRFSS data
brfss.data = read.csv(paste0(dataDir, "BRFSS_Data.csv"))

#Subsetting dataset to young adults only
brfss = brfss.data[brfss.data$age25 == 1,]

#Viewing dataframe
View(brfss)


# [0.3] Setting survey design ---------------------------------------------

#Setting lonely.psu
options(survey.lonely.psu = "adjust")

#Setting survey design
design = svydesign(data = brfss,
                   ids = ~1,
                   strata = ~ststr,
                   weight = ~svyweight,
                   nest = FALSE)

design.a = svydesign(data = subset(brfss[is.na(brfss$income) == FALSE,]),
                     ids = ~1,
                     strata = ~ststr,
                     weight = ~svyweight,
                     nest = FALSE)

#Setting survey design for ecig analyses
design2 = svydesign(data = subset(brfss[brfss$cohort != 2015,]),
                    ids = ~1,
                    strata = ~ststr,
                    weight = ~svyweight,
                    nest = FALSE)


# [0.4] Writing function for piecemeal confounder assessment --------------

#Single confounder
confounder.check = function(data, exposure, outcome, covar, method) {
  mod1 = svyglm(formula = as.formula(paste0(outcome, "~", exposure)),
                design = data,
                family = method)
  mod2 = svyglm(formula = as.formula(paste0(outcome, "~", exposure, "+", covar)),
                design = data,
                family = method)
  model.sum = cbind(cbind(exp(coef(mod1)[2]), 
                          exp(confint(mod1)[2, 1]), 
                          exp(confint(mod1)[2, 2])),
                    cbind(exp(coef(mod2)[2]), 
                          exp(confint(mod2)[2, 1]), 
                          exp(confint(mod2)[2, 2])))
  model.sum = cbind(model.sum, ifelse((abs(model.sum[1] - model.sum[4])/model.sum[1]) >= 0.1, 1.00, 0))
  colnames(model.sum) = c("UDJ: PR", "2.5%", "97.5%", "ADJ: PR", "2.5%", "97.5%", "Flag")
  rownames(model.sum) = paste0(outcome, " ", "(", exposure, ")")
  return(model.sum)
}

#Multiple confounders
confounder.check.multivariate = function(data, exposure, outcome, covar,
                                         covar.n, method){
  mod.sum = matrix(data = NA, nrow = 1, ncol = 7, byrow = TRUE)
  rownames(mod.sum) = paste0(outcome, " ", "(", exposure, ")")
  for(i in 1:covar.n){
    model = confounder.check(data = data, exposure = exposure,
                             outcome = outcome, covar = covar[i], method = method)
    rownames(model) = paste0("ADJ: ", covar[i])
    mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}

#Multiple confounders + multiple exposures
confounder.check.complex1 = function(data, exposure, exposure.n, outcome,
                                     covar, covar.n, method){
  mod.sum = matrix(data = NA, nrow = 1, ncol = 7, byrow = TRUE)
  for(i in 1:exposure.n){
    model = confounder.check.multivariate(data = data, exposure = exposure[i],
                                          outcome = outcome, covar = covar, 
                                          covar.n = covar.n, method = method)
    mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}

#Multiple confounders + multiple exposures + multiple outcomes
confounder.check.complex2 = function(data, exposure, exposure.n, outcome,
                                     outcome.n, covar, covar.n, method){
  mod.sum = matrix(data = NA, nrow = 1, ncol = 7, byrow = TRUE)
  for(i in 1:outcome.n){
  model = confounder.check.complex1(data = data, exposure = exposure,
                                    exposure.n = exposure.n,
                                    outcome = outcome[i], covar = covar,
                                    covar.n = covar.n, method = method)
  mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}

# [1.1] Creating Table1 ---------------------------------------------------

catvars = c("age21", "age", "male", "race2", "educ4", 
            "employment", "income", "hlthpln",
            "poorment", "bngdrnk", "hvydrnk",
            "cig", "ecig")

#Generating Table 1 w/ raw counts
table1a = CreateTableOne(vars = catvars,
                         strata = "exposure3",
                         data = brfss,
                         includeNA = TRUE)

#Generating Table 1 w/ raw counts for e-cig [cohort == 2016 & 2017]
table1b = CreateTableOne(vars = "ecig",
                         strata = "exposure3",
                         data = subset(brfss[brfss$cohort != 2015,]),
                         includeNA = TRUE)

#Generating Table 1 w/ weighted percentages
table1c = svyCreateTableOne(vars = catvars,
                            strata = "exposure3"
                            data = design,
                            includeNA = FALSE)

#Generating Table 1 w/ weighted percentages for e-cig [cohort == 2016 & 2017]
table1d = svyCreateTableOne(vars = "ecig",
                            strata = "exposure3",
                            data = design2,
                            includeNA = FALSE)

# [1.2] Table1 Output -----------------------------------------------------
# > table1a
#                  Stratified by exposure3
#                  Current CG   Expectant CG Non-CG        p      test
# n                3087         2303         12216                    
# age21 = TRUE (%) 1967 (63.7)  1393 (60.5)   8101 (66.3)  <0.001     
# male (%)                                                 <0.001     
#   FALSE         1582 (51.2)  1075 (46.7)   5705 (46.7)             
#   TRUE          1501 (48.6)  1227 (53.3)   6507 (53.3)             
#   NA               4 ( 0.1)     1 ( 0.0)      4 ( 0.0)             
# race2 (%)                                                <0.001     
#   Black          374 (12.1)   265 (11.5)    999 ( 8.2)             
#   Hispanic       437 (14.2)   425 (18.5)   1854 (15.2)             
#   Multiracial    152 ( 4.9)   134 ( 5.8)    506 ( 4.1)             
#   Other          247 ( 8.0)   227 ( 9.9)    931 ( 7.6)             
#   White         1828 (59.2)  1224 (53.1)   7791 (63.8)             
#   NA              49 ( 1.6)    28 ( 1.2)    135 ( 1.1)             
# educ4 (%)                                                <0.001     
#   <HS            249 ( 8.1)   234 (10.2)    726 ( 5.9)             
#   College Grad   492 (15.9)   331 (14.4)   2513 (20.6)             
#   HS Grad       1160 (37.6)   976 (42.4)   4338 (35.5)             
#   Some College  1180 (38.2)   759 (33.0)   4615 (37.8)             
#   NA               6 ( 0.2)     3 ( 0.1)     24 ( 0.2)             
# employment (%)                                           <0.001     
#   can't work      72 ( 2.3)    48 ( 2.1)    184 ( 1.5)             
#   employed      1745 (56.5)  1263 (54.8)   7062 (57.8)             
#   homemaker       86 ( 2.8)    55 ( 2.4)    322 ( 2.6)             
#   retired          4 ( 0.1)     1 ( 0.0)     14 ( 0.1)             
#   student        793 (25.7)   689 (29.9)   3613 (29.6)             
#   unemployed     358 (11.6)   215 ( 9.3)    917 ( 7.5)             
#   NA              29 ( 0.9)    32 ( 1.4)    104 ( 0.9)             
#   income (%)                                               <0.001     
#   ($0, $15k)     322 (10.4)   269 (11.7)   1245 (10.2)             
#   [$15k $25k)    610 (19.8)   460 (20.0)   1916 (15.7)             
#   [$25k, $35k)   323 (10.5)   230 (10.0)   1168 ( 9.6)             
#   [35k, 50k)     330 (10.7)   256 (11.1)   1452 (11.9)             
#   50k+           731 (23.7)   476 (20.7)   3465 (28.4)             
#   NA             771 (25.0)   612 (26.6)   2970 (24.3)             
# hlthpln (%)                                              <0.001     
#   FALSE          453 (14.7)   406 (17.6)   1632 (13.4)             
#   TRUE          2580 (83.6)  1851 (80.4)  10383 (85.0)             
#   NA              54 ( 1.7)    46 ( 2.0)    201 ( 1.6)             
# poorment (%)                                             <0.001     
#   FALSE         2423 (78.5)  1931 (83.8)  10528 (86.2)             
#   TRUE           611 (19.8)   331 (14.4)   1560 (12.8)             
#   NA              53 ( 1.7)    41 ( 1.8)    128 ( 1.0)             
# bngdrnk (%)                                               0.016     
#   FALSE         2260 (73.2)  1717 (74.6)   8789 (71.9)             
#   TRUE           756 (24.5)   543 (23.6)   3205 (26.2)             
#   NA              71 ( 2.3)    43 ( 1.9)    222 ( 1.8)             
# hvydrnk (%)                                               0.135     
#   FALSE         2780 (90.1)  2084 (90.5)  11120 (91.0)             
#   TRUE           211 ( 6.8)   157 ( 6.8)    813 ( 6.7)             
#   NA              96 ( 3.1)    62 ( 2.7)    283 ( 2.3)             
# cig (%)                                                  <0.001     
#   FALSE         2410 (78.1)  1911 (83.0)  10555 (86.4)             
#   TRUE           667 (21.6)   382 (16.6)   1634 (13.4)             
#   NA              10 ( 0.3)    10 ( 0.4)     27 ( 0.2)             
# ecig (%)                                                 <0.001     
#   FALSE         1399 (45.3)  1109 (48.2)   6603 (54.1)             
#   TRUE           199 ( 6.4)   145 ( 6.3)    630 ( 5.2)             
#   NA            1489 (48.2)  1049 (45.5)   4983 (40.8)             

# > table1b
#          Stratified by exposure3
#          Current CG   Expectant CG Non-CG       p      test
# n        1598         1256         7241                    
# ecig (%)                                        <0.001     
#   FALSE 1399 (87.5)  1109 (88.3)  6603 (91.2)             
#   TRUE   199 (12.5)   145 (11.5)   630 ( 8.7)             
#   NA       0 ( 0.0)     2 ( 0.2)     8 ( 0.1)             

# > table1c
#                     Stratified by exposure3
#                     Current CG        Expectant CG      Non-CG            p      test
# n                   1975477.8         1516835.2         7411062.4                    
# age21 = TRUE (%)    1155594.3 (58.5)   897283.5 (59.2)  4524349.7 (61.0)   0.405     
# male = TRUE (%)      980616.4 (49.7)   871596.4 (57.5)  3803074.3 (51.3)   0.014     
# race2 (%)                                                                  0.005     
#   Black             318639.5 (16.3)   234530.9 (15.6)   974314.3 (13.3)             
#   Hispanic          328771.5 (16.9)   373179.2 (24.9)  1450672.7 (19.8)             
#   Multiracial        48307.8 ( 2.5)    39849.3 ( 2.7)   164769.5 ( 2.2)             
#   Other             127972.7 ( 6.6)   137511.9 ( 9.2)   569488.3 ( 7.8)             
#   White            1125180.3 (57.7)   714746.4 (47.7)  4164523.0 (56.9)             
# educ4 (%)                                                                 <0.001     
#   <HS               241653.7 (12.3)   239195.0 (15.8)   826428.1 (11.2)             
#   College Grad      222790.0 (11.3)   148071.6 ( 9.8)  1073196.4 (14.5)             
#   HS Grad           732024.8 (37.1)   642879.4 (42.5)  2546608.2 (34.4)             
#   Some College      775900.8 (39.3)   482559.3 (31.9)  2949338.0 (39.9)             
# employment (%)                                                            <0.001     
#   can't work         45499.4 ( 2.3)    45027.9 ( 3.0)   109509.1 ( 1.5)             
#   employed         1060164.7 (54.2)   779306.8 (51.9)  3873334.4 (52.9)             
#   homemaker          36627.6 ( 1.9)    24757.5 ( 1.6)   192423.1 ( 2.6)             
#   retired             1985.8 ( 0.1)      127.7 ( 0.0)    13921.5 ( 0.2)             
#   student           568427.0 (29.1)   483411.2 (32.2)  2556418.0 (34.9)             
#   unemployed        242109.9 (12.4)   169905.1 (11.3)   576322.6 ( 7.9)             
# income (%)                                                                 0.004     
#   ($0, $15k)        182336.4 (12.5)   170398.4 (15.3)   699453.9 (12.6)             
#   [$15k $25k)       369401.7 (25.4)   280232.9 (25.2)  1133098.7 (20.4)             
#   [$25k, $35k)      193249.7 (13.3)   150904.2 (13.6)   620492.1 (11.2)             
#   [35k, 50k)        208980.9 (14.4)   160845.7 (14.5)   792284.8 (14.2)             
#   50k+              500826.2 (34.4)   349477.9 (31.4)  2316770.6 (41.7)             
# hlthpln = TRUE (%)  1560711.9 (80.0)  1179183.3 (79.2)  6097547.5 (84.2)   0.020     
# poorment = TRUE (%)  404620.6 (20.8)   200577.3 (13.5)   933633.9 (12.8)  <0.001     
# bngdrnk = TRUE (%)   505357.4 (26.2)   338261.7 (22.7)  1838110.1 (25.3)   0.347     
# hvydrnk = TRUE (%)   133145.0 ( 6.9)   100731.9 ( 6.8)   446080.4 ( 6.2)   0.666     
# cig = TRUE (%)       397992.7 (20.2)   265158.0 (17.6)   945818.7 (12.8)  <0.001     
# ecig = TRUE (%)       99317.5 (10.7)    67293.9 ( 8.8)   349986.8 ( 9.0)   0.517     

# > table1d
# Stratified by exposure3
# Current CG       Expectant CG    Non-CG           p      test
# n               929873.7         768066.9        3893755.1                   
# ecig = TRUE (%)  99317.5 (10.7)   67293.9 (8.8)   349986.8 (9.0)   0.517

# [2.0] Checking for confounders ------------------------------------------

# The following section utilizes formulas constructed in section [0.4]. Refer
# to [0.4] for full review of code.

#Identifying exposure vars
exp.vars = c("exposure2N", "exposure2X")

#Selecting outcome vars
out.vars = c("poorment", "bngdrnk", "hvydrnk", "cig")

#Selecting covariates for adjustment
covariates = c("age", "male", "race2", "educ4", "employment", "income",
               "hlthpln", "as.factor(state)")

#Univariate assessment of confounders for all exposures & outcomes
round(confounder.check.complex2(data = design, 
                                exposure = exp.vars,
                                exposure.n = 2,
                                outcome = out.vars,
                                outcome.n = 4,
                                covar = covariates,
                                covar.n = 8,
                                method = poisson), 
      digits = 2)

round(confounder.check.complex2(data = design2, 
                                exposure = exp.vars,
                                exposure.n = 2,
                                outcome = "ecig",
                                outcome.n = 1,
                                covar = covariates,
                                covar.n = 8,
                                method = poisson), 
      digits = 2)

#Results
#                         UDJ: PR 2.5% 97.5% ADJ: PR 2.5% 97.5% Flag
# ..................................................................
# poorment (exposure2N)        NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.63 1.37  1.93    1.63 1.37  1.93    0
#   ADJ: male                1.63 1.37  1.93    1.61 1.36  1.91    0
#   ADJ: race2               1.63 1.37  1.93    1.64 1.37  1.96    0
#   ADJ: educ4               1.63 1.37  1.93    1.61 1.35  1.91    0
#   ADJ: employment          1.63 1.37  1.93    1.56 1.31  1.86    0
#   ADJ: income              1.63 1.37  1.93    1.50 1.23  1.82    1  #Confounder
#   ADJ: hlthpln             1.63 1.37  1.93    1.59 1.33  1.90    0
#   ADJ: as.factor(state)    1.63 1.37  1.93    1.63 1.37  1.93    0
# poorment (exposure2X)        NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.54 1.17  2.03    1.54 1.17  2.03    0
#   ADJ: male                1.54 1.17  2.03    1.50 1.14  1.96    0
#   ADJ: race2               1.54 1.17  2.03    1.55 1.17  2.07    0
#   ADJ: educ4               1.54 1.17  2.03    1.58 1.20  2.07    0
#   ADJ: employment          1.54 1.17  2.03    1.56 1.18  2.06    0
#   ADJ: income              1.54 1.17  2.03    1.67 1.28  2.17    1  #Confounder
#   ADJ: hlthpln             1.54 1.17  2.03    1.53 1.16  2.01    0
#   ADJ: as.factor(state)    1.54 1.17  2.03    1.49 1.14  1.95    0
# ..................................................................
# bngdrnk (exposure2N)         NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.03 0.90  1.19    1.03 0.90  1.19    0
#   ADJ: male                1.03 0.90  1.19    1.04 0.91  1.20    0
#   ADJ: race2               1.03 0.90  1.19    1.03 0.90  1.19    0
#   ADJ: educ4               1.03 0.90  1.19    1.06 0.92  1.22    0
#   ADJ: employment          1.03 0.90  1.19    1.05 0.91  1.20    0
#   ADJ: income              1.03 0.90  1.19    1.11 0.96  1.28    0
#   ADJ: hlthpln             1.03 0.90  1.19    1.04 0.90  1.19    0
#   ADJ: as.factor(state)    1.03 0.90  1.19    1.05 0.91  1.21    0
# bngdrnk (exposure2X)         NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.15 0.94  1.41    1.15 0.94  1.41    0
#   ADJ: male                1.15 0.94  1.41    1.20 0.98  1.46    0
#   ADJ: race2               1.15 0.94  1.41    1.14 0.93  1.38    0
#   ADJ: educ4               1.15 0.94  1.41    1.12 0.91  1.36    0
#   ADJ: employment          1.15 0.94  1.41    1.14 0.94  1.39    0
#   ADJ: income              1.15 0.94  1.41    1.15 0.93  1.44    0
#   ADJ: hlthpln             1.15 0.94  1.41    1.15 0.94  1.41    0
#   ADJ: as.factor(state)    1.15 0.94  1.41    1.16 0.95  1.43    0
# ..................................................................
# hvydrnk (exposure2N)         NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.12 0.86  1.46    1.13 0.86  1.46    0
#   ADJ: male                1.12 0.86  1.46    1.12 0.86  1.46    0
#   ADJ: race2               1.12 0.86  1.46    1.10 0.84  1.45    0
#   ADJ: educ4               1.12 0.86  1.46    1.14 0.87  1.48    0
#   ADJ: employment          1.12 0.86  1.46    1.13 0.87  1.47    0
#   ADJ: income              1.12 0.86  1.46    1.26 0.95  1.67    1  #Confounder
#   ADJ: hlthpln             1.12 0.86  1.46    1.15 0.88  1.49    0
#   ADJ: as.factor(state)    1.12 0.86  1.46    1.13 0.87  1.48    0
# hvydrnk (exposure2X)         NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.02 0.68  1.53    1.01 0.68  1.53    0
#   ADJ: male                1.02 0.68  1.53    1.05 0.70  1.55    0
#   ADJ: race2               1.02 0.68  1.53    0.98 0.65  1.50    0
#   ADJ: educ4               1.02 0.68  1.53    0.99 0.65  1.51    0
#   ADJ: employment          1.02 0.68  1.53    1.01 0.68  1.52    0
#   ADJ: income              1.02 0.68  1.53    1.04 0.66  1.63    0
#   ADJ: hlthpln             1.02 0.68  1.53    1.01 0.67  1.52    0
#   ADJ: as.factor(state)    1.02 0.68  1.53    1.03 0.69  1.55    0
# ..................................................................
# cig (exposure2N)             NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.58 1.36  1.84    1.58 1.36  1.83    0
#   ADJ: male                1.58 1.36  1.84    1.58 1.36  1.84    0
#   ADJ: race2               1.58 1.36  1.84    1.55 1.34  1.80    0
#   ADJ: educ4               1.58 1.36  1.84    1.53 1.31  1.77    0
#   ADJ: employment          1.58 1.36  1.84    1.48 1.28  1.71    1  #Confounder
#   ADJ: income              1.58 1.36  1.84    1.44 1.21  1.71    1  #Confounder
#   ADJ: hlthpln             1.58 1.36  1.84    1.55 1.32  1.80    0
#   ADJ: as.factor(state)    1.58 1.36  1.84    1.52 1.32  1.76    0
# cig (exposure2X)             NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.15 0.91  1.45    1.14 0.91  1.43    0
#   ADJ: male                1.15 0.91  1.45    1.17 0.93  1.47    0
#   ADJ: race2               1.15 0.91  1.45    1.06 0.84  1.34    0
#   ADJ: educ4               1.15 0.91  1.45    1.20 0.95  1.51    0
#   ADJ: employment          1.15 0.91  1.45    1.14 0.91  1.43    0
#   ADJ: income              1.15 0.91  1.45    1.02 0.78  1.32    1  #Confounder
#   ADJ: hlthpln             1.15 0.91  1.45    1.14 0.91  1.44    0
#   ADJ: as.factor(state)    1.15 0.91  1.45    1.10 0.88  1.38    0
#                         UDJ: PR 2.5% 97.5% ADJ: PR 2.5% 97.5% Flag
# ..................................................................
# ecig (exposure2N)            NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.19 0.88  1.60    1.19 0.88  1.60    0
#   ADJ: male                1.19 0.88  1.60    1.24 0.91  1.68    0
#   ADJ: race2               1.19 0.88  1.60    1.12 0.82  1.52    0
#   ADJ: educ4               1.19 0.88  1.60    1.17 0.86  1.59    0
#   ADJ: employment          1.19 0.88  1.60    1.14 0.84  1.53    0
#   ADJ: income              1.19 0.88  1.60    0.97 0.69  1.35    1  #Confounder
#   ADJ: hlthpln             1.19 0.88  1.60    1.17 0.87  1.59    0
#   ADJ: as.factor(state)    1.19 0.88  1.60    1.18 0.88  1.59    0
# ecig (exposure2X)            NA   NA    NA      NA   NA    NA   NA
#   ADJ: age                 1.22 0.81  1.83    1.22 0.81  1.83    0
#   ADJ: male                1.22 0.81  1.83    1.33 0.88  2.03    1  #Confounder
#   ADJ: race2               1.22 0.81  1.83    1.16 0.76  1.78    0
#   ADJ: educ4               1.22 0.81  1.83    1.33 0.88  2.02    1  #Confounder
#   ADJ: employment          1.22 0.81  1.83    1.22 0.81  1.84    0
#   ADJ: income              1.22 0.81  1.83    0.97 0.60  1.55    1  #Confounder
#   ADJ: hlthpln             1.22 0.81  1.83    1.19 0.79  1.79    0
#   ADJ: as.factor(state)    1.22 0.81  1.83    1.18 0.81  1.73    0

# [2.1] Writing function for multiple poisson models -----------------

#Base function
base.func = function(data, exposure, outcome, covar, method){
  mod1 = svyglm(formula = paste0(outcome, " ~ ", exposure),
                design = data,
                family = method)  
  mod2 = svyglm(formula = paste0(outcome, " ~ ", exposure, covar),
                design = data,
                family = method)
  mod.sum = cbind(
    cbind(exp(coef(mod1)[2]),
          exp(confint(mod1)[2,1]),
          exp(confint(mod1)[2,2])),
    cbind(exp(coef(mod2)[2]),
          exp(confint(mod2)[2,1]),
          exp(confint(mod2)[2,2])))
  colnames(mod.sum) = c("Crude PR", "02.5%", "97.5%",
                        "Adjst PR", "02.5%", "97.5%")
  rownames(mod.sum) = outcome
  return(mod.sum)
}

#Function for multiple outcomes
base.func2 = function(data, exposure, outcome, n.outcome, covar, method) {
  mod.sum = matrix(data = NA, nrow = 1, ncol = 6, byrow = TRUE)
  rownames(mod.sum) = ifelse(grepl("N", exposure) == TRUE, "Non-caregiver (Ref)",
                             "Expectant Caregiver (Ref)")
  for(i in 1:n.outcome){
    model = base.func(data = data, exposure = exposure, outcome = outcome[i],
                      covar = covar, method = method)
    mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}

#Function for multiple outcomes w/ multiple exposures
glm.model = function(data, exposure, n.exposure, outcome, n.outcome, covar,
                     method){
  mod.sum = matrix(data = NA, nrow = 1, ncol = 6, byrow = TRUE)
  rownames(mod.sum) = "Summary of Poisson Models"
  for(i in 1:n.exposure){
    model = base.func2(data = data, exposure = exposure[i], outcome = outcome,
                       n.outcome = n.outcome, covar = covar, method = method)
    mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}

# [2.2] Models: Outcomes ~ Caregivng Expsoures ----------------------------

#Selecting exposure variables
exp.vars = c("exposure2N", "exposure2X")
exp.vars2 = c("exposure2Ns", "exposure2Xs")

#Selecting outcome variables
out.vars = c("poorment", "bngdrnk", "hvydrnk", "cig")

#Writing expression for covariates
covariates = c("+ income")

#Writing function for comparative tables
mod.sum1 = glm.model(data = design,
                       exposure = exp.vars,
                       n.exposure = 2,
                       outcome = out.vars,
                       n.outcome = 4,
                       covar = covariates,
                       method = poisson)
mod.sum2 = glm.model(data = design,
                     exposure = exp.vars2,
                     n.exposure = 2,
                     outcome = out.vars,
                     n.outcome = 4,
                     covar = covariates,
                     method = poisson)
mod.sumA = cbind(mod.sum1, mod.sum2)

#Writing function for comparative tables [ecig]
mod.sum3 = glm.model(data = design2,
                       exposure = exp.vars,
                       n.exposure = 2,
                       outcome = "ecig",
                       n.outcome = 1,
                       covar = covariates,
                       method = poisson)
mod.sum4 = glm.model(data = design2,
                     exposure = exp.vars2,
                     n.exposure = 2,
                     outcome = "ecig",
                     n.outcome = 1,
                     covar = covariates,
                     method = poisson)
mod.sumB = cbind(mod.sum3, mod.sum4)

#Merging model summaries
model.sum = rbind(mod.sumA, mod.sumB)
model.sum = cbind(model.sum, (abs(model.sum[,4] - model.sum[,10])/model.sum[,4])*100)


# [2.3] Summary of [2.2] Models -------------------------------------------

# > round(model.sum, digits = 2)
#                             Crude PR 02.5% 97.5% Adjst PR 02.5% 97.5% Crude PR 02.5% 97.5% Adjst PR 02.5% 97.5%
# Summary of Poisson Models         NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
# Non-caregiver (Ref)               NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
#   poorment                      1.63  1.37  1.93     1.50  1.23  1.82     1.65  1.38  1.98     1.51  1.24  1.84
#   bngdrnk                       1.03  0.90  1.19     1.11  0.96  1.28     1.02  0.86  1.20     1.07  0.91  1.27
#   hvydrnk                       1.12  0.86  1.46     1.26  0.95  1.67     1.00  0.73  1.37     1.05  0.74  1.48
#   cig                           1.58  1.36  1.84     1.44  1.21  1.71     1.63  1.38  1.91     1.49  1.24  1.80
# Expectant Caregiver (Ref)         NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
#   poorment                      1.54  1.17  2.03     1.67  1.28  2.17     1.57  1.18  2.07     1.71  1.31  2.23
#   bngdrnk                       1.15  0.94  1.41     1.15  0.93  1.44     1.14  0.91  1.42     1.12  0.89  1.42
#   hvydrnk                       1.02  0.68  1.53     1.04  0.66  1.63     0.90  0.58  1.41     0.88  0.53  1.46
#   cig                           1.15  0.91  1.45     1.02  0.78  1.32     1.18  0.93  1.50     1.05  0.80  1.38
# Summary of Poisson Models         NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
# Non-caregiver (Ref)               NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
#   ecig                          1.19  0.88  1.60     0.97  0.69  1.35     1.14  0.82  1.57     0.97  0.67  1.40
# Expectant Caregiver (Ref)         NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA
#   ecig                          1.22  0.81  1.83     0.97  0.60  1.55     1.17  0.77  1.77     1.00  0.61  1.64

# [3.0] Writing functions for complex svyglm models -----------------------

#Base function
base.func1 = function(data, exposure, outcome, covar, method){
  mod1 = svyglm(formula = paste0(outcome, " ~ ", exposure),
                design = data,
                family = method)  
  mod2 = svyglm(formula = paste0(outcome, " ~ ", exposure, covar),
                design = data,
                family = method)
  mod.sum = cbind(
    cbind(exp(coef(mod1)[2:5]),
          exp(confint(mod1)[2:5,1]),
          exp(confint(mod1)[2:5,2])),
    cbind(exp(coef(mod2)[2:5]),
          exp(confint(mod2)[2:5, 1]),
          exp(confint(mod2)[2:5, 2])))
  colnames(mod.sum) = c("Crude PR", "02.5%", "97.5%",
                        "Adjst PR", "02.5%", "97.5%")
  return(mod.sum)
}

#Multiple exposures
glm.model2 = function(data, exposure, n.exposure, outcome, covar, method){
  mod.sum = matrix(data = NA, nrow = 1, ncol = 6, byrow = TRUE)
  rownames(mod.sum) = "Summary of Poisson Models"
  for(i in 1:n.exposure){
    model = base.func1(data = data, exposure = exposure[i], outcome = outcome,
                       covar = covar, method = method)
    mod.sum = rbind(mod.sum, model)
  }
  return(mod.sum)
}



# [3.1] Models: Poorment ~ Exposures --------------------------------------

#Selecting exposure variables
exp.vars = c("exposure5NType", "exposure5XType",
             "exposure5NHours", "exposure5XHours")
exp.vars2 = c("exposure5NType2", "exposure5XType2",
              "exposure5NHours2", "exposure5XHours2")

#Selecting covariates
covariates = "+ income"

#Generating model
mod.sum5 = glm.model2(data = design,
                        exposure = exp.vars,
                        n.exposure = 4,
                        outcome = "poorment",
                        cov = covariates,
                        method = poisson)
mod.sum6 = glm.model2(data = design,
                        exposure = exp.vars2,
                        n.exposure = 4,
                        outcome = "poorment",
                        cov = covariates,
                        method = poisson)
model.sum2 = cbind(mod.sum5, mod.sum6)
model.sum2 = cbind(model.sum2, (abs(model.sum2[,4] - model.sum2[,10])/model.sum2[,4])*100)


# [3.2] Summary of [3.1] Models -------------------------------------------

# > round(model.sum2, digits = 2)
# Crude PR 02.5% 97.5% Adjst PR 02.5% 97.5% Crude PR 02.5% 97.5% Adjst PR 02.5% 97.5%
# Summary of Poisson Models                   NA    NA    NA       NA    NA    NA       NA    NA    NA       NA    NA    NA

# exposure5NType(2) CG = Neither Type       0.86  0.52  1.42     1.02  0.59  1.77     0.86  0.52  1.42     1.02  0.59  1.77
# exposure5NType(3) CG = Household Only     1.45  1.11  1.88     1.20  0.90  1.60     1.45  1.11  1.88     1.20  0.90  1.60
# exposure5NType(4) CG = Personal Only      1.61  1.02  2.55     1.81  1.13  2.91     1.61  1.02  2.55     1.81  1.13  2.91
# exposure5NType(5) CG = Both               1.93  1.54  2.41     1.76  1.36  2.28     1.93  1.54  2.41     1.76  1.36  2.28

# exposure5XType(2) CG = Neither Type       0.81  0.47  1.40     1.15  0.64  2.06     0.81  0.47  1.40     1.15  0.64  2.06
# exposure5XType(3) CG = Household Only     1.37  0.97  1.94     1.35  0.96  1.88     1.37  0.97  1.94     1.35  0.96  1.88
# exposure5XType(4) CG = Personal Only      1.53  0.93  2.53     1.97  1.20  3.24     1.53  0.93  2.53     1.97  1.20  3.24
# exposure5XType(5) CG = Both               1.83  1.35  2.48     1.95  1.43  2.65     1.83  1.35  2.48     1.95  1.43  2.65

# exposure5NHours(2) CG hrs = (0, 8]        1.46  1.17  1.82     1.37  1.06  1.77     1.46  1.17  1.82     1.37  1.06  1.77
# exposure5NHours(3) CG hrs = [9, 19]       1.94  1.44  2.61     1.97  1.42  2.75     1.94  1.44  2.61     1.97  1.42  2.75
# exposure5NHours(4) CG hrs = [20, 39]      1.26  0.74  2.16     1.18  0.64  2.20     1.26  0.74  2.16     1.18  0.64  2.20
# exposure5NHours(5) CG hrs = 40+           2.42  1.77  3.30     2.06  1.46  2.91     2.42  1.77  3.30     2.06  1.46  2.91

# exposure5XHours(2) CG hrs = (0, 8]        1.38  1.01  1.89     1.53  1.12  2.08     1.38  1.01  1.89     1.53  1.12  2.08
# exposure5XHours(3) CG hrs = [9, 19]       1.84  1.27  2.65     2.23  1.54  3.22     1.84  1.27  2.65     2.23  1.54  3.22
# exposure5XHours(4) CG hrs = [20, 39]      1.20  0.67  2.14     1.31  0.67  2.54     1.20  0.67  2.14     1.31  0.67  2.54
# exposure5XHours(5) CG hrs = 40+           2.29  1.58  3.34     2.25  1.53  3.32     2.29  1.58  3.34     2.25  1.53  3.32
