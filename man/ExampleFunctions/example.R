
# Having a data.table -----------------------------------------------------
# this could be changed into another data.table

library(feather)

data_test <- read_feather(here::here("tests", "testdata", "tpData.feather"))

# select first 10000 cases to test with 
setDT(data_test)

data_test <- data_test[1:1e4]
setnames(data_test, 'Clm_Count', 'claimNumb')
setnames(data_test, 'TLength', 'exposure')
setnames(data_test, 'AgeInsured', 'age')
setnames(data_test, 'SexInsured', 'sex')
setnames(data_test, 'Experience', 'experience')
setnames(data_test, 'VAge', 'carAge')
setnames(data_test, 'PrivateCar', 'privateUse')
setnames(data_test, 'Cover_C', 'coverType')
setnames(data_test, 'AvPayment', 'claimSize')
setnames(data_test, 'NCD_0', 'noClaimsHistory')

isFactorDT(data_test, ,T)
isNumericDT(data_test, ,T)

asFactorDT(data_test, c('privateUse', 'noClaimsHistory', 'coverType'))
asNumericDT(data_test, c('exposure', 'claimSize', 'carAge', 'age'))

isFactorDT(data_test, ,T)
isNumericDT(data_test, ,T)

data_test[, exposure := exposure/365]

extractLevelDT(data_test)


# GAM modeling ------------------------------------------------------------

library(mgcv)
library(plyr)

gam_model <- gam(formula = 
                   claimNumb ~ s(age) + sex + s(experience) + s(carAge) + 
                   privateUse + coverType + noClaimsHistory + offset(exposure), 
                 family = poisson(link = log), 
                 data = data_test)


parts <- c('age', "experience", "carAge")

# extract the spline estimates
splineEst <- extractSplineEstimate(gam_model, 
                                   parts)
# -------------------------------------------------------------------------

# split based on the regression trees
split <- optNumbGroups(data_test, 
                       gam_model, 
                       splineEst, 
                       c("age", "carAge", "experience"), 
                       nGroups = c(5,7, 9, 10), 
                       typeSplit = "tree")

# split based on the classInt package of the spline results
split_class_int <- optNumbGroups(data_test, 
                                 gam_model, 
                                 splineEst, 
                                 c("age", "carAge", "experience"), 
                                 nGroups =  c(5,7, 9, 10), 
                                 typeSplit = c("classInt"))

# the ClassInt method splits based on the y-direction (= spline result), 
# while we would need the splits in the x-direction (= variable value)
splits_age <- y2xSplits(gamFit = gam_model, 
                        selectedVar = "age", 
                        min_max_values = c(19, 99), 
                        ySplits = split_class_int$splits$ageSpline)

splits_carage <- y2xSplits(gamFit = gam_model, 
                           selectedVar = "carAge", 
                           min_max_values = c(0, 30), 
                           ySplits = split_class_int$splits$carAgeSpline)

# -------------------------------------------------------------------------

library(ggplot2)
ggplotObject <- plotUnivSpline(gam_model, 
                               "carAge", 
                               min_max_values = c(0, 30), 
                               simultaneousCI = FALSE, 
                               transparence = 0.5, 
                               colorLine = 'black', 
                               colorRibbon = 'steelblue', 
                               lineWidth = 1.2, 
                               showPlot = TRUE, 
                               nSamples = 1000, 
                               xLimits = c(0,30))


# Vertical splits (x-direction) -------------------------------------------


ggplotObject + 
  geom_vline(xintercept = split$splits$carAge, 
             color = 'forestgreen', 
             linewidth = 1.1)


# Horizontal splits (y-direction) -------------------------------------------

ggplotObject + 
  geom_hline(yintercept = split_class_int$splits$carAgeSpline, 
             color = 'forestgreen', 
             linewidth = 1.1)


# Check if horizontal = vertical in conversion ----------------------------
# a lot of lines are created
ggplotObject + 
  geom_vline(xintercept = splits_carage, 
             color = 'forestgreen', 
             linewidth = 1.1) + 
  geom_hline(yintercept = split_class_int$splits$carAgeSpline, 
             color = 'forestgreen', 
             linewidth = 1.1) 


# Plots for Age -----------------------------------------------------------
ggplotObject_age <- plotUnivSpline(gam_model, 
                                   "age", 
                                   min_max_values = c(19, 99), 
                                   simultaneousCI = FALSE, 
                                   transparence = 0.5, 
                                   colorLine = 'black', 
                                   colorRibbon = 'steelblue', 
                                   lineWidth = 1.2, 
                                   showPlot = TRUE, 
                                   nSamples = 1000, 
                                   xLimits = c(18,99))

ggplotObject_age + 
  geom_vline(xintercept = split$splits$age, 
             color = 'forestgreen', 
             linewidth = 1.1)

# Plots for Experience -----------------------------------------------------------
ggplotObject_exp <- plotUnivSpline(gam_model, 
                                   "experience", 
                                   min_max_values = c(1, 55), 
                                   simultaneousCI = FALSE, 
                                   transparence = 0.5, 
                                   colorLine = 'black', 
                                   colorRibbon = 'steelblue', 
                                   lineWidth = 1.2, 
                                   showPlot = TRUE, 
                                   nSamples = 1000, 
                                   xLimits = c(1,55))

ggplotObject_exp + 
  geom_vline(xintercept = split$splits$experience, 
             color = 'forestgreen', 
             linewidth = 1.1)
