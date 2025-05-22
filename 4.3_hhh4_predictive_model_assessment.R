##############################################
###### COVID-19 in EU/EEA 2020 - 2022 ########
##############################################


### MANUSCRIPT
### CAPTURING THE SPATIOTEMPORAL SPREAD OF COVID-19 IN 30 EUROPEAN COUNTRIES DURING 2020 - 2022

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium



# ====== GENERAL INFO ==========
### Document: MODEL COMPARISON - PREDICTIVE MODEL ASSESSMENT
### Author: trangngpmd
### Date: 2024-10-09


# ---------------------------------------------------------#
# PREDICTIVE MODEL ASSESSMENT -----
# Test the whole period
# ---------------------------------------------------------#

fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 
fit_ri_ARNEEND <- get(load(paste0(path_model_fit,"fit_original_ri_ARNEEND.rda"))) 

timepoint <- c(3, 142) ## the whole time series
models_compare <- c("fit_original","fit_ri_ARNEEND") # Models to compare

predictions <- lapply(mget(models_compare), oneStepAhead_hhh4lag,
                tp = timepoint, type = "final")

SCORES <- c("logs", "rps", "dss", "ses") #List score to compare between models
Scores <- lapply(predictions, scores, which = SCORES, individual = TRUE)

t(sapply(Scores, colMeans, dims = 2))
#                 logs      rps      dss        ses
# fit_original   9.255884 8554.054 18.91716 1908050867
# fit_ri_ARENEND 9.120791 7930.063 16.76422 1717778805

# ---------------------------------------------------------#
# PERMUTATION TEST -----
# ---------------------------------------------------------#

set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores$fit_original[, , score],
  score2 = Scores$fit_ri_ARENEND[, , score]))
# Results
#             logs         rps          dss      ses        
# diffObs     0.1350927    623.9917     2.152937 190272062  
# pVal.permut 1e-04        1e-04        1e-04    1e-04      
# pVal.t      6.682294e-06 2.579005e-27 0.053109 2.85268e-05

