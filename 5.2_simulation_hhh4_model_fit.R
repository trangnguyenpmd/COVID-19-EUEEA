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
### Document: SIMULATION - hhh4 MODEL FIT
### Author: trangngpmd
### Date: 2024-10-09



# -------------------------------------------------------------------------------------------#
# MODEL SET UP -----
# -------------------------------------------------------------------------------------------#
# The original fit
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

y.start <- observed(fit_original$stsObj)[c(1:2), ]

# List covariates
log_pop <- log(population(fit_original$stsObj))
offset <- population(fit_original$stsObj)
log_susceptible <- log(1 - vaccine_mat)
stringency <- str_index_lag2
variant_alpha <- variant_alpha_mat 
variant_delta <- variant_delta_mat
variant_omicron <- variant_omicron_mat

# Model formula
f_end <- ~ 1 + log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ar <- ~ 1 + log_pop + stringency + log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ne <- ~ 1 + log_pop + stringency + variant_alpha + variant_delta + variant_omicron

# hhh4 data list
hhh4_datalist <- list(
  log_pop = log_pop,
  log_susceptible = log_susceptible,
  stringency = stringency, 
  variant_alpha = variant_alpha,
  variant_delta = variant_delta,
  variant_omicron = variant_omicron)


# Initial set up
lag_optimal <- 2
fit_start <- 3   
fit_end <- 143 

# Extract parameters
nterms <- terms(fit_original)$nGroups + 2 #add neweights decay & overdisp parameters
coefs <- coef(fit_original)[1:nterms]
CIs <- confint(fit_original)[1:nterms, ]
tab <- cbind(coefs, CIs)
round(tab,3)

# Control list
# starting values are modified following the table of true parameters for each scenario

# control_sim for Scenario 0, 1, 2
# control_sim <- list(
#   end = list(f= f_end, offset = offset),
#   ar = list(f = f_ar),
#   ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE, initial = c("logd" = log(2)))),
#   optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
#   start= list(fixed=c("ar.1"=-0.038, "ar.log_pop"=0.002,"ar.stringency"=0.001,
#                       "ar.log_susceptible"=-0.053,"ar.variant_alpha"=-0.129,
#                       "ar.variant_delta"=0.077, "ar.variant_omicron"=-0.180,
#                       "ne.1"=-13, "ne.log_pop"=0.707, "ne.stringency"=-0.087,
#                       "ne.variant_alpha"=-3.492, "ne.variant_delta"=-1.968,
#                       "ne.variant_omicron"=-8.539, "end.1"=-19.107, "end.log_susceptible"=-0.172,
#                       "end.variant_alpha"=-2.307, "end.variant_delta"=8.444,
#                       "end.variant_omicron"=10.882, "neweights.logd"=0.018,'overdisp'=0.2)),
#   family = "NegBin1",
#   data = hhh4_datalist,
#   subset = fit_start:fit_end,
#   funct_lag = poisson_lag, #continue finding the time-lag weights, which will be estimated from simulated datasets
#   max_lag = lag_optimal)


# control_sim for Scenario 3,4,5,6,7
control_sim <- list(
  end = list(f= f_end, offset = offset),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE, initial = c("logd" = log(2)))),
  optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
  start= list(fixed=c("ar.1"=-1.0, "ar.log_pop"=0.002,"ar.stringency"=0.001,
                      "ar.log_susceptible"=-0.053,"ar.variant_alpha"=0,
                      "ar.variant_delta"=0, "ar.variant_omicron"=0,
                      "ne.1"=-13.8, "ne.log_pop"=0.707, "ne.stringency"=-0.087,
                      "ne.variant_alpha"=0, "ne.variant_delta"=0,
                      "ne.variant_omicron"=0, "end.1"=-6.1, "end.log_susceptible"=-0.172,
                      "end.variant_alpha"=0, "end.variant_delta"=0,
                      "end.variant_omicron"=0, "neweights.logd"=0.018,'overdisp'=0.2)),
  family = "NegBin1",
  data = hhh4_datalist,
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, #continue finding the time-lag weights, which will be estimated from simulated datasets
  max_lag = lag_optimal)

# -------------------------------------------------------------------------------------------#
# STORING THE RESULTS -----
# -------------------------------------------------------------------------------------------#
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

# Create a list to store the fitting results from each simulated data
fit_list <- list("fit_original" = fit_original)


# Create a data frame to store the general information of each fitted model with each simulated data
fit_summary <- data.frame(model =  c("fit_original", paste0("sim_",c(1:100))),
                          loglikelihood = NA, AIC = NA, BIC = NA, overdisper = NA,
                          totalcases_simulated = NA, sim_vs_observed_ratio = NA,
                          totalcases_estimate_from_simdata = NA, fit_vs_sim = NA)

fit_summary$loglikelihood[1] <- summary(fit_original)$loglikelihood
fit_summary$AIC[1] <- summary(fit_original)$AIC
fit_summary$BIC[1] <- summary(fit_original)$BIC
fit_summary$overdisper[1] <- summary(fit_original)$fixef[20,1]
fit_summary$totalcases_simulated[1] <- 181962841 
fit_summary$sim_vs_observed_ratio[1] <- fit_summary$totalcases_simulated[1]/181962841 
fit_summary$totalcases_estimate_from_simdata[1] <- sum(fit_original$fitted.values)
fit_summary$fit_vs_sim[1] <- fit_summary$totalcases_estimate_from_simdata[1]/fit_summary$totalcases_simulated[1]  


# Create array to store the coefficients estimated from fitting each simulated data
# Extract parameter from the original fit
nterms <- terms(fit_original)$nGroups +  2 #adding: neweight_d & overdispersion params
coefs <- coef(fit_original)[1:nterms]
CIs <- confint(fit_original)[1:nterms, ]
tab <- cbind(coefs, CIs)
dim(tab)

param_array <- array(data = NA, dim = c(20,3,101), 
                     dimnames = list(rownames(tab), colnames(tab), c("fit_original", paste0("sim_",c(1:100)))))
param_array[,,1] <- tab #parameters information

# -------------------------------------------------------------------------------------------#
# FITTING THE MODEL WITH EACH SIMULATED DATASETS (800 DATASETS IN TOTAL) -----
# -------------------------------------------------------------------------------------------#

scenario <- 7  #can be changed to other scenarios
sim_data_array <- readRDS(paste0(path_simulation,"sim_data_array_scenario_",scenario,".rds"))


for (i in c(1:100)) {
  
  simname <- paste0("sim_",i)
  
  # Create dataframe for sts object
  sim_df <- as.data.frame(sim_data_array[,,simname])
  sim_df <- rbind(y.start, sim_df)
  
  # Create sts object
  sts <- sts(observed = sim_df, start = c(2020, 13), frequency = 52,
             neighbourhood = neighbor_adjusted,  #use the adjusuted neighbourhood matrix
             map = map_sts, population = pop_country_mat)
  
  # fitting the model with the new simulated dataset
  fit <- profile_par_lag(sts, control = control_sim) #control list can be switched between two above-mentioned lists
  
  if (fit$convergence == TRUE){
    
    # Extract parameter
    nterms <- terms(fit)$nGroups + 2 #adding: neweight_d & overdispersion params
    coefs <- coef(fit)[1:nterms]
    CIs <- confint(fit)[1:nterms, ]
    tab <- cbind(coefs, CIs)
    
    # Output
    # Save fitted model in the fit_list
    fit_list <- c(fit_list, list(simname = fit)) #model fit
    names(fit_list)[length(fit_list)] <- simname
    
    # Add params in param_array
    param_array[,,i+1] <- tab
    
    # Complete the fit_summary
    fit_summary$loglikelihood[1+i] <- summary(fit)$loglikelihood
    fit_summary$AIC[1+i] <- summary(fit)$AIC
    fit_summary$BIC[1+i] <- summary(fit)$BIC
    fit_summary$overdisper[1+i] <- summary(fit)$fixef[20,1]
    fit_summary$totalcases_simulated[1+i] <- sum(sim_df) - sum(y.start)
    fit_summary$sim_vs_observed_ratio[1+i] <- fit_summary$totalcases_simulated[1+i]/181962841 
    fit_summary$totalcases_estimate_from_simdata[1+i] <- sum(fit$fitted.values)
    fit_summary$fit_vs_sim[1+i] <- fit_summary$totalcases_estimate_from_simdata[1+i]/fit_summary$totalcases_simulated[1+i]  
    
  } else {
    
    # Output
    fit_list <- c(fit_list, list(simname = NA)) #model fit
    names(fit_list)[length(fit_list)] <- simname
    
    param_array[,,i+1] <- NA #parameter
    
    fit_summary$loglikelihood[1+i] <- NA
    fit_summary$AIC[1+i] <- NA
    fit_summary$BIC[1+i] <- NA
    fit_summary$overdisper[1+i] <- NA
    fit_summary$totalcases_simulated[1+i] <- sum(sim_df) - sum(y.start)
    fit_summary$sim_vs_observed_ratio[1+i] <- fit_summary$totalcases_simulated[1+i]/181962841 
    fit_summary$totalcases_estimate_from_simdata[1+i] <- NA
    fit_summary$fit_vs_sim[1+i] <- NA 
  }
  
  # Check progress
  print(simname)
  print(fit$convergence)
  # print(tab[19,])
}

table(is.na(fit_summary$AIC))

saveRDS(fit_summary, paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
saveRDS(fit_list, paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
saveRDS(param_array, paste0(path_simulation,"param_array_scenario_",scenario,".rds"))




# -------------------------------------------------------------------------------------------#
# FIT AGAIN THE SIMULATED DATA WHEN NON-CONVERGENCE -----
# -------------------------------------------------------------------------------------------#

scenario <- 3
aa <- readRDS(paste0(path_simulation,"fit_summary_scenario_",scenario,".rds")) %>% filter(is.na(AIC))
list <- aa$model
list

# After the first fit with start_values = true_param, we have only 2 non-converged models, including
# Scenario_3 (1 models)     list <- c(10)
# Scenario_4 (1 models)     list <- c(10)


# Change the start values in control_list
scenario <- 3
sim_data_array <- readRDS(paste0(path_simulation,"sim_data_array_scenario_",scenario,".rds"))

fit_summary <- readRDS(paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
param_array <- readRDS(paste0(path_simulation,"param_array_scenario_",scenario,".rds"))

# control list (can be switched between two lists as below)
control_sim <- list(
  end = list(f= f_end, offset = offset),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE,
                                           initial = c("logd" = log(2)))),
  optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
  family = "NegBin1",
  data = hhh4_datalist,
  subset = fit_start:fit_end,
  funct_lag = poisson_lag,
  max_lag = lag_optimal)

control_sim <- list(
  end = list(f= f_end, offset = offset),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE,
                                           initial = c("logd" = log(2)))),
  optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
  start= list(fixed=c("ar.1"=-0.038, "ar.log_pop"=0.002,"ar.stringency"=0.001,
                      "ar.log_susceptible"=-0.053,"ar.variant_alpha"=-0.129,
                      "ar.variant_delta"=0.077, "ar.variant_omicron"=-0.180,
                      "ne.1"=-13, "ne.log_pop"=0.707, "ne.stringency"=-0.087,
                      "ne.variant_alpha"=-3.492, "ne.variant_delta"=-1.968,
                      "ne.variant_omicron"=-8.539, "end.1"=-19.107, "end.log_susceptible"=-0.172,
                      "end.variant_alpha"=-2.307, "end.variant_delta"=8.444,
                      "end.variant_omicron"=10.882, "neweights.logd"=0.018,'overdisp'=0.2)),  family = "NegBin1",
  data = hhh4_datalist,
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, #continue finding the time-lag weights, which will be estimated from simulated datasets
  max_lag = lag_optimal)


for (i in list) {
  
  simname <- paste0("sim_",i)
  
  #create dataframe for sts object
  sim_df <- as.data.frame(sim_data_array[,,simname])
  sim_df <- rbind(y.start, sim_df)
  
  #create sts object
  sts <- sts(observed = sim_df, start = c(2020, 13), frequency = 52,
             neighbourhood = neighbor_adjusted, map = map_sts, 
             population = pop_country_mat)
  
  #model fit
  fit <- profile_par_lag(sts, control = control_sim) #control list can be switched between two above-mentioned lists
  
  if (fit$convergence == TRUE){
    
    #extract parameter
    nterms <- terms(fit)$nGroups + 2 #adding: neweight_d & overdispersion params
    coefs <- coef(fit)[1:nterms]
    CIs <- confint(fit)[1:nterms, ]
    tab <- cbind(coefs, CIs)
    
    #output
    #save fitted model in the fit_list
    simsim <- list(simsim = fit)
    names(simsim)[length(simsim)] <- simname
    aa <- names(fit_list) %in% names(simsim) & is.na(fit_list)
    fit_list <- modifyList(fit_list, simsim)
    
    #add params in param_array
    param_array[,,i+1] <- tab
    
    #complete the fit_summary
    fit_summary$loglikelihood[1+i] <- summary(fit)$loglikelihood
    fit_summary$AIC[1+i] <- summary(fit)$AIC
    fit_summary$BIC[1+i] <- summary(fit)$BIC
    fit_summary$overdisper[1+i] <- summary(fit)$fixef[20,1]
    fit_summary$totalcases_simulated[1+i] <- sum(sim_df) - sum(y.start)
    fit_summary$sim_vs_observed_ratio[1+i] <- fit_summary$totalcases_simulated[1+i]/181962841
    fit_summary$totalcases_estimate_from_simdata[1+i] <- sum(fit$fitted.values)
    fit_summary$fit_vs_sim[1+i] <- fit_summary$totalcases_estimate_from_simdata[1+i]/fit_summary$totalcases_simulated[1+i]
    
  } else {
    #output
    fit_list <- fit_list
    
    param_array[,,i+1] <- NA #parameter
    
    fit_summary$loglikelihood[1+i] <- NA
    fit_summary$AIC[1+i] <- NA
    fit_summary$BIC[1+i] <- NA
    fit_summary$overdisper[1+i] <- NA
    fit_summary$totalcases_simulated[1+i] <- sum(sim_df) - sum(y.start)
    fit_summary$sim_vs_observed_ratio[1+i] <- fit_summary$totalcases_simulated[1+i]/181962841
    fit_summary$totalcases_estimate_from_simdata[1+i] <- NA
    fit_summary$fit_vs_sim[1+i] <- NA
  }
  
  # Check progress
  print(simname)
  print(fit$convergence)
  print(tab[19,])
}

table(is.na(fit_summary$AIC))
aa <- fit_summary %>% filter(is.na(AIC))
aa$model

scenario

saveRDS(fit_summary, paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
saveRDS(fit_list, paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
saveRDS(param_array, paste0(path_simulation,"param_array_scenario_",scenario,".rds"))



