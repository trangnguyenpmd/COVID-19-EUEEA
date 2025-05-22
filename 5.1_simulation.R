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
### Document: SIMULATION
### Author: trangngpmd
### Date: 2024-10-09


############ FIND DIFFERENT SCENARIOS OF THE EPIDEMICS #################################
# ---------------------------------------------------------#
# BY SIMULATION AFTER CHANGING THE PARAMETERS -----
# ---------------------------------------------------------#
# Call the original fitted model (model with component-specific fixed intercepts)
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

# Look at the parameter estimates

fit <- fit_original
nterms <- terms(fit)$nGroups + 2 #add neweights decay & overdisp parameters
coefs <- coef(fit)[1:nterms]
CIs <- confint(fit)[1:nterms, ]
tab_original <- cbind(coefs, CIs)
tab_original <- as.data.frame(tab_original) %>% mutate(param = rownames(tab_original))
round(tab_original[,1:3],4)

#                        coefs    2.5 %   97.5 %
# ar.1                 -0.0383  -0.2229   0.1464
# ar.log_pop            0.0019  -0.0094   0.0131
# ar.stringency         0.0005  -0.0009   0.0018
# ar.log_susceptible   -0.0526  -0.0795  -0.0258
# ar.variant_alpha     -0.1292  -0.1974  -0.0610
# ar.variant_delta      0.0768   0.0057   0.1479
# ar.variant_omicron   -0.1796  -0.2607  -0.0985
# ne.1                -13.0944 -15.2335 -10.9553
# ne.log_pop            0.7009   0.5866   0.8152
# ne.stringency        -0.0868  -0.1073  -0.0664
# ne.variant_alpha     -3.4919  -5.6889  -1.2950
# ne.variant_delta     -1.9682  -2.7111  -1.2253
# ne.variant_omicron   -8.5386 -12.4842  -4.5929
# end.1               -19.1071 -23.7788 -14.4353
# end.log_susceptible  -0.1720  -0.3518   0.0077
# end.variant_alpha    -2.3074 -15.1288  10.5140
# end.variant_delta     8.4436   3.6423  13.2450
# end.variant_omicron  10.8824   6.1795  15.5853
# neweights.logd        0.0177  -0.5439   0.5793
# overdisp              0.2383   0.2282   0.2485


#---------------------------------------------------------------------------#
# TABLE SUMMARY THE CHANGING OF THE PARAMETERS (point estimates only) -----
#---------------------------------------------------------------------------#
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

point_estimates_change <- data.frame(Scenario_0 = coef(fit_original),
                                     Scenario_1 = replace(coef(fit_original), c("ar.1", "ne.1","end.1"),c(-1.5, -11.9, -16.1)),
                                     Scenario_2 = replace(coef(fit_original), c("ar.1", "ne.1","end.1"),c(-0.8, -10.0, -16.6)),
                                     Scenario_3 = replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18 ),c(-3.0, -13.3, -6.4, rep(0,9))),
                                     Scenario_4 = replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18 ),c(-2.9, -13.2, -6.5, rep(0,9))),
                                     Scenario_5 = replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18 ),c(-1.1, -13.4, -7.1, rep(0,9))),
                                     Scenario_6 = replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18 ),c(-2.6, -13.9, -6.9, rep(0,9))),
                                     Scenario_7 = replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18 ),c(-1.0, -13.8, -6.1, rep(0,9)))
)
point_estimates_change <- as.data.frame(t(point_estimates_change))

write_xlsx(as.data.frame(t(point_estimates_change)), 
           paste0(path_simulation_results,"point_estimates_change.xlsx"))

#---------------------------------------------------------------------------#
# FINAL SELECTION -----
#---------------------------------------------------------------------------#

# ---------------------------------------------------------#
## SIMULATION FROM THE ORIGINAL MODEL FIT (SCENARIO_0) -----
# ---------------------------------------------------------#
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

nsims <- 100 #number of simulations
y.start = observed(fit_original$stsObj)[c(1:2), ] #using the first 2 weeks to predict the following week
subset = 3:143 

sim_original <- simulate(fit_original, y.start = y.start,subset = subset,
                         nsim = nsims, seed = 9)

# Plot check
# plot(sim_original, "time", ylim=c(0,9000000))
plot(sim_original, "time")

saveRDS(sim_original, paste0(path_simulation, "sim100_scenario_0.rds"))

# Change to array (each dataframe in the array represents for a new simulated dataset)
sim_data_array <- as.array(as.hhh4simslist(sim_original)$sim_original)
dimnames(sim_data_array)[[3]] <- c(paste0("sim_",c(1:100)))
dim(sim_data_array)

saveRDS(sim_data_array, paste0(path_simulation, "sim_data_array_scenario_0.rds"))

# Calculate the Mean total cases over 100 simulated datasets and compare with the original observed data
mean(apply(sim_data_array, c(3), sum))/181962841*100      # Result: 145.5312%

# sum(sim_original@observed)/181962841   # results: 1.256541 for one simulation only (nsim = 1)
# plot(sim_original, type = observed ~ time)

# ---------------------------------------------------------#
## SCENARIOS 1 TO 7-----
# ---------------------------------------------------------#
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 

nsims <- 100 #number of simulations
y.start = observed(fit_original$stsObj)[c(1:2), ] #using the first 2 weeks to predict the following week
subset = 3:143 

scenario <- 7
coefs <- as.numeric(point_estimates_change[scenario+1,])
# coefs <- replace(coef(fit_original), c("ar.1", "ne.1","end.1"),c(-3.7, -13.2, -6.4))
# coefs <- replace(coef(fit_original), c(1,8,14,5:7,11:13,16:18), c(-1, -13.8, -6.1, rep(0,9)))
sim_adjusted <- simulate(fit_original, y.start = y.start,subset = subset,
                         nsim = nsims, seed = 9, coefs = coefs)

# Plot check
# plot(sim_adjusted, "time", ylim=c(0,9000000))
plot(sim_adjusted, "time")

saveRDS(sim_adjusted, paste0(path_simulation, "sim100_scenario_",scenario,".rds"))

# Change to array (each dataframe in the array represents for a new simulated dataset)
sim_data_array <- as.array(as.hhh4simslist(sim_adjusted)$sim_adjusted)
dimnames(sim_data_array)[[3]] <- c(paste0("sim_",c(1:100)))
# dim(sim_data_array)

saveRDS(sim_data_array, paste0(path_simulation,"sim_data_array_scenario_",scenario,".rds"))

# Calculate the Mean total cases over 100 simulated datasets and compare with the original observed data
mean(apply(sim_data_array, c(3), sum))/181962841*100

# Scenario 1 mean: 99.77464%
# Scenario 2 mean: 96.99389%
# Scenario 3 mean: 100.5042%
# Scenario 4 mean: 101.3882%
# Scenario 5 mean: 103.3595%
# Scenario 6 mean: 49.90956%
# Scenario 7 mean: 202.7643%
