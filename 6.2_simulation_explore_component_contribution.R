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
### Document: SIMULATION - EXPLORE THE RESULTS
### Author: trangngpmd
### Date: 2024-10-11


# --------------------------------------------------------------#
# IMPORT SIMULATED DATA -----
# --------------------------------------------------------------#
# scenario <- 0
# 
# fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
# fit_summary <- readRDS(paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
# param_array <- readRDS(paste0(path_simulation,"param_array_scenario_",scenario,".rds"))


# --------------------------------------------------------------#
# CALCULATE COMPONENT CONTRIBUTION FROM EACH FITTED MODEL FROM SIMULATION DATA -----
# Total countries only
# --------------------------------------------------------------#

# ===============================================
# Extract fitted data for each component
for (i in 0:7){
  scenario <- i
  fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
  
  # Create a data frame to store the results
  component_extracted <- data.frame(model=NA, obs=NA, date_week=NA, 
                                    fitted_mean=NA, ar=NA, ne=NA, end=NA, 
                                    ar.exppred.mean=NA, ne.exppred.mean=NA, end.exppred.mean=NA)
  component_extracted$date_week <- as.Date(component_extracted$date_week)
  
  # Calculate the component contribution (use fit_list only)
  for(sim in c(1:length(fit_list))){
    
    fit_sim <- fit_list[[sim]]
    
    if (class(fit_sim)[1]=="hhh4lag"){
      
      # extract fitted mean by component
      model <- hhh4addon:::terms.hhh4lag(fit_sim) #extract the list of control & hhh4_datalist
      meanhhh <- surveillance:::meanHHH(fit_sim$coefficients, model, total.only = FALSE)
      
      fitted_by_component <- data.frame(model = names(fit_list)[sim],
                                        obs = rowSums(fit_sim$stsObj@observed),
                                        date_week = seq(dateweek_start, dateweek_end, by=7),
                                        fitted_mean = c(rep(NA,2),rowSums(meanhhh$mean)), # adding 2 weeks with NA fitted values => total 143 weeks
                                        #fitted_values_check = c(rep(NA,2),rowSums(fitted.values(fit))),
                                        ar = c(rep(NA,2),rowSums(meanhhh$epi.own)),
                                        ne = c(rep(NA,2),rowSums(meanhhh$epi.neighbours)),
                                        end = c(rep(NA,2),rowSums(meanhhh$endemic)),
                                        ar.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ar.exppred)),
                                        ne.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ne.exppred)),
                                        end.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$end.exppred))
      )
      
      component_extracted <- rbind(component_extracted, fitted_by_component)
    }
    else {
      component_extracted <- component_extracted
    }
    
    print(sim)
  }
  
  #unique(component_extracted$model)
  
  saveRDS(component_extracted[2:nrow(component_extracted),], 
          paste0(path_simulation_results,"component_extracted_scenario_",scenario,".rds"))
}



# ===============================================
# Calculate the contribution of each component (percentage)

component_contribution <- NULL

for (i in 0:7){
scenario <- i
component_extracted <- readRDS(paste0(path_simulation_results,"component_extracted_scenario_",scenario,".rds"))

aa <- component_extracted %>%
  filter(!is.na(fitted_mean), !model=="fit_original") %>%
  group_by(model) %>%
  summarise(total_obs = sum(obs, na.rm = T),
            total_fitted_mean = sum(fitted_mean, na.rm = T), 
            endemic = sum(end, na.rm = T),
            autoregressive = sum(ar, na.rm = T),
            neighbourhood = sum(ne, na.rm = T)) %>%
  mutate(percent_fit = total_fitted_mean/total_obs*100,
         percent_end = endemic/total_fitted_mean*100,
         percent_ar = autoregressive/total_fitted_mean*100,
         percent_ne = neighbourhood/total_fitted_mean*100)

#library(summarytools)
aaa <- as.data.frame(descr(aa[,c("percent_ar", "percent_ne", "percent_end")],
                           headings = FALSE, stats = "common", round.digits = 3, transpose = FALSE)) 
aaa <- as.data.frame(t(aaa))
aaa <- aaa %>%
  mutate(scenario = paste0("Scenario_",scenario), 
         characteristics = rownames(aaa))

component_contribution <- rbind(component_contribution, aaa)
}


write_xlsx(component_contribution, paste0(path_simulation_results,"component_contribution.xlsx"))


#--------------------------------------------------------------#
# CALCULATE COMPONENT CONTRIBUTION FROM EACH FITTED MODEL FROM SIMULATION DATA -----
# Each country
#--------------------------------------------------------------#

# ===============================================
# Extract fitted data for each component (in each country)
for (i in 0:7) {
  scenario <- i
  fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
  
  #create array to store the results
  component_bycountry_array <- array(data = NA, dim = c(143,30,8,101), 
                                     dimnames = list(as.character(seq(dateweek_start, dateweek_end, by=7)), 
                                                     EU30_countries, 
                                                     c("obs","fitted_mean","ar", "ne", "end","ar.exppred", "ne.exppred", "end.exppred"), 
                                                     c("fit_original", paste0("sim_",c(1:100)))))
  
  for(sim in c(1:length(fit_list))){
    
    fit_sim <- fit_list[[sim]]
    
    if (class(fit_sim)[1]=="hhh4lag"){
      # extract fitted mean by component
      model <- hhh4addon:::terms.hhh4lag(fit_sim) #extract the list of control & hhh4_datalist
      meanhhh <- surveillance:::meanHHH(fit_sim$coefficients, model, total.only = FALSE)
      
      #fill the array
      component_bycountry_array[,,"obs",sim] <- fit_sim$stsObj@observed
      component_bycountry_array[,,"fitted_mean",sim] <- rbind(NA, NA, meanhhh$mean)
      component_bycountry_array[,,"ar",sim] <- rbind(NA, NA, meanhhh$epi.own)
      component_bycountry_array[,,"ne",sim] <- rbind(NA, NA, meanhhh$epi.neighbours)
      component_bycountry_array[,,"end",sim] <- rbind(NA, NA, meanhhh$endemic)
      
      component_bycountry_array[,,"ar.exppred",sim] <- rbind(NA, NA, meanhhh$ar.exppred)
      component_bycountry_array[,,"ne.exppred",sim] <- rbind(NA, NA, meanhhh$ne.exppred)
      component_bycountry_array[,,"end.exppred",sim] <- rbind(NA, NA, meanhhh$end.exppred)
    }
    else {
      component_bycountry_array <- component_bycountry_array
    }
    
    print(sim)
  }
  
  saveRDS(component_bycountry_array, 
          paste0(path_simulation_results,"component_extracted_bycountry_scenario_",scenario,".rds"))
  
}


########################################################################################
#--------------------------------------------------------------#
# CALCULATE COMPONENT CONTRIBUTION FROM "TRUE" PARAMETERS & SIMULATED DATASETS -----
# Total countries only
#--------------------------------------------------------------#

# ===============================================
# Extract fitted data for each component

# Get data 
for (i in 0:7){
  scenario <- i
  fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
  
  # Create a data frame to store the results
  component_extracted_paramtrue <- data.frame(model=NA, obs=NA, date_week=NA, 
                                              fitted_mean=NA, ar=NA, ne=NA, end=NA,
                                              ar.exppred.mean=NA, ne.exppred.mean=NA, end.exppred.mean=NA)
  component_extracted_paramtrue$date_week <- as.Date(component_extracted_paramtrue$date_week)
  
  
  
  # Calculate the component contribution (use fit_list only)
  for(sim in c(1:length(fit_list))){
    
    fit_original <- fit_list$fit_original
    fit_sim <- fit_list[[sim]]
    
    if (class(fit_sim)[1]=="hhh4lag"){
      
      # Get the parameter set from each scenario:
      param <- as.numeric(point_estimates_change[scenario+1,1:20])
      
      # Extract fitted mean by component
      control_set <- hhh4addon:::setControl(control = fit_sim$control,  # continue using the u_d estimated 
                                            stsObj = fit_sim$stsObj)    # from the model fitted with simulated data 
      
      model <- hhh4addon:::interpretControl(control = control_set, 
                                            stsObj = fit_sim$stsObj)
      
      meanhhh <- surveillance:::meanHHH(theta = param,                  # parameters from the true
                                        model = model,  total.only = FALSE)
      
      fitted_by_component <- data.frame(model = names(fit_list)[sim],
                                        obs = rowSums(fit_sim$stsObj@observed),
                                        date_week = seq(dateweek_start, dateweek_end, by=7),
                                        fitted_mean = c(rep(NA,2),rowSums(meanhhh$mean)), # adding 2 weeks with NA fitted values => total 143 weeks
                                        #fitted_values_check = c(rep(NA,2),rowSums(fitted.values(fit))),
                                        ar = c(rep(NA,2),rowSums(meanhhh$epi.own)),
                                        ne = c(rep(NA,2),rowSums(meanhhh$epi.neighbours)),
                                        end = c(rep(NA,2),rowSums(meanhhh$endemic)),
                                        ar.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ar.exppred)),
                                        ne.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ne.exppred)),
                                        end.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$end.exppred))) 
      
      component_extracted_paramtrue <- rbind(component_extracted_paramtrue, fitted_by_component)
    } else {
      component_extracted_paramtrue <- component_extracted_paramtrue
    }
    
    print(sim)
  }
  
  #unique(component_extracted_paramtrue$model)
  
  saveRDS(component_extracted_paramtrue[2:nrow(component_extracted_paramtrue),], 
          paste0(path_simulation_results,"component_extracted_paramtrue_scenario_",scenario,".rds"))
  
}


# =====================================
# Contribution of each component

component_contribution_paramtrue <- NULL

for (i in 0:7){
scenario <- i
component_extracted_paramtrue <- readRDS(paste0(path_simulation_results,"component_extracted_paramtrue_scenario_",scenario,".rds"))

aa <- component_extracted_paramtrue %>%
  filter(!is.na(fitted_mean), !model=="fit_original") %>% 
  group_by(model) %>%
  summarise(total_obs = sum(obs, na.rm = T),
            total_fitted_mean = sum(fitted_mean, na.rm = T), 
            endemic = sum(end, na.rm = T),
            autoregressive = sum(ar, na.rm = T),
            neighbourhood = sum(ne, na.rm = T)) %>%
  mutate(percent_fit = total_fitted_mean/total_obs*100,
         percent_end = endemic/total_fitted_mean*100,
         percent_ar = autoregressive/total_fitted_mean*100,
         percent_ne = neighbourhood/total_fitted_mean*100)

#library(summarytools)
aaa <- as.data.frame(descr(aa[,c("percent_ar", "percent_ne", "percent_end")],
                           headings = FALSE, stats = "common", round.digits = 3, transpose = FALSE)) 
aaa <- as.data.frame(t(aaa))
aaa <- aaa %>%
  mutate(scenario = paste0("Scenario_",scenario), 
         characteristics = rownames(aaa))

component_contribution_paramtrue <- rbind(component_contribution_paramtrue, aaa)

}

write_xlsx(component_contribution_paramtrue, paste0(path_simulation_results,"component_contribution_paramtrue.xlsx"))


#--------------------------------------------------------------#
# CONTRIBUTION EXTRA -----
#--------------------------------------------------------------#
contribution_extra <- NULL

for (i in 0:7){
scenario <- i
component_extracted <- readRDS(paste0(path_simulation_results,"component_extracted_scenario_",scenario,".rds"))
component_extracted_paramtrue <- readRDS(paste0(path_simulation_results,"component_extracted_paramtrue_scenario_",scenario,".rds"))

aa <- component_extracted %>%
  filter(!is.na(fitted_mean), !model=="fit_original") %>%
  group_by(model) %>%
  summarise(total_obs = sum(obs, na.rm = T),
            total_fitted_mean = sum(fitted_mean, na.rm = T), 
            endemic = sum(end, na.rm = T),
            autoregressive = sum(ar, na.rm = T),
            neighbourhood = sum(ne, na.rm = T)) %>%
  mutate(percent_fit = total_fitted_mean/total_obs*100,
         percent_end = endemic/total_fitted_mean*100,
         percent_ar = autoregressive/total_fitted_mean*100,
         percent_ne = neighbourhood/total_fitted_mean*100, 
         simulation = "simulation")

aa_true <- component_extracted_paramtrue %>%
  filter(!is.na(fitted_mean), !model=="fit_original") %>%
  group_by(model) %>%
  summarise(total_obs = sum(obs, na.rm = T),
            true_total_fitted_mean = sum(fitted_mean, na.rm = T), 
            true_endemic = sum(end, na.rm = T),
            true_autoregressive = sum(ar, na.rm = T),
            true_neighbourhood = sum(ne, na.rm = T)) %>%
  mutate(true_percent_fit = true_total_fitted_mean/total_obs*100,
         true_percent_end = true_endemic/true_total_fitted_mean*100,
         true_percent_ar = true_autoregressive/true_total_fitted_mean*100,
         true_percent_ne = true_neighbourhood/true_total_fitted_mean*100, 
         true = "true")

aaa <- merge(aa, aa_true, by=c("model", "total_obs")) %>%
  select(model, percent_ar, percent_ne, percent_end,
         true_percent_ar, true_percent_ne, true_percent_end) %>%
  mutate(scenario = paste0("Scenario_", scenario),
         diff_ar = percent_ar - true_percent_ar,
         diff_ne = percent_ne - true_percent_ne,
         diff_end = percent_end - true_percent_end) %>%
  mutate(diff2_ar = diff_ar^2,
         diff2_ne = diff_ne^2,
         diff2_end = diff_end^2)

contribution_extra <- rbind(contribution_extra, aaa)
}

write_xlsx(contribution_extra, paste0(path_simulation_results,"contribution_extra.xlsx"))


# Plot contribution extra
aa <- contribution_extra %>%
  select(model, scenario, diff_ar, diff_ne, diff_end, diff2_ar, diff2_ne, diff2_end) %>%
  pivot_longer(cols= c("diff_ar", "diff_ne", "diff_end", "diff2_ar", "diff2_ne", "diff2_end"),
               names_to='diff_diff',
               values_to='estimate') %>%
  arrange(diff_diff)

ggplot(data = aa %>% filter(diff_diff %in% c("diff2_ar", "diff2_ne", "diff2_end")), 
       aes(x=scenario, y=estimate, fill=diff_diff)) +
  geom_boxplot(position=position_dodge(1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  xlab ("") +
  # ylab("absolute difference (unit: percent)") +
  ylab("squared absolute difference") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        legend.title=element_blank(),
        legend.position = "top") +
  scale_fill_manual(values=c("diff2_ar"="#0077b6", 
                             "diff2_end"="#f15bb5", "diff2_ne"="#00f5d4"),
                    labels =c("Autoregressive", "Endemic", "Neighbourhood") )

ggsave(filename = paste0("absolute_differences_2.png"),
       path = paste0(path_plot,"simulation_plot/"), units = "in",
       width = 8,   height = 5, 
       device="png", dpi=300)

#----------------------#
# Contribution extra summary
contribution_extra_summary <- contribution_extra %>%
  group_by(scenario) %>%
  summarize(number_of_fitted_model = n(),
            
            diff_ar_mean = mean(diff_ar),
            diff_ar_sd = sd(diff_ar),
            diff_ar_median = median(diff_ar),
            diff_ar_min = min(diff_ar),
            diff_ar_max = max(diff_ar),
            diff_ar_IQR = IQR(diff_ar),
            diff_ar_lower = quantile(diff_ar,probs = 0.025),
            diff_ar_upper = quantile(diff_ar,probs = 0.975),
            
            diff_ne_mean = mean(diff_ne),
            diff_ne_sd = sd(diff_ne),
            diff_ne_median = median(diff_ne),
            diff_ne_min = min(diff_ne),
            diff_ne_max = max(diff_ne),
            diff_ne_IQR = IQR(diff_ne),
            diff_ne_lower = quantile(diff_ne,probs = 0.025),
            diff_ne_upper = quantile(diff_ne,probs = 0.975),
            
            diff_end_mean = mean(diff_end),
            diff_end_sd = sd(diff_end),
            diff_end_median = median(diff_end),
            diff_end_min = min(diff_end),
            diff_end_max = max(diff_end),
            diff_end_IQR = IQR(diff_end),
            diff_end_lower = quantile(diff_end,probs = 0.025),
            diff_end_upper = quantile(diff_end,probs = 0.975),
            
            diff2_ar_mean = mean(diff2_ar),
            diff2_ar_sd = sd(diff2_ar),
            diff2_ar_median = median(diff2_ar),
            diff2_ar_min = min(diff2_ar),
            diff2_ar_max = max(diff2_ar),
            diff2_ar_IQR = IQR(diff2_ar),
            diff2_ar_lower = quantile(diff2_ar,probs = 0.025),
            diff2_ar_upper = quantile(diff2_ar,probs = 0.975),
            
            diff2_ne_mean = mean(diff2_ne),
            diff2_ne_sd = sd(diff2_ne),
            diff2_ne_median = median(diff2_ne),
            diff2_ne_min = min(diff2_ne),
            diff2_ne_max = max(diff2_ne),
            diff2_ne_IQR = IQR(diff2_ne),
            diff2_ne_lower = quantile(diff2_ne,probs = 0.025),
            diff2_ne_upper = quantile(diff2_ne,probs = 0.975),
            
            diff2_end_mean = mean(diff2_end),
            diff2_end_sd = sd(diff2_end),
            diff2_end_median = median(diff2_end),
            diff2_end_min = min(diff2_end),
            diff2_end_max = max(diff2_end),
            diff2_end_IQR = IQR(diff2_end),
            diff2_end_lower = quantile(diff2_end,probs = 0.025),
            diff2_end_upper = quantile(diff2_end,probs = 0.975)) %>%
  mutate(diff_ar_range_minmax = diff_ar_max - diff_ar_min,
         diff_ne_range_minmax = diff_ne_max - diff_ne_min,
         diff_end_range_minmax = diff_end_max - diff_end_min,
         diff2_ar_range_minmax = diff2_ar_max - diff2_ar_min,
         diff2_ne_range_minmax = diff2_ne_max - diff2_ne_min,
         diff2_end_range_minmax = diff2_end_max - diff2_end_min
  )


aa <- as.data.frame(t(contribution_extra_summary))
aa$name <- row.names(aa)

write_xlsx(aa, paste0(path_simulation_results,"contribution_extra_summary.xlsx"))




#--------------------------------------------------------------#
# CALCULATE COMPONENT CONTRIBUTION FROM TRUE BY COUNTRY -----
#--------------------------------------------------------------#
for (i in 0:7){
  scenario <- i
  fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
  
  #create array to store the results
  component_bycountry_array <- array(data = NA, dim = c(143,30,8,101), 
                                     dimnames = list(as.character(seq(dateweek_start, dateweek_end, by=7)), 
                                                     EU30_countries, 
                                                     c("obs","fitted_mean","ar", "ne", "end","ar.exppred", "ne.exppred", "end.exppred"), 
                                                     c("fit_original", paste0("sim_",c(1:100)))))
  
  for(sim in c(1:length(fit_list))){
    
    fit_original <- fit_list$fit_original
    fit_sim <- fit_list[[sim]]
    
    y.start <- observed(fit_original$stsObj)[c(1:2), ]
    
    if (class(fit_sim)[1]=="hhh4lag"){
      
      #get the parameter set from each scenario:
      param <- as.numeric(point_estimates_change[scenario+1,1:20])
      
      # extract fitted mean by component
      control_set <- hhh4addon:::setControl(control = fit_sim$control, 
                                            stsObj = fit_sim$stsObj)
      
      model <- hhh4addon:::interpretControl(control = control_set, 
                                            stsObj = fit_sim$stsObj)
      
      meanhhh <- surveillance:::meanHHH(theta = param, 
                                        model = model,  total.only = FALSE)
      
      
      component_bycountry_array[,,"obs",sim] <- fit_sim$stsObj@observed
      component_bycountry_array[,,"fitted_mean",sim] <- rbind(NA, NA, meanhhh$mean)
      component_bycountry_array[,,"ar",sim] <- rbind(NA, NA, meanhhh$epi.own)
      component_bycountry_array[,,"ne",sim] <- rbind(NA, NA, meanhhh$epi.neighbours)
      component_bycountry_array[,,"end",sim] <- rbind(NA, NA, meanhhh$endemic)
      
      component_bycountry_array[,,"ar.exppred",sim] <- rbind(NA, NA, meanhhh$ar.exppred)
      component_bycountry_array[,,"ne.exppred",sim] <- rbind(NA, NA, meanhhh$ne.exppred)
      component_bycountry_array[,,"end.exppred",sim] <- rbind(NA, NA, meanhhh$end.exppred)
    }  else {
      component_bycountry_array <- component_bycountry_array
    }
    
    print(sim)
    
    saveRDS(component_bycountry_array, 
            paste0(path_simulation_results,"component_extracted_bycountry_paramtrue_scenario_",scenario,".rds"))
  }}




