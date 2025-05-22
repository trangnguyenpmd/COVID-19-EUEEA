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
# EXTRACT PARAMETER POINT ESTIMATES-----
# --------------------------------------------------------------#
# ======= Extract parameters from the array ===============
param_df_long <- NULL

for (i in 0:7){
  df <- readRDS(paste0(path_simulation,"param_array_scenario_",i,".rds")) %>%
    as.data.frame.table() %>%
    rename(param_name = Var1, estimate_type = Var2,
           model = Var3, estimate = Freq) %>%
    mutate(scenario = paste0("Scenario_", i))
  param_df_long <- rbind(param_df_long, df)
}

unique(param_df_long$scenario)

saveRDS(param_df_long, 
        paste0(path_simulation_results,"param_df_long_fitted_simulated_data.rds"))



# ======= Summary of the point estimates =============================
point_diff <- as.data.frame(point_estimates_change) %>%
  mutate(scenario = rownames(point_estimates_change)) %>%
  pivot_longer(cols = ar.1:overdisp, 
               names_to = "param_name",
               values_to = "true_estimate") %>%
  right_join(param_df_long %>% filter(estimate_type=="coefs"), 
             by=c("param_name", "scenario")) %>%
  mutate(diff_estimate = true_estimate - estimate) %>%
  mutate(diff_square = diff_estimate^2)


point_summary <- point_diff %>%
  filter(!model == "fit_original", #this is fit_original 
         estimate_type == "coefs", !is.na(estimate)) %>%
  group_by(scenario, param_name) %>%
  summarize(number_of_fitted_model = n(),
            true_estimate = true_estimate[1],
            point_median = median(estimate),
            point_mean = mean(estimate),
            point_sd = sd(estimate),
            
            diff_median = median(diff_estimate),
            diff_mean = mean(diff_estimate),
            diff_sd = sd(diff_estimate),
            
            diff2_median = median(diff_square),
            diff2_mean = mean(diff_square),
            diff2_sd = sd(diff_square),
            
            point_min = min(estimate), 
            point_max = max(estimate), 
            point_IQR = IQR(estimate),
            
            lower = quantile(estimate,probs = 0.025),
            upper = quantile(estimate,probs = 0.975)) %>%
  mutate(range_minmax = point_max - point_min)

write_xlsx(point_summary, paste0(path_simulation_results,"point_estimates_summary.xlsx"))


# ======== Plot point estimates in boxplots ======================
# for (i in 1:20){
param_no <- 2  #range from 1 to 20 (20 parameters in total)
param <- colnames(point_estimates_change)[param_no]

aa <- point_diff %>% filter(!model == "fit_original", estimate_type == "coefs", 
                            !is.na(estimate), param_name == param)
# summary(aa$estimate)

ggplot(data = aa, aes(y=scenario, x=estimate)) +
  geom_boxplot() +
  geom_point(aes(y=scenario, x=true_estimate), color = 'red', size=0.8) +
  #geom_point(aes(y=scenario, x=exp(true_estimate)), color = 'red', size=1.5) + #for decay d
  # labs(x=paste0("Point estimate of ",param), y="Simulate from") +
  labs(x=paste0("(Zoomed) Point estimate of ",param), y="Simulate from") +
  theme_bw()  +  
  coord_cartesian(xlim=c(-1,1))

ggsave(#filename = paste0("point_estimates_nozoom_", param,".png"),
  filename = paste0("point_estimates_", param,".png"),
  # path = paste0(path_plot,"simulation_plot/"), units = "in",
  # width = 7,   height = 6,
  path = paste0(path_plot,"parameters/"), units = "in",
  width = 4.5,   height = 3,
  device="png", dpi=600)
# }


# --------------------------------------------------------------#
# EXTRACT CONFIDENCE INTERVALS (95%CIs) -----
# --------------------------------------------------------------#
# ===== Plot CIs =========
# Set up
for (i in 3:7){
  for(j in 1:20){
  param_no <- 13 #from 1 to 20
  scenario_no <- 0 #from 0 to 7 
  
  param <- colnames(point_estimates_change)[param_no]
  scenario <- rownames(point_estimates_change)[scenario_no+1]
  yintercept_plot <- point_estimates_change[scenario_no+1, param_no] #the "true" point estimates
  
  # Create a dummy data 
  aa <- param_df_long %>% 
    filter(!model == "fit_original", !is.na(estimate), param_name == param) %>%
    pivot_wider(names_from = estimate_type, 
                values_from = estimate) %>%
    rename(lower = `2.5 %`,
           upper = `97.5 %`)
  
  # Plot
  ggplot(data = aa %>% filter(scenario == paste0("Scenario_", scenario_no)), 
         aes(x=reorder(model, coefs), y=coefs)) +    
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    facet_wrap(~scenario, scales = "free", nrow=1, ncol=1) + 
    geom_hline(aes(yintercept = yintercept_plot), linewidth = 0.5, linetype = "dashed", color="red") +
    labs(x="", y= param) + 
    theme_bw() + 
   # coord_flip()
     coord_flip(ylim=c(-100,50))
  
  ggsave(filename = paste0("CIs_estimates_", param, "_" ,scenario,".png"),
         path = paste0(path_plot,"simulation_plot/"), units = "in",
         width = 5.5,   height = 8, 
         device="png", dpi=300)
  }
}


# =================================================================================
# Count how many fitted models that their param CIs contain the point estimates

param_withinCI <- as.data.frame(point_estimates_change) %>%
  mutate(scenario = rownames(point_estimates_change)) %>%
  pivot_longer(cols = ar.1:overdisp, 
               names_to = "param_name",
               values_to = "true_estimate") %>%
  right_join(param_df_long , 
             by=c("param_name", "scenario")) %>%
  filter(!model == "fit_original", !is.na(estimate)) %>%
  pivot_wider(names_from = estimate_type, 
              values_from = estimate) %>%
  rename(lower = `2.5 %`,
         upper = `97.5 %`) %>%
  mutate(withinCI_check = ifelse(true_estimate>=lower & true_estimate<=upper, TRUE, FALSE),
         CIwidth = upper - lower)



# ======== Count ==============
aa <- as.data.frame(table(param_withinCI$param_name, param_withinCI$scenario, 
                          param_withinCI$withinCI_check)) %>%
  rename(param_name = Var1, 
         scenario = Var2,
         withinCI_check = Var3,
         number_model = Freq)

CI_count <- merge(aa, point_summary[,c("param_name", "scenario", "number_of_fitted_model")], 
                  by=c("param_name", "scenario"),
                  all.x=TRUE)
CI_count$percent <- CI_count$number_model/CI_count$number_of_fitted_model

saveRDS(CI_count, paste0(path_simulation_results,"count_withinCI.rds"))


# ============= CIwidth summary ==================

CI_width <- param_withinCI %>%
  # filter(withinCI_check == FALSE) %>%
  filter(withinCI_check == TRUE) %>%
  group_by(param_name, scenario) %>%
  summarize(count = n(),
            CIwidth_mean = mean(CIwidth),
            CIwidth_sd = sd(CIwidth),
            CIwidth_median = median(CIwidth),
            CIwidth_min = min(CIwidth), 
            CIwidth_max = max(CIwidth), 
            CI_IQR = IQR(CIwidth)) %>%
  mutate(range_minmax = CIwidth_max - CIwidth_min)

write_xlsx(CI_width, paste0(path_simulation_results,"CI_width_TRUE.xlsx"))



# ============= PARLAG summary ==================

parlag_df <- NULL

for (i in 0:7){
  fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",i,".rds"))
  aa <- data.frame(model=c("fit_original", paste0("sim_",c(1:100))),
                   parlag = NA, lag1 = NA, lag2 = NA, scenario=NA)
  for (sim in 1:length(fit_list)) {
    fit_sim <- fit_list[[sim]]
    aa$parlag[sim] <- fit_sim$par_lag
    aa$lag1[sim] <- fit_sim$distr_lag[1]
    aa$lag2[sim] <- fit_sim$distr_lag[2]
    aa$scenario[sim] <- paste0("Scenario_", i)
  }
  parlag_df <- rbind(parlag_df,aa)
}

unique(parlag_df$scenario)


# Plot parlag
ggplot() +
  geom_boxplot(data = parlag_df %>% filter(!model=="fit_original", !is.na(scenario)), 
               aes(y=lag2, x=scenario)) +
  geom_point(data = parlag_df %>% filter(model=="fit_original"), 
             aes(y=lag2, x=scenario), color = 'red', size=1.5) +
  labs(x="Simulate from", y="Lag 2 estimate") +
  theme_bw()   +  
  coord_flip()

ggsave(filename = paste0("parlag_lag2_estimate.png"),
       path = paste0(path_plot,"simulation_plot/"), units = "in",
       width = 7,   height = 6, 
       device="png", dpi=300)




