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
### Document: PLOT SIMULATION EXTRA FIGURES - PERCENTAGE SCENARIOS VS. TRUE
### Author: trangngpmd
### Date: 2024-05-28



## Import data
scenario <- 0

# fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
# fit_summary <- readRDS(paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
# param_array <- readRDS(paste0(path_simulation,"param_array_scenario_",scenario,".rds"))

# Import data
component_extracted <- readRDS(paste0(path_simulation_results,"component_extracted_scenario_",scenario,".rds"))
component_extracted_paramtrue <- readRDS(paste0(path_simulation_results,"component_extracted_paramtrue_scenario_",scenario,".rds"))
component_bycountry_array <- readRDS(paste0(path_simulation_results,"component_extracted_bycountry_scenario_",scenario,".rds"))
component_bycountry_array_true <- readRDS(paste0(path_simulation_results,"component_extracted_bycountry_paramtrue_scenario_",scenario,".rds"))


# --------------------------------------------------------------------#
# ======= PLOT =================
# --------------------------------------------------------------------#

# 100 simulated datasets
aa <- component_extracted %>%  filter(!model=="fit_original") %>%
  select(date_week, model, obs) %>%
  group_by(date_week) %>%
  mutate(median = median(obs),
         upper = quantile(obs,probs = 0.975),
         lower = quantile(obs,probs = 0.025)) %>% ungroup()

plot1 <- ggplot() +
  geom_line(data=aa,aes(x=date_week, y=obs,group=model, color="simulation"), linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median, group=model, color="simulation"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper, group=model, color="simulation"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower, group=model,color="simulation"), linewidth=0.6, linetype = "dashed") +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle(paste0("Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold')) +
  theme(legend.position = "none") +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,10000000)) +
  scale_color_manual(name="",
                     values=c("simulation"="red"),
                     labels = c("Simulated data"))
# plot1

# Percentage (all countries)
aa <- component_extracted %>% filter(!model=="fit_original") %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100)  %>%
  group_by(date_week) %>%
  mutate(median_ar = median(percent_ar, na.rm = T),
         upper_ar = quantile(percent_ar,probs = 0.975,na.rm = T),
         lower_ar = quantile(percent_ar,probs = 0.025,na.rm = T),
         median_ne = median(percent_ne,na.rm = T),
         upper_ne = quantile(percent_ne,probs = 0.975,na.rm = T),
         lower_ne = quantile(percent_ne,probs = 0.025,na.rm = T),
         median_end = median(percent_end,na.rm = T),
         upper_end = quantile(percent_end,probs = 0.975,na.rm = T),
         lower_end = quantile(percent_end,probs = 0.025,na.rm = T)) %>% ungroup()


plot2 <- ggplot() +
  #Autoregressive
  geom_line(data=aa, aes(x=date_week, y=percent_ar, group=model), color="#0077b6", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ar, group=model, color="Sim_AR"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") +
  #Endemic
  geom_line(data=aa, aes(x=date_week, y=percent_end, group=model), color="#00f5d4", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_end, group=model, color="Sim_END"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") +
  #Neighbourhood
  geom_line(data=aa, aes(x=date_week, y=percent_ne, group=model),color="#f15bb5", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ne, group=model, color="Sim_NE"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") +
  
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("Contribution (%) from fitted models") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position=c(0.2,0.55)) + 
  #theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  scale_color_manual(name="",
                     breaks=c("Sim_AR", "Sim_END", "Sim_NE"),
                     values=c("Sim_AR"="#0077b6", 
                              "Sim_END"="#00f5d4", 
                              "Sim_NE"="#f15bb5"),
                     labels = c("Autoregressive",
                                "Endemic", 
                                "Neighbourhood"))
# plot2

# Percentage FROM TRUE (all countries)
aa <- component_extracted_paramtrue %>% filter(!model=="fit_original") %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100)  %>%
  group_by(date_week) %>%
  mutate(median_ar = median(percent_ar, na.rm = T),
         upper_ar = quantile(percent_ar,probs = 0.975,na.rm = T),
         lower_ar = quantile(percent_ar,probs = 0.025,na.rm = T),
         median_ne = median(percent_ne,na.rm = T),
         upper_ne = quantile(percent_ne,probs = 0.975,na.rm = T),
         lower_ne = quantile(percent_ne,probs = 0.025,na.rm = T),
         median_end = median(percent_end,na.rm = T),
         upper_end = quantile(percent_end,probs = 0.975,na.rm = T),
         lower_end = quantile(percent_end,probs = 0.025,na.rm = T)) %>% ungroup()


plot3 <- ggplot() +
  #Autoregressive
  geom_line(data=aa, aes(x=date_week, y=percent_ar, group=model), color="#0077b6", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ar, group=model, color="Sim_AR"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") +
  #Endemic
  geom_line(data=aa, aes(x=date_week, y=percent_end, group=model), color="#00f5d4", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_end, group=model, color="Sim_END"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") +
  #Neighbourhood
  geom_line(data=aa, aes(x=date_week, y=percent_ne, group=model),color="#f15bb5", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ne, group=model, color="Sim_NE"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") +
  
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("Contribution (%) from the 'true'") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position=c(0.2,0.55)) +
  # theme(legend.position="none") +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  scale_color_manual(name="",
                     breaks=c("Sim_AR", "Sim_END", "Sim_NE"),
                     values=c("Sim_AR"="#0077b6", 
                              "Sim_END"="#00f5d4", 
                              "Sim_NE"="#f15bb5"),
                     labels = c("Autoregressive",
                                "Endemic", 
                                "Neighbourhood"))
# plot3

## combine plots
#library("cowplot")
plotcombine_0 <- plot_grid(plot1, plot2, plot3, ncol = 3, align = "h", axis = "l")
# plotcombine_3


plotcombine <- plot_grid(plotcombine_0, plotcombine_1, plotcombine_2,
                         plotcombine_3, plotcombine_4, plotcombine_5,
                         plotcombine_6, plotcombine_7,
                         ncol = 1, align = "h", axis = "l")


ggsave(file="combine_simulation.png", plotcombine,
       path = paste0(path_plot,"simulation_plot/"),
       height=20, width=15, units="in")
